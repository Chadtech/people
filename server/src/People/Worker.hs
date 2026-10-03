{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module People.Worker (runWorker, runCycle) where

import qualified GenerationError
import People.DomainInstances ()
import qualified PromptSnapshot

import qualified Acadia.Records as Records
import Acadia.Transaction (Transaction)
import qualified Chat
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception
    ( SomeAsyncException
    , SomeException
    , catch
    , displayException
    , fromException
    , throwIO
    , try
    )
import Control.Monad (forM_, forever, unless, void)
import Conversation (Conversation)
import qualified Conversation
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as Lazy
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find, sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import Generation (Generation)
import qualified Generation
import qualified Goal
import qualified GoalId
import qualified Memory
import qualified MemoryId
import qualified People.Database as Database
import qualified People.OpenAI as OpenAI
import qualified People.Prompt as Prompt
import qualified People.SelectionReason as SelectionReason
import Snapshot (Snapshot, Sources (..))
import Person (Person)
import qualified Person
import qualified PersonId
import System.Timeout (timeout)


runWorker :: Database.Database -> OpenAI.Client -> IO ()
runWorker database client =
    forever $ do
        runCycle database (OpenAI.requestValue client) (OpenAI.generate client)
        threadDelay 1000000


-- One poll is independently executable so lifecycle tests can supply a
-- deterministic model response while exercising the actual database worker.
runCycle
    :: Database.Database
    -> (Prompt.Prompt -> [Value] -> Value)
    -> (Prompt.Prompt -> [Value] -> IO OpenAI.Outcome)
    -> IO ()
runCycle database requestValue generate =
    handleFailure $ do
        expired <- run Generation.getExpiredGenerations
        forM_ expired (\g -> handleFailure (run (Chat.expireGeneration g.id)))
        scheduled <- run Conversation.getScheduledConversations
        forM_ scheduled schedule
        pending <- run Generation.getPendingGenerations
        forM_ (sortOn (.id) pending) process
    where
        run :: Transaction a -> IO a
        run =
            Database.runTransaction database
        schedule :: Conversation -> IO ()
        schedule conversation =
            handleFailure $ do
                members <- run (Conversation.getParticipants conversation.id)
                people <- run Person.getAllPersons
                let
                    participants :: [PersonId.PersonId]
                    participants =
                        sortOn
                            personNumber
                            ( map
                                (.id)
                                (filter (\person -> person.id `elem` members && isAiPerson person) people)
                            )
                case participants of
                    [] ->
                        run (Chat.stopConversation conversation.id)

                    firstParticipant : _ -> do
                        let
                            later :: [PersonId.PersonId]
                            later =
                                case conversation.lastSpeaker of
                                    Nothing ->
                                        participants

                                    Just previous ->
                                        filter
                                            ( \p ->
                                                personNumber p > personNumber previous
                                            )
                                            participants
                            speaker :: PersonId.PersonId
                            speaker =
                                case later of
                                    next : _ -> next

                                    [] ->
                                        firstParticipant
                        void (run (Chat.requestTurn conversation.id conversation.revision speaker))
        process :: Generation -> IO ()
        process generation =
            do
                claimed <-
                    try (run (Generation.claimGeneration generation.id 180))
                        :: IO (Either SomeException ())
                case claimed of
                    Left err ->
                        rethrowAsync err

                    Right () ->
                        handleGenerationFailure generation $ do
                            people <- run Person.getAllPersons
                            participants <- run (Conversation.getParticipants generation.conversationId)
                            let
                                roster :: [Person]
                                roster =
                                    filter
                                        (\p -> any (\member -> personNumber p.id == personNumber member) participants)
                                        people
                            person <-
                                maybe
                                    (ioError (userError "The selected person is missing."))
                                    pure
                                    (find (\p -> personNumber p.id == personNumber generation.speaker) roster)
                            conversations <- run Conversation.getConversations
                            conversation <-
                                maybe
                                    (ioError (userError "The conversation is missing."))
                                    pure
                                    (find (\c -> c.id == generation.conversationId) conversations)
                            messages <- run (Chat.getMessages conversation.id)
                            goals <- run (Goal.getGoals person.id)
                            memories <- run (Memory.getMemories person.id)
                            prompt <-
                                either
                                    (ioError . userError . Prompt.errorToString)
                                    pure
                                    (Prompt.buildPrompt person roster messages goals memories)
                            result <-
                                race
                                    (timeout 175000000 (runTurn generation prompt))
                                    (waitForCancellation generation)
                            case result of
                                Right () ->
                                    putStrLn "Generation stopped."

                                Left Nothing ->
                                    ioError
                                        ( userError
                                            "The turn exceeded its time limit. Earlier tool actions may remain saved."
                                        )

                                Left (Just ()) ->
                                    putStrLn ("Completed generation " ++ show generation.id)
        runTurn :: Generation -> Prompt.Prompt -> IO ()
        runTurn generation prompt =
            do
                requests <- newIORef ([] :: [Value])
                let
                    save history =
                        do
                            sent <- readIORef requests
                            let
                                snapshot =
                                    object
                                        [ "request" .= requestValue prompt []
                                        , "requests" .= sent
                                        , "transcript" .= history
                                        , "selection" .= Prompt.selection prompt
                                        ]
                            run
                                ( Generation.savePrompt
                                    generation.id
                                    (PromptSnapshot.Saved (promptSnapshot prompt snapshot))
                                )
                    loop :: Int -> [Value] -> [(OpenAI.ToolCall, Value)] -> IO ()
                    loop count history previous =
                        do
                            modifyIORef' requests (++ [requestValue prompt history])
                            save history
                            outcome <- generate prompt history
                            let
                                withOutput =
                                    history ++ outcome.outputItems
                            save withOutput
                            if null outcome.toolCalls
                                then run (Chat.finishGeneration generation.id outcome.reply)
                                else do
                                    unless
                                        (count + length outcome.toolCalls <= 8)
                                        ( ioError
                                            (userError "Tool-call limit reached. Earlier tool actions remain saved.")
                                        )
                                    (continued, recorded) <- applyCalls withOutput previous outcome.toolCalls
                                    loop (count + length outcome.toolCalls) continued recorded
                    applyCalls history previous [] =
                        pure (history, previous)
                    applyCalls history previous (call : rest) =
                        do
                            result <- case find (\(old, _) -> old.callId == call.callId) previous of
                                Just (old, cached) | old == call -> pure cached

                                Just _ ->
                                    ioError (userError "A tool call ID was reused with different arguments.")

                                Nothing ->
                                    do
                                        value <- executeTool generation call
                                        pure
                                            ( object
                                                [ "type" .= ("function_call_output" :: T.Text)
                                                , "call_id" .= call.callId
                                                , "output" .= Text.decodeUtf8 (Lazy.toStrict (encode value))
                                                ]
                                            )
                            let
                                continued =
                                    history ++ [result]
                            save continued
                            applyCalls continued ((call, result) : previous) rest
                loop 0 [] []
        executeTool :: Generation -> OpenAI.ToolCall -> IO Value
        executeTool generation call =
            case OpenAI.parseAction call of
                Left problem ->
                    pure (toolError (OpenAI.actionErrorText problem))

                Right action ->
                    case action of
                        OpenAI.CreateGoal description ->
                            do
                                GoalId.GoalId identifier <-
                                    run (Chat.createGenerationGoal generation.id description)
                                pure (success "goal_id" identifier)

                        OpenAI.SaveMemory content ->
                            do
                                MemoryId.MemoryId identifier <-
                                    run (Chat.saveGenerationMemory generation.id content)
                                pure (success "memory_id" identifier)

                        OpenAI.CompleteGoal goalId@(GoalId.GoalId identifier) ->
                            do
                                goals <- run (Goal.getGoals generation.speaker)
                                if any (\g -> g.id == goalId && isActiveGoal g.status) goals
                                    then do
                                        run (Chat.completeGenerationGoal generation.id goalId)
                                        pure (success "goal_id" identifier)
                                    else pure (toolError "That goal is not one of your active goals.")
        success key identifier =
            object
                ["ok" .= True, key .= T.pack (show (identifier :: Word64))]
        toolError message =
            object ["ok" .= False, "error" .= (message :: T.Text)]
        isActiveGoal Goal.Active =
            True
        isActiveGoal _ =
            False
        waitForCancellation :: Generation -> IO ()
        waitForCancellation generation =
            do
                threadDelay 500000
                phase <- run (Generation.getGenerationPhase generation.id)
                case phase of
                    Just Generation.Running ->
                        waitForCancellation generation

                    _ ->
                        pure ()
        handleGenerationFailure :: Generation -> IO () -> IO ()
        handleGenerationFailure generation operation =
            operation `catch` \(err :: SomeException) -> do
                rethrowAsync err
                -- OpenAI errors have already been sanitized; database errors omit bodies.
                let
                    message :: T.Text
                    message =
                        T.take 400 (T.pack (displayException err))
                            <> " Earlier tool actions may remain saved; inspect this turn before retrying."
                handleFailure
                    ( run
                        (Chat.failGeneration generation.id (GenerationError.GenerationError message))
                    )
                putStrLn
                    ("Generation " ++ show generation.id ++ " failed; inspect it in the app.")


personNumber :: PersonId.PersonId -> Word64
personNumber (PersonId.PersonId value) =
    value


rethrowAsync :: SomeException -> IO ()
rethrowAsync err =
    case fromException err :: Maybe SomeAsyncException of
        Just _ ->
            throwIO err

        Nothing ->
            pure ()


handleFailure :: IO () -> IO ()
handleFailure operation =
    operation `catch` \(err :: SomeException) -> do
        rethrowAsync err
        putStrLn "Database operation failed; the worker will check again."


isAiPerson :: Person -> Bool
isAiPerson person =
    case person.kind of
        Person.AiPerson _ ->
            True

        Person.HumanPerson ->
            False


promptSnapshot :: Prompt.Prompt -> Value -> Snapshot
promptSnapshot prompt evidence =
    let
        sources :: [Prompt.SelectionBlock] -> Sources
        sources blocks =
            foldr
                (\block rest ->
                    SourceItem
                        ( Records.Record3
                            block.blockSource
                            (SelectionReason.toText block.blockSelectionReason)
                            block.blockText
                        )
                        rest
                )
                NoSources
                blocks
    in
    Records.Record5
        (sources prompt.selection.includedBlocks)
        (sources prompt.selection.omittedBlocks)
        (fromIntegral prompt.selection.contextBudget)
        (Just prompt.instructions)
        (Text.decodeUtf8 (Lazy.toStrict (encode evidence)))
