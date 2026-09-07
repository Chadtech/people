{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module People.Worker (runWorker, runCycle) where

import qualified GenerationError
import People.DomainInstances ()
import qualified PromptSnapshot

import Acadia.Transaction (Transaction)
import qualified Chat
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (
    AsyncException,
    SomeException,
    catch,
    displayException,
    fromException,
    throwIO,
    try,
 )
import Control.Monad (forM_, forever, void)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as Lazy
import Data.List (find, sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import qualified Goal
import qualified Memory
import qualified People.Database as Database
import qualified People.OpenAI as OpenAI
import qualified People.Prompt as Prompt
import qualified Person
import qualified PersonId


runWorker :: Database.Database -> OpenAI.Client -> IO ()
runWorker database client = forever $ do
    runCycle database (OpenAI.requestValue client) (OpenAI.generate client)
    threadDelay 1000000


-- One poll is independently executable so lifecycle tests can supply a
-- deterministic model response while exercising the actual database worker.
runCycle
    :: Database.Database
    -> (Prompt.Prompt -> Value)
    -> (Prompt.Prompt -> IO OpenAI.Outcome)
    -> IO ()
runCycle database requestValue generate =
    handleFailure $ do
        expired <- run Chat.getExpiredGenerations
        forM_ expired (\g -> handleFailure (run (Chat.expireGeneration g.id)))
        scheduled <- run Chat.getScheduledConversations
        forM_ scheduled schedule
        pending <- run Chat.getPendingGenerations
        forM_ (sortOn (.id) pending) process
    where
        run :: Transaction a -> IO a
        run = Database.runTransaction database
        schedule :: Chat.Conversation -> IO ()
        schedule conversation = handleFailure $ do
            participants <-
                sortOn personNumber <$> run (Chat.getParticipants conversation.id)
            case participants of
                [] -> run (Chat.stopConversation conversation.id)
                firstParticipant : _ -> do
                    let
                        later :: [PersonId.PersonId]
                        later = case conversation.lastSpeaker of
                            Nothing -> participants
                            Just previous -> filter (\p -> personNumber p > personNumber previous) participants
                        speaker :: PersonId.PersonId
                        speaker = case later of
                            next : _ -> next
                            [] -> firstParticipant
                    void (run (Chat.requestTurn conversation.id conversation.revision speaker ""))
        process :: Chat.Generation -> IO ()
        process generation = do
            claimed <-
                try (run (Chat.claimGeneration generation.id 180))
                    :: IO (Either SomeException ())
            case claimed of
                Left err -> rethrowAsync err
                Right () -> handleGenerationFailure generation $ do
                    people <- run Person.getAllPersons
                    participants <- run (Chat.getParticipants generation.conversationId)
                    let
                        roster :: [Person.Person]
                        roster =
                            filter
                                (\p -> any (\member -> personNumber p.id == personNumber member) participants)
                                people
                    person <-
                        maybe
                            (ioError (userError "The selected person is missing."))
                            pure
                            (find (\p -> personNumber p.id == personNumber generation.speaker) roster)
                    conversations <- run Chat.getConversations
                    conversation <-
                        maybe
                            (ioError (userError "The conversation is missing."))
                            pure
                            (find (\c -> c.id == generation.conversationId) conversations)
                    messages <- run (Chat.getMessages conversation.id)
                    goals <- run (Goal.getGoals person.id)
                    memories <- run (Memory.getMemories person.id)
                    if T.null (T.strip person.identity)
                        then
                            ioError (userError "Add an identity for this person before generating a reply.")
                        else pure ()
                    prompt <-
                        either
                            (ioError . userError)
                            pure
                            (Prompt.buildPrompt person roster conversation messages goals memories)
                    let
                        snapshot :: Value
                        snapshot =
                            object
                                ["request" .= requestValue prompt, "selection" .= Prompt.selection prompt]
                    run
                        ( Chat.savePrompt
                            generation.id
                            ( PromptSnapshot.PromptSnapshot
                                (Text.decodeUtf8 (Lazy.toStrict (encode snapshot)))
                            )
                        )
                    result <- race (generate prompt) (waitForCancellation generation)
                    case result of
                        Right () -> putStrLn "Generation stopped."
                        Left outcome -> do
                            run
                                ( Chat.finishGeneration
                                    generation.id
                                    outcome.reply
                                    outcome.newGoal
                                    outcome.reflection
                                    outcome.completeGoal
                                    outcome.sharedNote
                                )
                            putStrLn ("Completed generation " ++ show generation.id)
        waitForCancellation :: Chat.Generation -> IO ()
        waitForCancellation generation = do
            threadDelay 500000
            phase <- run (Chat.getGenerationPhase generation.id)
            case phase of
                Just Chat.Running -> waitForCancellation generation
                _ -> pure ()
        handleGenerationFailure :: Chat.Generation -> IO () -> IO ()
        handleGenerationFailure generation operation =
            operation `catch` \(err :: SomeException) -> do
                rethrowAsync err
                -- OpenAI errors have already been sanitized; database errors omit bodies.
                let
                    message :: T.Text
                    message = T.take 500 (T.pack (displayException err))
                handleFailure
                    ( run
                        (Chat.failGeneration generation.id (GenerationError.GenerationError message))
                    )
                putStrLn
                    ("Generation " ++ show generation.id ++ " failed; inspect it in the app.")


personNumber :: PersonId.PersonId -> Word64
personNumber (PersonId.PersonId value) = value


rethrowAsync :: SomeException -> IO ()
rethrowAsync err = case fromException err :: Maybe AsyncException of
    Just _ -> throwIO err
    Nothing -> pure ()


handleFailure :: IO () -> IO ()
handleFailure operation =
    operation `catch` \(err :: SomeException) -> do
        rethrowAsync err
        putStrLn "Database operation failed; the worker will check again."
