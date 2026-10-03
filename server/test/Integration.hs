{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AiPersonProfile (AiPersonProfile)
import qualified LocalAccount
import qualified MemoryContent
import qualified MessageContent
import People.DomainInstances ()
import qualified PromptSnapshot
import qualified Revision

import Chat (ConversationPageFlag, Message)
import qualified Chat
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Conversation (Conversation)
import qualified Conversation
import qualified ConversationId
import Data.Aeson (ToJSON, Value, eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Types (parseMaybe, withObject, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.Either (isLeft)
import Data.IORef
import Data.List (sort)
import Data.Maybe (isNothing)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import qualified Fixtures
import Generation (GenerationSummary)
import qualified Generation
import qualified GenerationId
import qualified Goal
import qualified GoalId
import qualified Memory
import qualified MessageId
import qualified Origin
import qualified People.Database as Database
import qualified People.OpenAI as OpenAI
import qualified People.Prompt as Prompt
import qualified People.Worker as Worker
import Person (Person)
import qualified Person
import PersonId (PersonId)
import qualified PersonId
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Timeout (timeout)


assert :: Bool -> String -> IO ()
assert condition message =
    unless condition (ioError (userError message))


mustFail :: IO a -> IO ()
mustFail operation =
    do
        result <- try (operation >> pure ()) :: IO (Either SomeException ())
        case result of
            Left _ ->
                pure ()

            Right _ ->
                ioError (userError "Expected transaction rejection")


requireAiPersonProfile :: Person -> IO AiPersonProfile
requireAiPersonProfile person =
    case person.kind of
        Person.AiPerson profile ->
            pure profile

        Person.HumanPerson ->
            ioError (userError "Expected an AI test person")


profileFields :: Person -> Maybe (Text, Text)
profileFields person =
    case person.kind of
        Person.AiPerson profile ->
            Just (profile.identity, profile.aspirations)

        Person.HumanPerson ->
            Nothing


checkConversationPageFlags :: Database.Database -> IO ()
checkConversationPageFlags database =
    do
        flags <-
            Database.runTransaction database Conversation.getAllConversationsPageFlags
        conversations <- Database.runTransaction database Conversation.getConversations
        people <- Database.runTransaction database Person.getAllPersons
        let
            conversationIds :: [ConversationId.ConversationId]
            conversationIds =
                concatMap
                    ( \flag -> case flag of
                        Conversation.ConversationFlag conversation ->
                            [conversation.id]

                        Conversation.PersonFlag _ ->
                            []
                    )
                    flags
            personIds :: [PersonId.PersonId]
            personIds =
                concatMap
                    ( \flag -> case flag of
                        Conversation.ConversationFlag _ ->
                            []

                        Conversation.PersonFlag person ->
                            [person.id]
                    )
                    flags
        assert
            ( sort conversationIds
                == sort (map (\conversation -> conversation.id) conversations)
            )
            "Page flags preserve every conversation exactly once"
        assert
            (sort personIds == sort (map (\person -> person.id) people))
            "Page flags preserve every person exactly once"


checkConversationRefresh
    :: Database.Database -> ConversationId.ConversationId -> IO ()
checkConversationRefresh database conversationId =
    do
        let
            run :: Database.Transaction a -> IO a
            run =
                Database.runTransaction database
            conversationRow :: ConversationPageFlag -> Maybe Conversation
            conversationRow flag =
                case flag of
                    Chat.ConversationFlag row ->
                        Just row

                    _ ->
                        Nothing
            currentPersonRow :: ConversationPageFlag -> Maybe PersonId
            currentPersonRow flag =
                case flag of
                    Chat.CurrentPersonFlag personId ->
                        Just personId

                    _ ->
                        Nothing
            personRow :: ConversationPageFlag -> Maybe Person
            personRow flag =
                case flag of
                    Chat.PersonFlag row ->
                        Just row

                    _ ->
                        Nothing
            messageRow :: ConversationPageFlag -> Maybe Message
            messageRow flag =
                case flag of
                    Chat.MessageFlag row ->
                        Just row

                    _ ->
                        Nothing
            participantRow :: ConversationPageFlag -> Maybe PersonId
            participantRow flag =
                case flag of
                    Chat.ParticipantFlag personId ->
                        Just personId

                    _ ->
                        Nothing
            generationRow :: ConversationPageFlag -> Maybe GenerationSummary
            generationRow flag =
                case flag of
                    Chat.GenerationFlag row ->
                        Just row

                    _ ->
                        Nothing
        full <- run (Chat.getConversationPageFlags conversationId)
        details <- run (Chat.getConversationDetailsFlags conversationId)
        conversation <- run (Conversation.getConversation conversationId)
        people <- run Person.getAllPersons
        messages <- run (Chat.getMessages conversationId)
        participants <- run (Conversation.getParticipants conversationId)
        generations <- run (Generation.getGenerationSummaries conversationId)
        assert
            ( map (.id) (Maybe.mapMaybe conversationRow full)
                == maybe [] (\row -> [row.id]) conversation
            )
            "Full refresh includes exactly the requested conversation, or none when missing"
        assert
            (null (Maybe.mapMaybe conversationRow details))
            "Details refresh omits the conversation"
        assert
            ( length details
                == 1 + length people + length messages + length participants + length generations
            )
            "Details refresh has exactly one flag per source row"
        assert
            (length full == length details + maybe 0 (const 1) conversation)
            "Full refresh adds only the conversation flag"
        currentPerson <- run LocalAccount.getCurrentPerson
        forM_ [full, details] $ \flags -> do
            assert
                (Maybe.mapMaybe currentPersonRow flags == [currentPerson])
                "Refresh includes the server-owned human identity exactly once"
            assert
                ( sort
                    ( map
                        (\row -> (row.id, row.name, profileFields row))
                        (Maybe.mapMaybe personRow flags)
                    )
                    == sort (map (\row -> (row.id, row.name, profileFields row)) people)
                )
                "Refresh preserves every person exactly once"
            assert
                ( sort
                    ( map
                        (\row -> (row.id, row.conversationId, row.author, row.content))
                        (Maybe.mapMaybe messageRow flags)
                    )
                    == sort
                        (map (\row -> (row.id, row.conversationId, row.author, row.content)) messages)
                )
                "Refresh preserves messages and scopes them to the conversation"
            assert
                (sort (Maybe.mapMaybe participantRow flags) == sort participants)
                "Refresh preserves participants exactly once"
            assert
                ( sort
                    ( map
                        (\row -> (row.id, row.speaker, row.error))
                        (Maybe.mapMaybe generationRow flags)
                    )
                    == sort (map (\row -> (row.id, row.speaker, row.error)) generations)
                )
                "Refresh preserves generation summaries exactly once"


checkTools :: Database.Database -> IO ()
checkTools database =
    do
        let
            run :: Database.Transaction a -> IO a
            run =
                Database.runTransaction database
            encodeText :: (ToJSON a) => a -> Text
            encodeText =
                Text.decodeUtf8 . Lazy.toStrict . encode
            tool identifier name args =
                do
                    let
                        item =
                            object
                                [ "type" .= ("function_call" :: Text)
                                , "call_id" .= (identifier :: Text)
                                , "name" .= (name :: Text)
                                , "arguments" .= encodeText args
                                ]
                        reasoning =
                            object
                                ["type" .= ("reasoning" :: Text), "encrypted_content" .= ("test" :: Text)]
                    either
                        (ioError . userError)
                        pure
                        ( OpenAI.parseResponse
                            ( encode
                                (object ["status" .= ("completed" :: Text), "output" .= [reasoning, item]])
                            )
                        )
            result history =
                do
                    let
                        decoded =
                            do
                                value <- case reverse history of
                                    latest : _ ->
                                        Just latest

                                    [] ->
                                        Nothing
                                text <- parseMaybe (withObject "tool output" (.: "output")) value
                                either (const Nothing) Just (eitherDecodeStrict (Text.encodeUtf8 text))
                    maybe (ioError (userError "Missing tool result")) pure decoded
            request _ history =
                object ["input" .= history]
        person <- run (Person.createNewPerson "Tool integration person")
        Just initial <- run (Person.loadPersonPage person)
        _ <-
            run
                (Person.updatePersonIdentity person initial.person.revision "Curious." "Learn.")
        conversation <- run (Conversation.createConversation "Tool loop" person)
        generation <- run (Chat.requestTurn conversation 0 person)
        calls <- newIORef (0 :: Int)
        let
            generate _ history =
                do
                    step <- atomicModifyIORef' calls (\n -> (n + 1, n))
                    messages <- run (Chat.getMessages conversation)
                    assert (null messages) "Only the final tool-free response becomes public speech"
                    case step of
                        0 ->
                            tool "create" "create_goal" (object ["description" .= ("Learn tools" :: Text)])

                        1 ->
                            do
                                value <- result history
                                let
                                    identifier =
                                        parseMaybe (withObject "result" (.: "goal_id")) value :: Maybe Text
                                assert
                                    (parseMaybe (withObject "result" (.: "ok")) value == Just True)
                                    "Create goal reports committed success"
                                [goal] <- run (Goal.getGoals person)
                                let
                                    GoalId.GoalId goalNumber = goal.id
                                assert
                                    (identifier == Just (T.pack (show goalNumber)))
                                    "Created ID reaches the model"
                                assert
                                    (T.isInfixOf "encrypted_content" (encodeText history))
                                    "Reasoning is replayed"
                                -- Same call ID is not applied twice.
                                tool "create" "create_goal" (object ["description" .= ("Learn tools" :: Text)])

                        2 ->
                            do
                                goals <- run (Goal.getGoals person)
                                assert (length goals == 1) "Repeated call ID does not duplicate a goal"
                                tool "invalid" "complete_goal" (object ["goal_id" .= ("999999" :: Text)])

                        3 ->
                            do
                                value <- result history
                                assert
                                    (parseMaybe (withObject "result" (.: "ok")) value == Just False)
                                    "Failed action is visible before the next response"
                                [goal] <- run (Goal.getGoals person)
                                let
                                    GoalId.GoalId goalNumber = goal.id
                                tool "complete" "complete_goal" (object ["goal_id" .= T.pack (show goalNumber)])

                        4 ->
                            do
                                [goal] <- run (Goal.getGoals person)
                                assert
                                    (case goal.status of
                                        Goal.Completed ->
                                            True

                                        _ ->
                                            False
                                    )
                                    "Completion commits before the model continues"
                                tool
                                    "memory"
                                    "save_memory"
                                    (object ["content" .= ("Tools return results." :: Text)])

                        5 ->
                            do
                                value <- result history
                                assert
                                    (parseMaybe (withObject "result" (.: "ok")) value == Just True)
                                    "Memory reports success"
                                [memory] <- run (Memory.getMemories person)
                                assert
                                    ( case memory.source of
                                        Origin.Reflection origin ->
                                            origin == generation

                                        _ ->
                                            False
                                    )
                                    "Tool memory retains generation provenance"
                                pure (OpenAI.Outcome "I saved what I learned." [] [])

                        _ ->
                            ioError (userError "Unexpected tool loop iteration")
        Worker.runCycle database request generate
        count <- readIORef calls
        assert (count == 6) "Tools continue through results to a final reply"
        messages <- run (Chat.getMessages conversation)
        assert
            (map (.content) messages == ["I saved what I learned."])
            "Exactly one final reply saved"
        Just snapshot <- run (Generation.getGenerationPrompt conversation generation)
        let
            PromptSnapshot.PromptSnapshot snapshotText = snapshot
        assert
            ( T.isInfixOf "function_call_output" snapshotText
                && T.isInfixOf "requests" snapshotText
            )
            "Inspector snapshot contains requests, calls, and results"
        mustFail (run (Chat.createGenerationGoal generation "Too late"))
        mustFail (run (Chat.saveGenerationMemory generation "Too late"))
        -- A later failure cannot undo an action already acknowledged by a tool.
        Just latest <- run (Conversation.getConversation conversation)
        failedTurn <- run (Chat.requestTurn conversation latest.revision person)
        Worker.runCycle
            database
            request
            ( \_ history ->
                if null history
                    then
                        tool "saved" "save_memory" (object ["content" .= ("Before failure" :: Text)])
                    else ioError (userError "Simulated provider failure")
            )
        Just failedPhase <- run (Generation.getGenerationPhase failedTurn)
        assert
            (case failedPhase of
                Generation.Failed ->
                    True

                _ ->
                    False
            )
            "Provider failure fails the turn"
        memories <- run (Memory.getMemories person)
        assert (length memories == 2) "Earlier successful actions survive reply failure"
        afterFailure <- run (Chat.getMessages conversation)
        assert (length afterFailure == 1) "Provider failure adds no reply"
        Just after <- run (Conversation.getConversation conversation)
        cancelled <- run (Chat.requestTurn conversation after.revision person)
        run (Generation.claimGeneration cancelled 180)
        _ <- run (Chat.saveGenerationMemory cancelled "Before cancellation")
        run (Chat.stopConversation conversation)
        mustFail (run (Chat.createGenerationGoal cancelled "After cancellation"))
        mustFail (run (Chat.saveGenerationMemory cancelled "After cancellation"))
        Just stopped <- run (Conversation.getConversation conversation)
        expired <- run (Chat.requestTurn conversation stopped.revision person)
        run (Generation.claimGeneration expired 1)
        threadDelay 1200000
        mustFail (run (Chat.createGenerationGoal expired "Expired"))
        mustFail (run (Chat.saveGenerationMemory expired "Expired"))
        run (Chat.expireGeneration expired)
        Just recovered <- run (Conversation.getConversation conversation)
        looping <- run (Chat.requestTurn conversation recovered.revision person)
        Worker.runCycle
            database
            request
            ( \_ history ->
                tool
                    (T.pack (show (length history)))
                    "unknown_tool"
                    (object [])
            )
        Just loopPhase <- run (Generation.getGenerationPhase looping)
        assert
            (case loopPhase of
                Generation.Failed ->
                    True

                _ ->
                    False
            )
            "Unbounded tool loops stop"
        putStrLn
            "PASS: tool results, replay, ownership, provenance, cancellation, failure, and loop limits."


main :: IO ()
main =
    do
        hSetBuffering stdout LineBuffering
        args <- getArgs
        case args of
            [url, "--tools-only"] ->
                Database.connect url >>= checkTools

            [url] ->
                runLifecycle url

            _ ->
                ioError (userError "Usage: people-integration URL [--tools-only]")


runLifecycle :: String -> IO ()
runLifecycle url =
    do
        database <- Database.connect url
        let
            run :: Database.Transaction a -> IO a
            run =
                Database.runTransaction database
        checkConversationPageFlags database
        mustFail (run LocalAccount.getCurrentPerson)
        mustFail (run (Person.createNewPerson ""))
        run Fixtures.fillDevelopmentData
        initialHuman <- run LocalAccount.getCurrentPerson
        Just initialProfile <- run (Person.loadPersonPage initialHuman)
        assert
            (initialProfile.person.name == "Chadtech")
            "Development account must have its explicit fixture name"
        personId <- run (Person.createNewPerson "MVP integration person")
        checkConversationPageFlags database
        -- Only the leftmost collection has rows; all joined collections are empty.
        checkConversationRefresh database (ConversationId.ConversationId 999999)
        Just initial <- run (Person.loadPersonPage personId)
        saved <-
            run
                ( Person.updatePersonIdentity
                    personId
                    initial.person.revision
                    "Curious and precise."
                    "Understand music."
                )
        savedProfile <- requireAiPersonProfile saved.person
        assert (savedProfile.identity == "Curious and precise.") "Identity round trip"
        cleared <-
            run (Person.updatePersonIdentity personId saved.person.revision "" "")
        clearedProfile <- requireAiPersonProfile cleared.person
        assert
            (T.null clearedProfile.identity && T.null clearedProfile.aspirations)
            "Empty identity fields round trip"
        _ <-
            run
                ( Person.updatePersonIdentity
                    personId
                    cleared.person.revision
                    savedProfile.identity
                    savedProfile.aspirations
                )
        putStrLn "Checking identity conflict"
        mustFail
            ( run
                (Person.updatePersonIdentity personId initial.person.revision "Stale" "Stale")
            )
        conversationId <-
            run (Conversation.createConversation "MVP integration conversation" personId)
        initialMembers <- run (Conversation.getParticipants conversationId)
        assert
            (initialMembers == [personId])
            "Creation does not automatically add the local human"
        mustFail (run (Chat.sendMessage conversationId "Not a participant"))
        run (Conversation.addParticipant conversationId initialHuman)
        run (Conversation.addParticipant conversationId initialHuman)
        selectedMembers <- run (Conversation.getParticipants conversationId)
        assert
            (sort selectedMembers == sort [personId, initialHuman])
            "Explicit participants are added exactly once"
        checkConversationPageFlags database
        otherConversationId <-
            run (Conversation.createConversation "Other integration conversation" personId)
        Just focusedConversation <- run (Conversation.getConversation conversationId)
        Just otherConversation <- run (Conversation.getConversation otherConversationId)
        assert
            ( focusedConversation.id == conversationId
                && otherConversation.id == otherConversationId
            )
            "Conversation detail returns only the requested conversation"
        missingConversation <-
            run (Conversation.getConversation (ConversationId.ConversationId 999999))
        assert
            (isNothing missingConversation)
            "Missing conversation detail returns Nothing"
        checkConversationRefresh database conversationId
        checkConversationRefresh database otherConversationId
        checkConversationRefresh database (ConversationId.ConversationId 999999)
        empty <- run (Chat.getMessages conversationId)
        assert (null empty) "Empty row decoding"
        members <- run (Conversation.getParticipants conversationId)
        print (length members)
        run (Chat.sendMessage conversationId "Hello")
        generationId <- run (Chat.requestTurn conversationId 0 personId)
        firstMessages <- run (Chat.getMessages conversationId)
        let
            generationNumber :: Word64
            GenerationId.GenerationId generationNumber = generationId
            messageNumbers :: [Word64]
            messageNumbers =
                map (\row -> case row.id of MessageId.MessageId value -> value) firstMessages
        assert
            (messageNumbers == [generationNumber])
            "Refresh coverage overlaps message and generation IDs"
        checkConversationRefresh database conversationId
        putStrLn "Checking duplicate request"
        mustFail (run (Chat.requestTurn conversationId 0 personId))
        run (Generation.claimGeneration generationId 180)
        putStrLn "Checking duplicate claim"
        mustFail (run (Generation.claimGeneration generationId 180))
        run (Generation.savePrompt generationId "Test prompt")
        Just storedPrompt <-
            run (Generation.getGenerationPrompt conversationId generationId)
        assert (storedPrompt == "Test prompt") "On-demand prompt round trip"
        missingPrompt <-
            run
                ( Generation.getGenerationPrompt
                    (ConversationId.ConversationId 999999)
                    generationId
                )
        assert (missingPrompt == Nothing) "Prompt lookup respects conversation identity"
        checkConversationRefresh database conversationId
        checkConversationRefresh database otherConversationId
        summaries <- run (Generation.getGenerationSummaries conversationId)
        assert (length summaries == 1) "Lightweight generation summary round trip"
        runningPhase <- run (Generation.getGenerationPhase generationId)
        assert
            (case runningPhase of
                Just Generation.Running ->
                    True

                _ ->
                    False
            )
            "Single-generation phase lookup"
        _ <- run (Chat.createGenerationGoal generationId "Explore rhythm")
        _ <- run (Chat.saveGenerationMemory generationId "We discussed rhythm.")
        run (Chat.finishGeneration generationId "Hello back")
        checkConversationRefresh database conversationId
        checkConversationRefresh database otherConversationId
        checkConversationRefresh database (ConversationId.ConversationId 999999)
        messages <- run (Chat.getMessages conversationId)
        assert (length messages == 2) "Exactly one user and one AI message"
        goals <- run (Goal.getGoals personId)
        memories <- run (Memory.getMemories personId)
        assert
            (length goals == 1 && length memories == 1)
            "Tool actions saved before the reply"
        putStrLn "Checking stale completion"
        mustFail
            ( run
                (Chat.finishGeneration generationId "Duplicate reply")
            )
        conversationRows <- run Conversation.getConversations
        let
            conversation :: Conversation
            [conversation] = filter (\c -> c.id == conversationId) conversationRows
        cancelledId <-
            run (Chat.requestTurn conversationId conversation.revision personId)
        run (Generation.claimGeneration cancelledId 180)
        run (Chat.stopConversation conversationId)
        putStrLn "Checking stale completion"
        mustFail
            ( run
                (Chat.finishGeneration cancelledId "Late reply")
            )
        finalMessages <- run (Chat.getMessages conversationId)
        finalGoals <- run (Goal.getGoals personId)
        assert
            (length finalMessages == 2 && length finalGoals == 1)
            "Cancelled results cannot mutate history or goals"
        afterStop <- run Conversation.getConversations
        let
            stopped :: Conversation
            [stopped] = filter (\c -> c.id == conversationId) afterStop
        assert
            (stopped.remainingTurns == 0)
            "Cancellation stops scheduling"
        otherPerson <- run (Person.createNewPerson "Other integration person")
        run (Goal.createGoal otherPerson "A private goal")
        [otherGoal] <- run (Goal.getGoals otherPerson)
        protectedId <-
            run (Chat.requestTurn conversationId stopped.revision personId)
        run (Generation.claimGeneration protectedId 180)
        mustFail
            ( run
                (Chat.completeGenerationGoal protectedId otherGoal.id)
            )
        rolledBackMessages <- run (Chat.getMessages conversationId)
        rolledBackGoals <- run (Goal.getGoals personId)
        rolledBackMemories <- run (Memory.getMemories personId)
        assert
            ( length rolledBackMessages == 2
                && length rolledBackGoals == 1
                && length rolledBackMemories == 1
            )
            "Invalid goal completion leaves saved state unchanged"
        let
            ownGoal :: Goal.Goal
            [ownGoal] = goals
        run (Chat.completeGenerationGoal protectedId ownGoal.id)
        run (Chat.finishGeneration protectedId "Rhythm explored.")
        [completed] <- run (Goal.getGoals personId)
        assert
            (case completed.status of
                Goal.Completed ->
                    True

                _ ->
                    False
            )
            "AI completes its own goal"
        [untouched] <- run (Goal.getGoals otherPerson)
        assert
            (case untouched.status of
                Goal.Active ->
                    True

                _ ->
                    False
            )
            "Another person's goal is unchanged"
        latestRows <- run Conversation.getConversations
        let
            latest :: Conversation
            [latest] = filter (\c -> c.id == conversationId) latestRows
        run (Conversation.setRunLength conversationId latest.revision 255)
        scheduledRows <- run Conversation.getScheduledConversations
        let
            scheduled :: Conversation
            [scheduled] = filter (\c -> c.id == conversationId) scheduledRows
        assert (scheduled.remainingTurns == 20) "Run length is capped"
        budgetId <- run (Chat.requestTurn conversationId scheduled.revision personId)
        busyRows <- run Conversation.getScheduledConversations
        assert
            (null (filter (\c -> c.id == conversationId) busyRows))
            "Busy conversations cannot be scheduled twice"
        run (Chat.failGeneration budgetId "Deliberate test failure")
        failedRows <- run Conversation.getConversations
        let
            failed :: Conversation
            [failed] = filter (\c -> c.id == conversationId) failedRows
        assert (failed.remainingTurns == 0) "Failure stops further paid calls"
        putStrLn "Checking persisted autonomy and competing workers"
        Just otherInitial <- run (Person.loadPersonPage otherPerson)
        _ <-
            run
                ( Person.updatePersonIdentity
                    otherPerson
                    otherInitial.person.revision
                    "Patient and analytical."
                    "Explore sound."
                )
        autoId <- run (Conversation.createConversation "Autonomy integration" personId)
        run (Conversation.addParticipant autoId otherPerson)
        run (Conversation.setAutonomy autoId 0 True 60)
        calls <- newIORef (0 :: Int)
        let
            generate :: Prompt.Prompt -> [Value] -> IO OpenAI.Outcome
            generate prompt _ =
                do
                    assert
                        (T.isInfixOf "identity" (Prompt.context prompt))
                        "Worker builds identity context"
                    if T.isInfixOf "Patient and analytical." (Prompt.context prompt)
                        then do
                            assert
                                (T.isInfixOf "A useful next step." (Prompt.context prompt))
                                "Second AI sees the first AI's reply"
                        else pure ()
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    pure (OpenAI.Outcome "A useful next step." [] [])
            cycleOnce :: Database.Database -> IO ()
            cycleOnce connection =
                Worker.runCycle connection (\_ _ -> object []) generate
        -- A fresh database connection emulates worker startup using saved state.
        restartedConnection <- Database.connect url
        concurrently_ (cycleOnce restartedConnection) (cycleOnce database)
        cycleOnce database
        count <- readIORef calls
        assert (count == 1) "Competing workers and immediate repolls generate once"
        autoMessages <- run (Chat.getMessages autoId)
        assert (length autoMessages == 1) "Autonomy begins without a user message"
        autoRows <- run Conversation.getConversations
        let
            automatic :: Conversation
            [automatic] = filter (\c -> c.id == autoId) autoRows
        assert automatic.autonomous "Autonomy stays enabled after completion"
        run (Conversation.setAutonomy autoId automatic.revision False 60)
        run (Conversation.setRunLength autoId (automatic.revision + 1) 2)
        cycleOnce database
        cycleOnce database
        cycleOnce database
        runMessages <- run (Chat.getMessages autoId)
        countAfterRun <- readIORef calls
        assert
            (length runMessages == 3 && countAfterRun == 3)
            "Bounded run performs exactly its saved turn count"
        let
            authors :: [Word64]
            authors =
                let
                    authorNumber :: Message -> Word64
                    authorNumber message =
                        case message.author of
                            PersonId.PersonId identifier ->
                                identifier
                in
                    map authorNumber runMessages
        assert
            (case authors of
                [a, b, c] ->
                    a /= b && a == c

                _ ->
                    False
            )
            "Speakers alternate across autonomous and bounded turns"
        putStrLn "Checking expired worker recovery"
        recoveryRows <- run Conversation.getConversations
        let
            recoverable :: Conversation
            [recoverable] = filter (\c -> c.id == autoId) recoveryRows
        run (Conversation.setAutonomy autoId recoverable.revision True 60)
        lostId <- run (Chat.requestTurn autoId (recoverable.revision + 1) personId)
        run (Generation.claimGeneration lostId 1)
        mustFail (run (Chat.expireGeneration lostId))
        threadDelay 1200000
        mustFail (run (Generation.savePrompt lostId "Too late"))
        mustFail
            (run (Chat.finishGeneration lostId "Late reply"))
        cycleOnce restartedConnection
        recoveredRows <- run Conversation.getConversations
        let
            recovered :: Conversation
            [recovered] = filter (\c -> c.id == autoId) recoveredRows
        assert
            (not recovered.autonomous && recovered.remainingTurns == 0)
            "Expired work pauses autonomous paid calls"
        expiredPhase <- run (Generation.getGenerationPhase lostId)
        assert
            (case expiredPhase of
                Just Generation.Failed ->
                    True

                _ ->
                    False
            )
            "Expired generation is visibly failed"
        finalCount <- readIORef calls
        assert (finalCount == 3) "Recovery does not repeat the uncertain API call"
        putStrLn "Checking memory selection under history pressure"
        promptId <-
            run (Conversation.createConversation "Prompt budget integration" personId)
        run (Conversation.addParticipant promptId initialHuman)
        run (Goal.createGoal personId "REQUIRED-GOAL: Compare rhythm structures.")
        run
            ( Memory.createMemory
                personId
                "RELEVANT-MEMORY: Recall alternating accents."
                "rhythm"
            )
        run (Memory.createMemory personId "UNMATCHED-MEMORY: Recall geology." "volcano")
        run
            (Memory.createMemory personId "RETIRED-MEMORY: An incorrect recollection." "")
        allMemories <- run (Memory.getMemories personId)
        let
            retiredMemory :: Memory.Memory
            [retiredMemory] =
                filter
                    ( \m -> case m.content of
                        MemoryContent.MemoryContent value ->
                            T.isPrefixOf "RETIRED-MEMORY" value
                    )
                    allMemories
        run (Memory.retireMemory personId retiredMemory.id)
        forM_ [0 .. 7] $ \n -> do
            run
                ( Chat.sendMessage
                    promptId
                    ( MessageContent.MessageContent
                        ("Older question " <> T.pack (show n) <> T.replicate 1000 "é")
                    )
                )
            queued <- run (Chat.requestTurn promptId (Revision.Revision (2 * n)) personId)
            run (Generation.claimGeneration queued 180)
            run
                ( Chat.finishGeneration
                    queued
                    ( MessageContent.MessageContent
                        ("Latest rhythm reply " <> T.pack (show n) <> T.replicate 1000 "é")
                    )
                )
        Just promptPerson <- run (Person.loadPersonPage personId)
        promptProfile <- requireAiPersonProfile promptPerson.person
        promptHistory <- run (Chat.getMessages promptId)
        promptGoals <- run (Goal.getGoals personId)
        promptMemories <- run (Memory.getMemories personId)
        Just promptHuman <- run (Person.loadPersonPage initialHuman)
        assert
            ( isLeft
                ( Prompt.buildPrompt
                    promptPerson.person
                    [promptPerson.person]
                    promptHistory
                    promptGoals
                    promptMemories
                )
            )
            "An unresolved message author must fail prompt assembly"
        blankPersonId <- run (Person.createNewPerson "  ")
        Just blankPerson <- run (Person.loadPersonPage blankPersonId)
        assert
            ( isLeft
                ( Prompt.buildPrompt
                    blankPerson.person
                    [promptPerson.person, promptHuman.person]
                    promptHistory
                    promptGoals
                    promptMemories
                )
            )
            "A blank person name must fail prompt assembly"
        prompt <-
            either
                (ioError . userError . Prompt.errorToString)
                pure
                ( Prompt.buildPrompt
                    promptPerson.person
                    [promptPerson.person, promptHuman.person]
                    promptHistory
                    promptGoals
                    promptMemories
                )
        let
            context :: Text
            context =
                Prompt.context prompt
        assert
            (BS.length (Text.encodeUtf8 context) <= 24000)
            "Context exceeds its UTF-8 byte budget"
        assert
            ( T.isInfixOf "REQUIRED-GOAL" context
                && T.isInfixOf "Latest rhythm reply 7" context
            )
            "Required goal or newest message was dropped"
        assert
            (T.isInfixOf "RELEVANT-MEMORY" context)
            "Long history crowded out a relevant memory"
        assert
            ( not (T.isInfixOf "UNMATCHED-MEMORY" context)
                && not (T.isInfixOf "RETIRED-MEMORY" context)
            )
            "Ineligible memory leaked into model context"
        assert
            (not (T.isInfixOf "Older question 0" context))
            "History pressure did not trim the oldest message"
        _ <-
            run
                ( Person.updatePersonIdentity
                    personId
                    promptPerson.person.revision
                    (T.replicate 25000 "x")
                    "Oversized identity test"
                )
        Just oversizedPerson <- run (Person.loadPersonPage personId)
        assert
            ( isLeft
                ( Prompt.buildPrompt
                    oversizedPerson.person
                    [oversizedPerson.person, promptHuman.person]
                    promptHistory
                    promptGoals
                    promptMemories
                )
            )
            "Oversized required context was silently truncated"
        _ <-
            run
                ( Person.updatePersonIdentity
                    personId
                    oversizedPerson.person.revision
                    promptProfile.identity
                    promptProfile.aspirations
                )
        cancellationId <-
            run (Conversation.createConversation "Worker cancellation integration" personId)
        run (Conversation.addParticipant cancellationId initialHuman)
        run (Chat.sendMessage cancellationId "Please stop this turn.")
        cancelledTurn <- run (Chat.requestTurn cancellationId 0 personId)
        finishedModel <- newIORef False
        heldModel <- newEmptyMVar
        let
            stoppedModel :: Prompt.Prompt -> [Value] -> IO OpenAI.Outcome
            stoppedModel _ _ =
                do
                    run (Chat.stopConversation cancellationId)
                    takeMVar heldModel
                    writeIORef finishedModel True
                    pure
                        (OpenAI.Outcome "Late message" [] [])
        stoppedResult <-
            timeout 15000000 (Worker.runCycle database (\_ _ -> object []) stoppedModel)
        assert
            (stoppedResult == Just ())
            "Cancellation must interrupt a held model request"
        modelFinished <- readIORef finishedModel
        assert (not modelFinished) "Cancellation interrupts the pending model request"
        stoppedPhase <- run (Generation.getGenerationPhase cancelledTurn)
        assert
            (case stoppedPhase of
                Just Generation.Cancelled ->
                    True

                _ ->
                    False
            )
            "Worker observes cancelled phase"
        cancellationMessages <- run (Chat.getMessages cancellationId)
        assert
            (length cancellationMessages == 1)
            "Cancelled model output creates no message"
        putStrLn "Checking human identity and independent posting"
        human <- run LocalAccount.getCurrentPerson
        sameHuman <- run LocalAccount.getCurrentPerson
        Just humanProfile <- run (Person.loadPersonPage human)
        assert (human == sameHuman) "Local account resolves a stable person"
        assert
            (case humanProfile.person.kind of
                Person.HumanPerson ->
                    True

                _ ->
                    False
            )
            "Local identity is human-controlled"
        mustFail
            ( run
                ( Person.updatePersonIdentity
                    human
                    humanProfile.person.revision
                    "Must not turn a human into an AI"
                    "Not a human field"
                )
            )
        mustFail (run (Goal.createGoal human "AI-only goal"))
        mustFail (run (Memory.createMemory human "AI-only recollection" ""))
        Just unchangedHuman <- run (Person.loadPersonPage human)
        humanGoals <- run (Goal.getGoals human)
        humanMemories <- run (Memory.getMemories human)
        assert
            ( profileFields unchangedHuman.person == Nothing
                && unchangedHuman.person.revision == humanProfile.person.revision
                && null humanGoals
                && null humanMemories
            )
            "Humans cannot acquire AI profiles, goals, or memories"
        renamedHuman <-
            run (Person.updateHumanPersonName human humanProfile.person.revision "Morgan")
        assert
            (renamedHuman.person.name == "Morgan" && renamedHuman.person.id == human)
            "Human name edits preserve participant identity"
        mustFail
            ( run
                (Person.updateHumanPersonName human humanProfile.person.revision "Stale name")
            )
        mustFail
            (run (Person.updateHumanPersonName human renamedHuman.person.revision ""))
        Just aiBeforeRename <- run (Person.loadPersonPage personId)
        mustFail
            ( run
                ( Person.updateHumanPersonName
                    personId
                    aiBeforeRename.person.revision
                    "Human name"
                )
            )
        humanConversation <-
            run (Conversation.createConversation "Human participation" personId)
        run (Conversation.addParticipant humanConversation human)
        assert
            ( isLeft
                ( Prompt.buildPrompt
                    humanProfile.person
                    [humanProfile.person]
                    []
                    []
                    []
                )
            )
            "Prompt assembly rejects a human generation target"
        beforeHuman <- run (Generation.getGenerationSummaries humanConversation)
        run (Chat.sendMessage humanConversation "A standalone human message")
        afterHuman <- run (Generation.getGenerationSummaries humanConversation)
        [humanMessage] <- run (Chat.getMessages humanConversation)
        assert
            (humanMessage.author == human && null beforeHuman && null afterHuman)
            "Posting records an explicit human author without requesting generation"
        mustFail (run (Chat.sendMessage humanConversation ""))
        mustFail
            (run (Chat.sendMessage (ConversationId.ConversationId 999999) "Missing"))
        mustFail (run (Chat.requestTurn humanConversation 0 human))
        outsider <- run (Person.createNewPerson "Not a member")
        mustFail (run (Chat.requestTurn humanConversation 0 outsider))
        queuedHuman <- run (Chat.requestTurn humanConversation 0 personId)
        let
            concurrentHuman :: Prompt.Prompt -> [Value] -> IO OpenAI.Outcome
            concurrentHuman prompt _ =
                do
                    assert
                        (T.isInfixOf "Morgan (human)" (Prompt.context prompt))
                        "Prompt roster distinguishes the human participant"
                    assert
                        (T.isInfixOf "Morgan: A standalone human message" (Prompt.context prompt))
                        "Prompt preserves human authorship"
                    run (Chat.sendMessage humanConversation "Posted while the AI was thinking")
                    assert
                        (not (T.isInfixOf "Posted while the AI was thinking" (Prompt.context prompt)))
                        "An in-flight reply keeps the context it actually saw"
                    pure (OpenAI.Outcome "AI response" [] [])
        Worker.runCycle database (\_ _ -> object []) concurrentHuman
        humanHistory <- run (Chat.getMessages humanConversation)
        assert
            (map (\message -> message.author) humanHistory == [human, human, personId])
            "Human posting during generation preserves both messages and AI completion"
        Just humanSnapshot <-
            run (Generation.getGenerationPrompt humanConversation queuedHuman)
        let
            PromptSnapshot.PromptSnapshot snapshotText = humanSnapshot
        assert
            (not (T.isInfixOf "Posted while the AI was thinking" snapshotText))
            "Saved prompt records the actual generation boundary"
        humanOnly <-
            run (Conversation.createConversation "Human-only conversation" human)
        run (Chat.sendMessage humanOnly "No AI needed")
        run (Conversation.setRunLength humanOnly 0 2)
        Worker.runCycle
            database
            (\_ _ -> object [])
            ( \_ _ -> ioError (userError "A human-only conversation must never call a model")
            )
        humanOnlyGenerations <- run (Generation.getGenerationSummaries humanOnly)
        Just humanOnlyState <- run (Conversation.getConversation humanOnly)
        assert
            (null humanOnlyGenerations && humanOnlyState.remainingTurns == 0)
            "Scheduler safely stops when there are no AI participants"
        checkConversationRefresh database humanConversation
        putStrLn "Checking development fixtures do not reset a running session"
        let
            seed :: IO ()
            seed =
                run Fixtures.fillDevelopmentData
            attemptSeed :: IO ()
            attemptSeed =
                do
                    _ <- try seed :: IO (Either SomeException ())
                    pure ()
        concurrently_ attemptSeed attemptSeed
        seed
        seededPeople <- run Person.getAllPersons
        let
            ada :: Person
            [ada] = filter (\p -> p.name == "Ada") seededPeople
            sam :: Person
            [sam] = filter (\p -> p.name == "Sam") seededPeople
        adaProfile <- requireAiPersonProfile ada
        samProfile <- requireAiPersonProfile sam
        assert
            (not (T.null adaProfile.identity) && not (T.null samProfile.identity))
            "Fixture AIs have usable identities"
        adaGoals <- run (Goal.getGoals ada.id)
        adaMemories <- run (Memory.getMemories ada.id)
        assert
            (length adaGoals == 1 && length adaMemories == 1)
            "Fixture goals and memories are seeded once"
        seededConversations <- run Conversation.getConversations
        let
            starter :: Conversation
            [starter] = filter (\c -> c.title == "Long-running conversation") seededConversations
        starterParticipants <- run (Conversation.getParticipants starter.id)
        assert
            ( length starterParticipants == 3
                && not starter.autonomous
                && starter.remainingTurns == 0
            )
            "Starter conversation is ready without starting paid calls"
        editedAda <-
            run
                ( Person.updatePersonIdentity
                    ada.id
                    ada.revision
                    "Identity changed during this session."
                    adaProfile.aspirations
                )
        seed
        Just preservedAda <- run (Person.loadPersonPage ada.id)
        preservedProfile <- requireAiPersonProfile preservedAda.person
        assert
            (preservedProfile.identity == "Identity changed during this session.")
            "Reseeding must preserve accumulated edits"
        reseededPeople <- run Person.getAllPersons
        reseededConversations <- run Conversation.getConversations
        assert
            ( length seededPeople == length reseededPeople
                && length seededConversations == length reseededConversations
            )
            "Repeated seeding creates no duplicate people or conversations"
        _ <-
            run
                ( Person.updatePersonIdentity
                    ada.id
                    editedAda.person.revision
                    adaProfile.identity
                    adaProfile.aspirations
                )
        putStrLn
            "PASS: lifecycle, prompt selection, active cancellation, and \
            \duplicate-safe session fixtures."

        checkTools database
