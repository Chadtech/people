{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AIProfile (AIProfile)
import qualified LocalAccount
import qualified MemoryContent
import qualified MessageContent
import qualified Note
import People.DomainInstances ()
import qualified PromptSnapshot
import qualified Revision

import qualified Chat
import Conversation (Conversation)
import qualified Conversation
import qualified Generation
import qualified GenerationId
import qualified MessageId
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import qualified ConversationId
import Data.Aeson (object)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List (sort)
import Data.Maybe (isNothing)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import qualified Fixtures
import qualified Goal
import qualified Memory
import qualified Origin
import qualified People.Database as Database
import qualified People.OpenAI as OpenAI
import qualified People.Prompt as Prompt
import qualified People.Worker as Worker
import Person (Person)
import qualified Person
import qualified PersonId
import System.Environment (getArgs)


assert :: Bool -> String -> IO ()
assert condition message = unless condition (ioError (userError message))


mustFail :: IO a -> IO ()
mustFail operation = do
    result <- try (operation >> pure ()) :: IO (Either SomeException ())
    case result of
        Left _ -> pure ()
        Right _ -> ioError (userError "Expected transaction rejection")


requireAIProfile :: Person -> IO AIProfile
requireAIProfile person =
    case person.kind of
        Person.AI profile -> pure profile
        Person.Human -> ioError (userError "Expected an AI test person")


profileFields :: Person -> Maybe (Text, Text)
profileFields person =
    case person.kind of
        Person.AI profile -> Just (profile.identity, profile.aspirations)
        Person.Human -> Nothing


checkConversationPageFlags :: Database.Database -> IO ()
checkConversationPageFlags database = do
    flags <- Database.runTransaction database Conversation.getAllConversationsPageFlags
    conversations <- Database.runTransaction database Conversation.getConversations
    people <- Database.runTransaction database Person.getAllPersons
    let
        conversationIds :: [ConversationId.ConversationId]
        conversationIds =
            concatMap
                (\flag -> case flag of
                    Conversation.ConversationFlag conversation -> [conversation.id]
                    Conversation.PersonFlag _ -> []
                )
                flags
        personIds :: [PersonId.PersonId]
        personIds =
            concatMap
                (\flag -> case flag of
                    Conversation.ConversationFlag _ -> []
                    Conversation.PersonFlag person -> [person.id]
                )
                flags
    assert
        (sort conversationIds == sort (map (\conversation -> conversation.id) conversations))
        "Page flags preserve every conversation exactly once"
    assert
        (sort personIds == sort (map (\person -> person.id) people))
        "Page flags preserve every person exactly once"


checkConversationRefresh :: Database.Database -> ConversationId.ConversationId -> IO ()
checkConversationRefresh database conversationId = do
    let
        run :: Database.Transaction a -> IO a
        run = Database.runTransaction database
    full <- run (Chat.getConversationPageFlags conversationId)
    details <- run (Chat.getConversationDetailsFlags conversationId)
    conversation <- run (Conversation.getConversation conversationId)
    people <- run Person.getAllPersons
    messages <- run (Chat.getMessages conversationId)
    participants <- run (Conversation.getParticipants conversationId)
    generations <- run (Generation.getGenerationSummaries conversationId)
    assert
        ([row.id | Chat.ConversationFlag row <- full] == maybe [] (\row -> [row.id]) conversation)
        "Full refresh includes exactly the requested conversation, or none when missing"
    assert
        (null [row.id | Chat.ConversationFlag row <- details])
        "Details refresh omits the conversation"
    assert
        (length details == 1 + length people + length messages + length participants + length generations)
        "Details refresh has exactly one flag per source row"
    assert
        (length full == length details + maybe 0 (const 1) conversation)
        "Full refresh adds only the conversation flag"
    currentPerson <- run LocalAccount.getCurrentPerson
    forM_ [full, details] $ \flags -> do
        assert ([personId | Chat.CurrentPersonFlag personId <- flags] == [currentPerson])
            "Refresh includes the server-owned human identity exactly once"
        assert
            (sort [(row.id, row.name, profileFields row) | Chat.PersonFlag row <- flags]
                == sort (map (\row -> (row.id, row.name, profileFields row)) people))
            "Refresh preserves every person exactly once"
        assert
            (sort [(row.id, row.conversationId, row.author, row.content) | Chat.MessageFlag row <- flags]
                == sort (map (\row -> (row.id, row.conversationId, row.author, row.content)) messages))
            "Refresh preserves messages and scopes them to the conversation"
        assert
            (sort [personId | Chat.ParticipantFlag personId <- flags] == sort participants)
            "Refresh preserves participants exactly once"
        assert
            (sort [(row.id, row.speaker, row.error) | Chat.GenerationFlag row <- flags]
                == sort (map (\row -> (row.id, row.speaker, row.error)) generations))
            "Refresh preserves generation summaries exactly once"


main :: IO ()
main = do
    [url] <- getArgs
    database <- Database.connect url
    let
        run :: Database.Transaction a -> IO a
        run = Database.runTransaction database
    checkConversationPageFlags database
    checkConversationRefresh database (ConversationId.ConversationId 999999)
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
    savedProfile <- requireAIProfile saved.person
    assert (savedProfile.identity == "Curious and precise.") "Identity round trip"
    cleared <-
        run (Person.updatePersonIdentity personId saved.person.revision "" "")
    clearedProfile <- requireAIProfile cleared.person
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
    let
        conversationNumber :: Word64
        ConversationId.ConversationId conversationNumber = conversationId
    checkConversationPageFlags database
    otherConversationId <-
        run (Conversation.createConversation "Other integration conversation" personId)
    Just focusedConversation <- run (Conversation.getConversation conversationId)
    Just otherConversation <- run (Conversation.getConversation otherConversationId)
    assert
        (focusedConversation.id == conversationId && otherConversation.id == otherConversationId)
        "Conversation detail returns only the requested conversation"
    missingConversation <-
        run (Conversation.getConversation (ConversationId.ConversationId 999999))
    assert (isNothing missingConversation) "Missing conversation detail returns Nothing"
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
        (generationNumber == conversationNumber && messageNumbers == [conversationNumber])
        "Refresh coverage overlaps person, participant, message, generation, and conversation IDs"
    checkConversationRefresh database conversationId
    putStrLn "Checking duplicate request"
    mustFail (run (Chat.requestTurn conversationId 0 personId))
    run (Generation.claimGeneration generationId 180)
    putStrLn "Checking duplicate claim"
    mustFail (run (Generation.claimGeneration generationId 180))
    run (Generation.savePrompt generationId "Test prompt")
    Just storedPrompt <- run (Generation.getGenerationPrompt conversationId generationId)
    assert (storedPrompt == "Test prompt") "On-demand prompt round trip"
    missingPrompt <-
        run
            (Generation.getGenerationPrompt (ConversationId.ConversationId 999999) generationId)
    assert (missingPrompt == Nothing) "Prompt lookup respects conversation identity"
    checkConversationRefresh database conversationId
    checkConversationRefresh database otherConversationId
    summaries <- run (Generation.getGenerationSummaries conversationId)
    assert (length summaries == 1) "Lightweight generation summary round trip"
    runningPhase <- run (Generation.getGenerationPhase generationId)
    assert
        (case runningPhase of Just Generation.Running -> True; _ -> False)
        "Single-generation phase lookup"
    run
        ( Chat.finishGeneration
            generationId
            "Hello back"
            "Explore rhythm"
            "We discussed rhythm."
            Nothing
            "Shared rhythm plan"
        )
    checkConversationRefresh database conversationId
    checkConversationRefresh database otherConversationId
    checkConversationRefresh database (ConversationId.ConversationId 999999)
    messages <- run (Chat.getMessages conversationId)
    assert (length messages == 2) "Exactly one user and one AI message"
    goals <- run (Goal.getGoals personId)
    memories <- run (Memory.getMemories personId)
    assert (length goals == 1 && length memories == 1) "Atomic reflection and goal"
    putStrLn "Checking stale completion"
    mustFail
        ( run
            ( Chat.finishGeneration
                generationId
                "Duplicate reply"
                "Duplicate goal"
                "Duplicate memory"
                Nothing
                ""
            )
        )
    conversationRows <- run Conversation.getConversations
    let
        conversation :: Conversation
        [conversation] = filter (\c -> c.id == conversationId) conversationRows
    assert (conversation.note == "Shared rhythm plan") "Shared note round trip"
    [firstRevision] <- run (Chat.getNoteRevisions conversationId)
    assert
        ( firstRevision.generationId == generationId
            && firstRevision.content == "Shared rhythm plan"
        )
        "Note revision records its source generation"
    cancelledId <-
        run (Chat.requestTurn conversationId conversation.revision personId)
    run (Generation.claimGeneration cancelledId 180)
    run (Chat.stopConversation conversationId)
    putStrLn "Checking stale completion"
    mustFail
        ( run
            ( Chat.finishGeneration
                cancelledId
                "Late reply"
                "Late goal"
                "Late memory"
                Nothing
                "Late note"
            )
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
        (stopped.note == "Shared rhythm plan" && stopped.remainingTurns == 0)
        "Cancellation preserves note and stops scheduling"
    otherPerson <- run (Person.createNewPerson "Other integration person")
    run (Goal.createGoal otherPerson "A private goal")
    [otherGoal] <- run (Goal.getGoals otherPerson)
    protectedId <-
        run (Chat.requestTurn conversationId stopped.revision personId)
    run (Generation.claimGeneration protectedId 180)
    mustFail
        ( run
            ( Chat.finishGeneration
                protectedId
                "Rejected reply"
                "Rejected goal"
                "Rejected memory"
                (Just otherGoal.id)
                "Rejected note"
            )
        )
    rolledBackMessages <- run (Chat.getMessages conversationId)
    rolledBackGoals <- run (Goal.getGoals personId)
    rolledBackMemories <- run (Memory.getMemories personId)
    rolledBackConversations <- run Conversation.getConversations
    let
        rolledBack :: Conversation
        [rolledBack] = filter (\c -> c.id == conversationId) rolledBackConversations
    assert
        ( length rolledBackMessages == 2
            && length rolledBackGoals == 1
            && length rolledBackMemories == 1
            && rolledBack.note == "Shared rhythm plan"
        )
        "Invalid goal completion rolls back every action"
    rolledBackNotes <- run (Chat.getNoteRevisions conversationId)
    assert
        (length rolledBackNotes == 1)
        "Cancelled and invalid actions cannot append note revisions"
    let
        ownGoal :: Goal.Goal
        [ownGoal] = goals
    run
        ( Chat.finishGeneration
            protectedId
            "Rhythm explored."
            ""
            ""
            (Just ownGoal.id)
            "Finished rhythm plan"
        )
    [completed] <- run (Goal.getGoals personId)
    assert
        (case completed.status of Goal.Completed -> True; _ -> False)
        "AI completes its own goal"
    [untouched] <- run (Goal.getGoals otherPerson)
    assert
        (case untouched.status of Goal.Active -> True; _ -> False)
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
        generate :: Prompt.Prompt -> IO OpenAI.Outcome
        generate prompt = do
            assert
                (T.isInfixOf "identity" (Prompt.context prompt))
                "Worker builds identity context"
            if T.isInfixOf "Patient and analytical." (Prompt.context prompt)
                then do
                    assert
                        (T.isInfixOf "A drafted the shared rhythm plan." (Prompt.context prompt))
                        "Second AI sees the first AI's saved note"
                    assert
                        (T.isInfixOf "A useful next step." (Prompt.context prompt))
                        "Second AI sees the first AI's reply"
                else pure ()
            atomicModifyIORef' calls (\n -> (n + 1, ()))
            let
                note :: Note.Note
                note =
                    if T.isInfixOf "Patient and analytical." (Prompt.context prompt)
                        then "B revised the shared rhythm plan."
                        else "A drafted the shared rhythm plan."
            pure (OpenAI.Outcome "A useful next step." "" "" Nothing note)
        cycleOnce :: Database.Database -> IO ()
        cycleOnce connection = Worker.runCycle connection (const (object [])) generate
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
        authors = [n | message <- runMessages, (PersonId.PersonId n) <- [message.author]]
    assert
        (case authors of [a, b, c] -> a /= b && a == c; _ -> False)
        "Speakers alternate across autonomous and bounded turns"
    activityRevisions <- run (Chat.getNoteRevisions autoId)
    assert
        (length activityRevisions == 3)
        "Every accepted note change keeps its revision"
    assert
        ( any
            (\revision -> revision.content == "A drafted the shared rhythm plan.")
            activityRevisions
            && any
                (\revision -> revision.content == "B revised the shared rhythm plan.")
                activityRevisions
        )
        "Two AI participants create and revise a shared artifact"
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
        (run (Chat.finishGeneration lostId "Late reply" "" "" Nothing "Late note"))
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
        (case expiredPhase of Just Generation.Failed -> True; _ -> False)
        "Expired generation is visibly failed"
    finalCount <- readIORef calls
    assert (finalCount == 3) "Recovery does not repeat the uncertain API call"
    putStrLn "Checking memory selection under history pressure"
    promptId <- run (Conversation.createConversation "Prompt budget integration" personId)
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
                    MemoryContent.MemoryContent value -> T.isPrefixOf "RETIRED-MEMORY" value
                )
                allMemories
    run (Memory.retireMemory personId retiredMemory.id)
    forM_ [0 .. 7] $ \n -> do
        run
            ( Chat.sendMessage promptId
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
                ""
                ""
                Nothing
                ""
            )
    Just promptPerson <- run (Person.loadPersonPage personId)
    promptProfile <- requireAIProfile promptPerson.person
    promptRows <- run Conversation.getConversations
    let
        promptConversation :: Conversation
        [promptConversation] = filter (\c -> c.id == promptId) promptRows
    promptHistory <- run (Chat.getMessages promptId)
    promptGoals <- run (Goal.getGoals personId)
    promptMemories <- run (Memory.getMemories personId)
    prompt <-
        either
            (ioError . userError)
            pure
            ( Prompt.buildPrompt
                promptPerson.person
                [promptPerson.person]
                promptConversation
                promptHistory
                promptGoals
                promptMemories
            )
    let
        context :: Text
        context = Prompt.context prompt
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
                [oversizedPerson.person]
                promptConversation
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
    run (Chat.sendMessage cancellationId "Please stop this turn.")
    cancelledTurn <- run (Chat.requestTurn cancellationId 0 personId)
    finishedModel <- newIORef False
    let
        stoppedModel :: Prompt.Prompt -> IO OpenAI.Outcome
        stoppedModel _ = do
            run (Chat.stopConversation cancellationId)
            threadDelay 1500000
            writeIORef finishedModel True
            pure
                (OpenAI.Outcome "Late message" "Late goal" "Late memory" Nothing "Late note")
    Worker.runCycle database (const (object [])) stoppedModel
    modelFinished <- readIORef finishedModel
    assert (not modelFinished) "Cancellation interrupts the pending model request"
    stoppedPhase <- run (Generation.getGenerationPhase cancelledTurn)
    assert
        (case stoppedPhase of Just Generation.Cancelled -> True; _ -> False)
        "Worker observes cancelled phase"
    cancellationMessages <- run (Chat.getMessages cancellationId)
    cancellationNotes <- run (Chat.getNoteRevisions cancellationId)
    assert
        (length cancellationMessages == 1 && null cancellationNotes)
        "Cancelled model output creates no message or note"
    putStrLn "Checking human identity and independent posting"
    human <- run LocalAccount.getCurrentPerson
    sameHuman <- run LocalAccount.getCurrentPerson
    Just humanProfile <- run (Person.loadPersonPage human)
    assert (human == sameHuman) "Local account resolves a stable person"
    assert (case humanProfile.person.kind of Person.Human -> True; _ -> False)
        "Local identity is human-controlled"
    mustFail
        (run (Person.updatePersonIdentity human humanProfile.person.revision
            "Must not turn a human into an AI" "Not a human field"))
    mustFail (run (Goal.createGoal human "AI-only goal"))
    mustFail (run (Memory.createMemory human "AI-only recollection" ""))
    Just unchangedHuman <- run (Person.loadPersonPage human)
    humanGoals <- run (Goal.getGoals human)
    humanMemories <- run (Memory.getMemories human)
    assert
        (profileFields unchangedHuman.person == Nothing
            && unchangedHuman.person.revision == humanProfile.person.revision
            && null humanGoals && null humanMemories)
        "Humans cannot acquire AI profiles, goals, or memories"
    humanConversation <- run (Conversation.createConversation "Human participation" personId)
    Just humanConversationState <- run (Conversation.getConversation humanConversation)
    assert
        (isLeft (Prompt.buildPrompt humanProfile.person [humanProfile.person]
            humanConversationState [] [] []))
        "Prompt assembly rejects a human generation target"
    beforeHuman <- run (Generation.getGenerationSummaries humanConversation)
    run (Chat.sendMessage humanConversation "A standalone human message")
    afterHuman <- run (Generation.getGenerationSummaries humanConversation)
    [humanMessage] <- run (Chat.getMessages humanConversation)
    assert (humanMessage.author == human && null beforeHuman && null afterHuman)
        "Posting records an explicit human author without requesting generation"
    mustFail (run (Chat.sendMessage humanConversation ""))
    mustFail (run (Chat.sendMessage (ConversationId.ConversationId 999999) "Missing"))
    mustFail (run (Chat.requestTurn humanConversation 0 human))
    outsider <- run (Person.createNewPerson "Not a member")
    mustFail (run (Chat.requestTurn humanConversation 0 outsider))
    queuedHuman <- run (Chat.requestTurn humanConversation 0 personId)
    let
        concurrentHuman :: Prompt.Prompt -> IO OpenAI.Outcome
        concurrentHuman prompt = do
            assert (T.isInfixOf "You (human)" (Prompt.context prompt))
                "Prompt roster distinguishes the human participant"
            assert (T.isInfixOf "You: A standalone human message" (Prompt.context prompt))
                "Prompt preserves human authorship"
            run (Chat.sendMessage humanConversation "Posted while the AI was thinking")
            assert (not (T.isInfixOf "Posted while the AI was thinking" (Prompt.context prompt)))
                "An in-flight reply keeps the context it actually saw"
            pure (OpenAI.Outcome "AI response" "" "" Nothing "")
    Worker.runCycle database (const (object [])) concurrentHuman
    humanHistory <- run (Chat.getMessages humanConversation)
    assert (map (\message -> message.author) humanHistory == [human, human, personId])
        "Human posting during generation preserves both messages and AI completion"
    Just humanSnapshot <- run (Generation.getGenerationPrompt humanConversation queuedHuman)
    let PromptSnapshot.PromptSnapshot snapshotText = humanSnapshot
    assert (not (T.isInfixOf "Posted while the AI was thinking" snapshotText))
        "Saved prompt records the actual generation boundary"
    humanOnly <- run (Conversation.createConversation "Human-only conversation" human)
    run (Chat.sendMessage humanOnly "No AI needed")
    run (Conversation.setRunLength humanOnly 0 2)
    Worker.runCycle database (const (object []))
        (\_ -> ioError (userError "A human-only conversation must never call a model"))
    humanOnlyGenerations <- run (Generation.getGenerationSummaries humanOnly)
    Just humanOnlyState <- run (Conversation.getConversation humanOnly)
    assert (null humanOnlyGenerations && humanOnlyState.remainingTurns == 0)
        "Scheduler safely stops when there are no AI participants"
    checkConversationRefresh database humanConversation
    putStrLn "Checking development fixtures do not reset a running session"
    let
        seed :: IO ()
        seed = run Fixtures.fillDevelopmentData
        attemptSeed :: IO ()
        attemptSeed = do
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
    adaProfile <- requireAIProfile ada
    samProfile <- requireAIProfile sam
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
    preservedProfile <- requireAIProfile preservedAda.person
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
