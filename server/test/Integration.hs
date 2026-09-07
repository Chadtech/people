{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified MemoryContent
import qualified MessageContent
import qualified Note
import People.DomainInstances ()
import qualified Revision

import qualified Chat
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import qualified ConversationId
import Data.Aeson (object)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.IORef
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


main :: IO ()
main = do
    [url] <- getArgs
    database <- Database.connect url
    let
        run :: Database.Transaction a -> IO a
        run = Database.runTransaction database
    personId <- run (Person.createNewPerson "MVP integration person")
    Just initial <- run (Person.loadPersonPage personId)
    saved <-
        run
            ( Person.updatePersonIdentity
                personId
                initial.person.revision
                "Curious and precise."
                "Understand music."
            )
    assert (saved.person.identity == "Curious and precise.") "Identity round trip"
    cleared <-
        run (Person.updatePersonIdentity personId saved.person.revision "" "")
    assert
        (T.null cleared.person.identity && T.null cleared.person.aspirations)
        "Empty identity fields round trip"
    _ <-
        run
            ( Person.updatePersonIdentity
                personId
                cleared.person.revision
                saved.person.identity
                saved.person.aspirations
            )
    putStrLn "Checking identity conflict"
    mustFail
        ( run
            (Person.updatePersonIdentity personId initial.person.revision "Stale" "Stale")
        )
    conversationId <-
        run (Chat.createConversation "MVP integration conversation" personId)
    empty <- run (Chat.getMessages conversationId)
    assert (null empty) "Empty row decoding"
    members <- run (Chat.getParticipants conversationId)
    print (length members)
    generationId <- run (Chat.requestTurn conversationId 0 personId "Hello")
    putStrLn "Checking duplicate request"
    mustFail (run (Chat.requestTurn conversationId 0 personId "Duplicate"))
    run (Chat.claimGeneration generationId 180)
    putStrLn "Checking duplicate claim"
    mustFail (run (Chat.claimGeneration generationId 180))
    run (Chat.savePrompt generationId "Test prompt")
    Just storedPrompt <- run (Chat.getGenerationPrompt conversationId generationId)
    assert (storedPrompt == "Test prompt") "On-demand prompt round trip"
    missingPrompt <-
        run
            (Chat.getGenerationPrompt (ConversationId.ConversationId 999999) generationId)
    assert (missingPrompt == Nothing) "Prompt lookup respects conversation identity"
    summaries <- run (Chat.getGenerationSummaries conversationId)
    assert (length summaries == 1) "Lightweight generation summary round trip"
    runningPhase <- run (Chat.getGenerationPhase generationId)
    assert
        (case runningPhase of Just Chat.Running -> True; _ -> False)
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
    conversationRows <- run Chat.getConversations
    let
        conversation :: Chat.Conversation
        [conversation] = filter (\c -> c.id == conversationId) conversationRows
    assert (conversation.note == "Shared rhythm plan") "Shared note round trip"
    [firstRevision] <- run (Chat.getNoteRevisions conversationId)
    assert
        ( firstRevision.generationId == generationId
            && firstRevision.content == "Shared rhythm plan"
        )
        "Note revision records its source generation"
    cancelledId <-
        run (Chat.requestTurn conversationId conversation.revision personId "")
    run (Chat.claimGeneration cancelledId 180)
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
    afterStop <- run Chat.getConversations
    let
        stopped :: Chat.Conversation
        [stopped] = filter (\c -> c.id == conversationId) afterStop
    assert
        (stopped.note == "Shared rhythm plan" && stopped.remainingTurns == 0)
        "Cancellation preserves note and stops scheduling"
    otherPerson <- run (Person.createNewPerson "Other integration person")
    run (Goal.createGoal otherPerson "A private goal")
    [otherGoal] <- run (Goal.getGoals otherPerson)
    protectedId <-
        run (Chat.requestTurn conversationId stopped.revision personId "")
    run (Chat.claimGeneration protectedId 180)
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
    rolledBackConversations <- run Chat.getConversations
    let
        rolledBack :: Chat.Conversation
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
    latestRows <- run Chat.getConversations
    let
        latest :: Chat.Conversation
        [latest] = filter (\c -> c.id == conversationId) latestRows
    run (Chat.setRunLength conversationId latest.revision 255)
    scheduledRows <- run Chat.getScheduledConversations
    let
        scheduled :: Chat.Conversation
        [scheduled] = filter (\c -> c.id == conversationId) scheduledRows
    assert (scheduled.remainingTurns == 20) "Run length is capped"
    budgetId <- run (Chat.requestTurn conversationId scheduled.revision personId "")
    busyRows <- run Chat.getScheduledConversations
    assert
        (null (filter (\c -> c.id == conversationId) busyRows))
        "Busy conversations cannot be scheduled twice"
    run (Chat.failGeneration budgetId "Deliberate test failure")
    failedRows <- run Chat.getConversations
    let
        failed :: Chat.Conversation
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
    autoId <- run (Chat.createConversation "Autonomy integration" personId)
    run (Chat.addParticipant autoId otherPerson)
    run (Chat.setAutonomy autoId 0 True 60)
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
    autoRows <- run Chat.getConversations
    let
        automatic :: Chat.Conversation
        [automatic] = filter (\c -> c.id == autoId) autoRows
    assert automatic.autonomous "Autonomy stays enabled after completion"
    run (Chat.setAutonomy autoId automatic.revision False 60)
    run (Chat.setRunLength autoId (automatic.revision + 1) 2)
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
        authors = [n | message <- runMessages, Just (PersonId.PersonId n) <- [message.author]]
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
    recoveryRows <- run Chat.getConversations
    let
        recoverable :: Chat.Conversation
        [recoverable] = filter (\c -> c.id == autoId) recoveryRows
    run (Chat.setAutonomy autoId recoverable.revision True 60)
    lostId <- run (Chat.requestTurn autoId (recoverable.revision + 1) personId "")
    run (Chat.claimGeneration lostId 1)
    mustFail (run (Chat.expireGeneration lostId))
    threadDelay 1200000
    mustFail (run (Chat.savePrompt lostId "Too late"))
    mustFail
        (run (Chat.finishGeneration lostId "Late reply" "" "" Nothing "Late note"))
    cycleOnce restartedConnection
    recoveredRows <- run Chat.getConversations
    let
        recovered :: Chat.Conversation
        [recovered] = filter (\c -> c.id == autoId) recoveredRows
    assert
        (not recovered.autonomous && recovered.remainingTurns == 0)
        "Expired work pauses autonomous paid calls"
    recoveredGenerations <- run (Chat.getGenerations autoId)
    let
        expired :: Chat.Generation
        [expired] = filter (\g -> g.id == lostId) recoveredGenerations
    assert
        (case expired.phase of Chat.Failed -> True; _ -> False)
        "Expired generation is visibly failed"
    finalCount <- readIORef calls
    assert (finalCount == 3) "Recovery does not repeat the uncertain API call"
    putStrLn "Checking memory selection under history pressure"
    promptId <- run (Chat.createConversation "Prompt budget integration" personId)
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
        queued <-
            run
                ( Chat.requestTurn
                    promptId
                    (Revision.Revision (2 * n))
                    personId
                    ( MessageContent.MessageContent
                        ("Older question " <> T.pack (show n) <> T.replicate 1000 "é")
                    )
                )
        run (Chat.claimGeneration queued 180)
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
    promptRows <- run Chat.getConversations
    let
        promptConversation :: Chat.Conversation
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
        context :: T.Text
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
                promptPerson.person.identity
                promptPerson.person.aspirations
            )
    cancellationId <-
        run (Chat.createConversation "Worker cancellation integration" personId)
    cancelledTurn <-
        run (Chat.requestTurn cancellationId 0 personId "Please stop this turn.")
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
    stoppedPhase <- run (Chat.getGenerationPhase cancelledTurn)
    assert
        (case stoppedPhase of Just Chat.Cancelled -> True; _ -> False)
        "Worker observes cancelled phase"
    cancellationMessages <- run (Chat.getMessages cancellationId)
    cancellationNotes <- run (Chat.getNoteRevisions cancellationId)
    assert
        (length cancellationMessages == 1 && null cancellationNotes)
        "Cancelled model output creates no message or note"
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
        ada :: Person.Person
        [ada] = filter (\p -> p.name == "Ada") seededPeople
        sam :: Person.Person
        [sam] = filter (\p -> p.name == "Sam") seededPeople
    assert
        (not (T.null ada.identity) && not (T.null sam.identity))
        "Fixture AIs have usable identities"
    adaGoals <- run (Goal.getGoals ada.id)
    adaMemories <- run (Memory.getMemories ada.id)
    assert
        (length adaGoals == 1 && length adaMemories == 1)
        "Fixture goals and memories are seeded once"
    seededConversations <- run Chat.getConversations
    let
        starter :: Chat.Conversation
        [starter] = filter (\c -> c.title == "Long-running conversation") seededConversations
    starterParticipants <- run (Chat.getParticipants starter.id)
    assert
        ( length starterParticipants == 2
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
                ada.aspirations
            )
    seed
    Just preservedAda <- run (Person.loadPersonPage ada.id)
    assert
        (preservedAda.person.identity == "Identity changed during this session.")
        "Reseeding must preserve accumulated edits"
    reseededPeople <- run Person.getAllPersons
    reseededConversations <- run Chat.getConversations
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
                ada.identity
                ada.aspirations
            )
    putStrLn
        "PASS: lifecycle, prompt selection, active cancellation, and \
        \duplicate-safe session fixtures."
