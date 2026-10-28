{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import People.DomainInstances ()

import Control.Monad (forM_, unless)
import Data.Aeson (ToJSON, Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as Lazy
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import qualified GenerationId
import qualified GoalId
import qualified People.Environment as Environment
import qualified People.OpenAI as OpenAI
import qualified People.Prompt as Prompt
import qualified People.Cognition as Cognition
import qualified People.SelectionReason as SelectionReason


assert :: Bool -> String -> IO ()
assert condition message =
    unless condition (ioError (userError message))


envelope :: T.Text -> [Value] -> Lazy.ByteString
envelope status output =
    encode (object ["status" .= status, "output" .= output])


message :: T.Text -> Value
message value =
    object
        [ "type" .= ("message" :: T.Text)
        , "content" .= [object ["type" .= ("output_text" :: T.Text), "text" .= value]]
        ]


call :: T.Text -> T.Text -> Value
call name args =
    object
        [ "type" .= ("function_call" :: T.Text)
        , "call_id" .= ("call_1" :: T.Text)
        , "name" .= name
        , "arguments" .= args
        ]


main :: IO ()
main =
    do
        checkCognition
        assert
            ( toJSON
                ( Prompt.PromptSelection
                    [Prompt.SelectionBlock "identity" SelectionReason.PersonIdentity "Example" 27]
                    [Prompt.SelectionBlock "memory:1" SelectionReason.RetiredMemory "Old" 26]
                    24000
                    6000
                )
                == object
                    [ "included"
                        .= [ object
                                [ "source" .= ("identity" :: T.Text)
                                , "reason" .= ("person" :: T.Text)
                                , "text" .= ("Example" :: T.Text)
                                , "bytes" .= (27 :: Int)
                                ]
                           ]
                    , "omitted"
                        .= [ object
                                [ "source" .= ("memory:1" :: T.Text)
                                , "reason" .= ("retired" :: T.Text)
                                , "text" .= ("Old" :: T.Text)
                                , "bytes" .= (26 :: Int)
                                ]
                           ]
                    , "budget_unit" .= ("UTF-8 bytes, not tokenizer counts" :: T.Text)
                    , "context_budget" .= (24000 :: Int)
                    , "memory_allowance" .= (6000 :: Int)
                    ]
            )
            "Typed prompt selection changed the saved snapshot format"
        forM_
            [ (SelectionReason.PersonIdentity, "person")
            , (SelectionReason.ConversationParticipants, "conversation")
            , (SelectionReason.RecentConversation, "recent conversation")
            , (SelectionReason.ActiveGoal, "active goal")
            , (SelectionReason.CuratedContext, "curated context")
            ,
                ( SelectionReason.ModelReflection (GenerationId.GenerationId 42)
                , "model reflection from generation 42; may be mistaken"
                )
            , (SelectionReason.RetiredMemory, "retired")
            , (SelectionReason.KeywordsDidNotMatch, "keywords did not match")
            , (SelectionReason.ContextBudgetExceeded, "excluded by context byte budget")
            ]
            $ \(selectionReason, expected) ->
                assert
                    ( toJSON (Prompt.SelectionBlock "source" selectionReason "text" 0)
                        == object
                            [ "source" .= ("source" :: T.Text)
                            , "reason" .= (expected :: T.Text)
                            , "text" .= ("text" :: T.Text)
                            , "bytes" .= (0 :: Int)
                            ]
                    )
                    "Selection reason changed the saved inspection text"
        assert
            ( Environment.parseFile
                "# comment\r\nexport KEY = 'value # literal'\r\nMODEL=example # comment\nEMPTY=\nLITERAL=$(echo value)\nQUOTED=\"a=b\"\n"
                == Right
                    [ ("KEY", "value # literal")
                    , ("MODEL", "example")
                    , ("EMPTY", "")
                    , ("LITERAL", "$(echo value)")
                    , ("QUOTED", "a=b")
                    ]
            )
            "Environment quoting, comments, CRLF, or literal values changed"
        forM_
            [ "KEY='unclosed"
            , "KEY=\"value\"garbage"
            , "1KEY=value"
            , "missing assignment"
            ]
            $ \invalid ->
                assert
                    (Environment.parseFile ("# comment\n" ++ invalid) == Left 2)
                    "Malformed environment assignment was accepted"
        let
            text = message "Hello"
            tool =
                call "complete_goal" "{\"goal_id\":\"42\"}"
            reasoning =
                object
                    ["type" .= ("reasoning" :: T.Text), "encrypted_content" .= ("opaque" :: T.Text)]
        assert
            ( OpenAI.parseResponse (envelope "completed" [text])
                == Right (OpenAI.Outcome "Hello" [] [text])
            )
            "Plain text reply is preserved without JSON decoding"
        assert
            ( OpenAI.parseResponse (envelope "completed" [reasoning, tool])
                == Right
                    ( OpenAI.Outcome
                        ""
                        [OpenAI.ToolCall "call_1" "complete_goal" "{\"goal_id\":\"42\"}"]
                        [reasoning, tool]
                    )
            )
            "Tool-only responses preserve reasoning and call IDs"
        assert
            ( OpenAI.parseAction (OpenAI.ToolCall "c" "complete_goal" "{\"goal_id\":\"42\"}")
                == Right (OpenAI.CompleteGoal (GoalId.GoalId 42))
            )
            "Valid goal completion"
        forM_
            [ ("-1" :: T.Text)
            , "0"
            , "18446744073709551616"
            , "+1"
            , "1.0"
            , "1e3"
            , "goal:1"
            ]
            $ \identifier ->
                assert
                    ( isLeft
                        ( OpenAI.parseAction
                            ( OpenAI.ToolCall
                                "c"
                                "complete_goal"
                                (Text.decodeUtf8 (Lazy.toStrict (encode (object ["goal_id" .= identifier]))))
                            )
                        )
                    )
                    "Malformed or overflowing goal ID was accepted"
        forM_ ["incomplete", "failed", "cancelled", "in_progress"] $ \status ->
            assert
                (isLeft (OpenAI.parseResponse (envelope status [tool])))
                "Unfinished response must not execute tools"
        forM_ [[], [message "   "], [message (T.replicate 12001 "x")], [tool, tool]] $ \items ->
            assert
                (isLeft (OpenAI.parseResponse (envelope "completed" items)))
                "Empty, oversized, or duplicate-ID response was accepted"
        forM_
            [ ("unknown", "{\"value\":\"x\"}", OpenAI.UnknownTool)
            , ("create_goal", "not JSON", OpenAI.InvalidArgumentsJson)
            , ("create_goal", "[]", OpenAI.ArgumentsNotObject)
            ,
                ( "create_goal"
                , "{\"description\":\" \"}"
                , OpenAI.InvalidArgumentLength "description" 1000
                )
            ,
                ( "create_goal"
                , "{\"description\":\"ok\",\"person_id\":\"2\"}"
                , OpenAI.UnexpectedArgumentCount
                )
            ,
                ( "save_memory"
                , "{\"description\":\"wrong field\"}"
                , OpenAI.MissingArgument "content"
                )
            , ("save_memory", "{\"content\":42}", OpenAI.ArgumentNotText "content")
            ,
                ( "save_memory"
                , Text.decodeUtf8
                    ( Lazy.toStrict
                        (encode (object ["content" .= T.replicate 2001 "x"]))
                    )
                , OpenAI.InvalidArgumentLength "content" 2000
                )
            , ("complete_goal", "{\"goal_id\":\"0\"}", OpenAI.InvalidCompletedGoalId)
            ]
            $ \(name, args, expected) -> do
                let
                    invalid = OpenAI.ToolCall "call_1" name args
                    wireCall =
                        call name args
                assert
                    (OpenAI.parseAction invalid == Left expected)
                    "Invalid arguments returned the wrong typed error"
                assert
                    ( OpenAI.parseResponse (envelope "completed" [wireCall])
                        == Right (OpenAI.Outcome "" [invalid] [wireCall])
                    )
                    "Invalid arguments must remain recoverable tool errors"
                assert
                    (not (T.null (OpenAI.actionErrorText expected)))
                    "Tool errors need a provider-facing explanation"
        assert
            ( OpenAI.parseAction
                (OpenAI.ToolCall "c" "create_goal" "{\"description\":\" Study rhythm \"}")
                == Right (OpenAI.CreateGoal "Study rhythm")
            )
            "Goal text trimmed"
        assert
            ( OpenAI.parseAction
                (OpenAI.ToolCall "c" "save_memory" "{\"content\":\"Useful detail\"}")
                == Right (OpenAI.SaveMemory "Useful detail")
            )
            "Valid memory accepted"
        assert
            (isLeft (OpenAI.parseResponse "not JSON"))
            "Malformed envelope was accepted"
        let
            refusal =
                object
                    [ "type" .= ("message" :: T.Text)
                    , "content"
                        .= [object ["type" .= ("refusal" :: T.Text), "refusal" .= ("No" :: T.Text)]]
                    ]
        assert
            (isLeft (OpenAI.parseResponse (envelope "completed" [tool, refusal])))
            "Refused response must not execute even accompanying tools"
        client <- OpenAI.connect "test-key-never-sent" "test-model"
        let
            prompt =
                Prompt.Prompt
                    Prompt.Narration
                    "Speak plainly."
                    "Context"
                    (Prompt.PromptSelection [] [] 24000 6000)
            toolResult =
                object
                    [ "type" .= ("function_call_output" :: T.Text)
                    , "call_id" .= ("call_1" :: T.Text)
                    , "output" .= ("success" :: T.Text)
                    ]
            history =
                [reasoning, tool, toolResult]
        case OpenAI.requestValue client prompt history of
            Object request ->
                do
                    assert
                        (KeyMap.lookup "text" request == Nothing)
                        "Replies must not use structured JSON"
                    assert
                        (KeyMap.lookup "store" request == Just (Bool False))
                        "Provider storage stays disabled"
                    assert
                        ( KeyMap.lookup "input" request
                            == Just
                                ( toJSON
                                    ( object ["role" .= ("user" :: T.Text), "content" .= ("Context" :: T.Text)]
                                        : history
                                    )
                                )
                        )
                        "Continuation replays exact ordered output and tool results"
                    assert
                        ( not
                            ( T.isInfixOf
                                "test-key-never-sent"
                                (Text.decodeUtf8 (Lazy.toStrict (encode request)))
                            )
                        )
                        "Saved request never includes credentials"

            _ ->
                ioError (userError "Request must be an object")
        putStrLn
            "PASS: plain replies, tool calls, reasoning replay, validation, refusals, and ID bounds."


checkCognition :: IO ()
checkCognition =
    do
        let
            proposal action support =
                Cognition.Proposal action Cognition.Uneasy 50 support
                    "PRIVATE_EVIDENCE" "PRIVATE_MOTIVE"
            assessment =
                Cognition.Assessment
                    [ proposal Cognition.Connect (Cognition.Drives 100 0 0)
                    , proposal Cognition.Investigate (Cognition.Drives 0 100 0)
                    ]
                    "PRIVATE_UNCERTAINTY" (Cognition.Drives 1 0 (-1))
            encodeText :: ToJSON a => a -> T.Text
            encodeText = Text.decodeUtf8 . Lazy.toStrict . encode
            base = Prompt.Prompt Prompt.Narration "Speak plainly."
                "Morgan: What evidence supports this?"
                (Prompt.PromptSelection [] [] 24000 6000)
            observer = Cognition.appraisalPrompt Cognition.initialDrives base
            projection = Cognition.select (Cognition.Drives 0 100 0) assessment
        assert (Cognition.parseAssessment (encodeText assessment) == Right assessment)
            "Valid appraisal round trips"
        assert (projection == Just (Cognition.Projection Cognition.Investigate Cognition.Uneasy))
            "Drive weights change action selection"
        assert (Cognition.select (Cognition.Drives 100 0 0) assessment
            == Just (Cognition.Projection Cognition.Connect Cognition.Uneasy))
            "Affiliation favors connection"
        forM_
            [ "not JSON"
            , encodeText (assessment { Cognition.proposals = [] })
            , encodeText (assessment { Cognition.changes = Cognition.Drives 2 0 0 })
            , encodeText (assessment { Cognition.uncertainty = "" })
            , encodeText (assessment { Cognition.proposals =
                [proposal Cognition.Answer (Cognition.Drives 101 0 0),
                 proposal Cognition.Connect Cognition.initialDrives] })
            ] $ \invalid ->
                assert (isLeft (Cognition.parseAssessment invalid))
                    "Malformed and unbounded appraisals must fail"
        assert (Cognition.select Cognition.initialDrives
            (assessment { Cognition.proposals = [] }) == Nothing)
            "Empty appraisal selection is total"
        client <- OpenAI.connect "test-key-never-sent" "test-model"
        assert
            (OpenAI.requestValue client observer [object ["private" .= True]]
                == OpenAI.requestValue client observer [])
            "Appraisal request builder cannot replay another stage's history"
        case OpenAI.requestValue client observer [] of
            Object request -> do
                assert (KeyMap.lookup "tools" request == Just (toJSON ([] :: [Value])))
                    "Appraisal has no action tools"
                assert (KeyMap.member "text" request)
                    "Appraisal requests a structured output schema"

            _ -> ioError (userError "Appraisal request must be an object")
        case projection of
            Nothing -> ioError (userError "Missing test projection")

            Just selected -> do
                let
                    narrator = Cognition.narratorPrompt selected base
                    request = encodeText (OpenAI.requestValue client narrator [])
                forM_ ["PRIVATE_EVIDENCE", "PRIVATE_MOTIVE", "PRIVATE_UNCERTAINTY", "Private drive state"] $ \secret ->
                    assert (not (T.isInfixOf secret request))
                        "Private appraisal material leaked into narration"
                assert (T.isInfixOf "investigate" request && T.isInfixOf "uneasy" request)
                    "Narrator receives selected action and coarse feeling"
                assert (Prompt.context base `T.isInfixOf` Prompt.context narrator)
                    "Narrator retains ordinary conversation context"
