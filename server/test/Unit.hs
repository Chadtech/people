{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import People.DomainInstances ()

import Control.Monad (forM_, unless)
import Data.Aeson (Value, encode, toJSON, object, (.=))
import qualified Data.ByteString.Lazy as Lazy
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import qualified GoalId
import qualified People.Prompt as Prompt
import qualified People.OpenAI as OpenAI
import qualified People.Environment as Environment


assert :: Bool -> String -> IO ()
assert condition message = unless condition (ioError (userError message))


turn :: T.Text -> T.Text -> Value
turn reply goalId =
    object
        [ "reply" .= reply
        , "new_goal" .= ("" :: T.Text)
        , "reflection" .= ("" :: T.Text)
        , "complete_goal" .= goalId
        , "shared_note" .= ("" :: T.Text)
        ]


envelope :: T.Text -> Value -> Lazy.ByteString
envelope status result =
    encode
        ( object
            [ "status" .= status
            , "output"
                .= [ object
                        [ "type" .= ("message" :: T.Text)
                        , "content"
                            .= [ object
                                    [ "type" .= ("output_text" :: T.Text)
                                    , "text" .= Text.decodeUtf8 (Lazy.toStrict (encode result))
                                    ]
                               ]
                        ]
                   ]
            ]
        )


main :: IO ()
main = do
    assert
        ( toJSON
            (Prompt.PromptSelection
                [Prompt.SelectionBlock "identity" "person" "Example" 27]
                [Prompt.SelectionBlock "memory:1" "retired" "Old" 26]
                24000
                6000)
            == object
                [ "included" .= [object
                    [ "source" .= ("identity" :: T.Text)
                    , "reason" .= ("person" :: T.Text)
                    , "text" .= ("Example" :: T.Text)
                    , "bytes" .= (27 :: Int)
                    ]]
                , "omitted" .= [object
                    [ "source" .= ("memory:1" :: T.Text)
                    , "reason" .= ("retired" :: T.Text)
                    , "text" .= ("Old" :: T.Text)
                    , "bytes" .= (26 :: Int)
                    ]]
                , "budget_unit" .= ("UTF-8 bytes, not tokenizer counts" :: T.Text)
                , "context_budget" .= (24000 :: Int)
                , "memory_allowance" .= (6000 :: Int)
                ]
        )
        "Typed prompt selection changed the saved snapshot format"
    assert
        (Environment.parseFile "# comment\r\nexport KEY = 'value # literal'\r\nMODEL=example # comment\nEMPTY=\nLITERAL=$(echo value)\nQUOTED=\"a=b\"\n"
            == Right [("KEY", "value # literal"), ("MODEL", "example"), ("EMPTY", ""), ("LITERAL", "$(echo value)"), ("QUOTED", "a=b")])
        "Environment quoting, comments, CRLF, or literal values changed"
    forM_ ["KEY='unclosed", "KEY=\"value\"garbage", "1KEY=value", "missing assignment"] $ \invalid ->
        assert (Environment.parseFile ("# comment\n" ++ invalid) == Left 2)
            "Malformed environment assignment was accepted"
    assert
        ( OpenAI.parseResponse (envelope "completed" (turn "Hello" ""))
            == Right (OpenAI.Outcome "Hello" "" "" Nothing "")
        )
        "Valid response without actions"
    assert
        ( OpenAI.parseResponse (envelope "completed" (turn "Done" "42"))
            == Right (OpenAI.Outcome "Done" "" "" (Just (GoalId.GoalId 42)) "")
        )
        "Valid goal completion"
    forM_
        [ "-1"
        , "0"
        , "18446744073709551616"
        , "18446744073709551617"
        , "+1"
        , "1.0"
        , "1e3"
        , "goal:1"
        ]
        $ \goalId ->
            assert
                (isLeft (OpenAI.parseResponse (envelope "completed" (turn "Done" goalId))))
                "Malformed or overflowing goal ID was accepted"
    forM_ ["incomplete", "failed", "cancelled", "in_progress"] $ \status ->
        assert
            (isLeft (OpenAI.parseResponse (envelope status (turn "Partial reply" "42"))))
            "Unfinished response was accepted"
    assert
        (isLeft (OpenAI.parseResponse (envelope "completed" (turn "   " ""))))
        "Empty reply was accepted"
    assert
        ( isLeft
            (OpenAI.parseResponse (envelope "completed" (turn (T.replicate 12001 "x") "")))
        )
        "Oversized reply was accepted"
    assert
        (isLeft (OpenAI.parseResponse "not JSON"))
        "Malformed envelope was accepted"
    assert
        ( isLeft
            ( OpenAI.parseResponse
                (envelope "completed" (object ["reply" .= ("Hello" :: T.Text)]))
            )
        )
        "Missing fields were accepted"
    let
        refusal :: Lazy.ByteString
        refusal =
            encode
                ( object
                    [ "status" .= ("completed" :: T.Text)
                    , "output"
                        .= [ object
                                [ "type" .= ("message" :: T.Text)
                                , "content"
                                    .= [object ["type" .= ("refusal" :: T.Text), "refusal" .= ("No" :: T.Text)]]
                                ]
                           ]
                    ]
                )
    assert
        (isLeft (OpenAI.parseResponse refusal))
        "Refusal was accepted as an action"
    putStrLn
        "PASS: OpenAI structured responses, incomplete/refused outputs, \
        \field validation, output limits, and goal ID bounds."
