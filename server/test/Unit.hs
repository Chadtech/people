{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import People.DomainInstances ()

import Control.Monad (forM_, unless)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as Lazy
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import qualified GoalId
import qualified People.OpenAI as OpenAI


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
