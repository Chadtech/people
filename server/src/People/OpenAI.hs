{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module People.OpenAI
    ( Client
    , Outcome (..)
    , ToolCall (..)
    , Action (..)
    , ActionError (..)
    , actionErrorText
    , connect
    , requestValue
    , generate
    , parseResponse
    , parseAction
    ) where

import GoalDescription (GoalDescription)
import qualified GoalDescription
import MemoryContent (MemoryContent)
import qualified MemoryContent
import MessageContent (MessageContent)
import qualified MessageContent
import People.DomainInstances ()

import Control.Exception (try)
import Control.Monad (unless)
import Data.Aeson
    ( Value (..)
    , eitherDecode
    , eitherDecodeStrict
    , encode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import GoalId (GoalId)
import qualified GoalId
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified People.Prompt as Prompt
import Text.Read (readMaybe)


-- Never derive Show: the client contains the secret Authorization header.
data Client = Client Manager Request Text


-- Keep provider output intact for stateless continuation (including reasoning).
data Outcome = Outcome
    { reply :: MessageContent
    , toolCalls :: [ToolCall]
    , outputItems :: [Value]
    }
    deriving (Eq, Show)


data ToolCall = ToolCall
    { callId :: Text
    , toolName :: Text
    , -- The wire format is JSON encoded inside a string. Preserve it for exact
      -- call-ID reuse checks and recoverable argument errors in parseAction.
      arguments :: Text
    }
    deriving (Eq, Show)


data Action
    = CreateGoal GoalDescription
    | CompleteGoal GoalId
    | SaveMemory MemoryContent
    deriving (Eq, Show)


data ActionError
    = InvalidArgumentsJson
    | ArgumentsNotObject
    | UnexpectedArgumentCount
    | MissingArgument Text
    | ArgumentNotText Text
    | InvalidArgumentLength Text Int
    | InvalidCompletedGoalId
    | UnknownTool
    deriving (Eq, Show)


actionErrorText :: ActionError -> Text
actionErrorText problem =
    case problem of
        InvalidArgumentsJson ->
            "Tool arguments must be valid JSON."

        ArgumentsNotObject ->
            "Tool arguments must be a JSON object."

        UnexpectedArgumentCount ->
            "Expected exactly one argument."

        MissingArgument name ->
            "Missing tool argument: " <> name <> "."

        ArgumentNotText name ->
            "Tool argument must be a string: " <> name <> "."

        InvalidArgumentLength name limit ->
            "Tool argument "
                <> name
                <> " must contain between 1 and "
                <> T.pack (show limit)
                <> " characters."

        InvalidCompletedGoalId ->
            "Invalid completed goal ID."

        UnknownTool ->
            "Unknown tool. Use create_goal, complete_goal, or save_memory."


connect :: String -> Text -> IO Client
connect apiKey model =
    do
        unless
            (not (null apiKey) && not (T.null (T.strip model)))
            ( ioError
                (userError "Set OPENAI_API_KEY and OPENAI_MODEL in the worker environment.")
            )
        manager <- newManager tlsManagerSettings
        request <- parseRequest "https://api.openai.com/v1/responses"
        pure
            ( Client
                manager
                request
                    { method = "POST"
                    , requestHeaders =
                        [ ("Authorization", BS.pack ("Bearer " ++ apiKey))
                        , ("Content-Type", "application/json")
                        ]
                    , responseTimeout = responseTimeoutMicro 120000000
                    , redirectCount = 0
                    }
                model
            )


requestValue :: Client -> Prompt.Prompt -> [Value] -> Value
requestValue (Client _ _ model) prompt history =
    object
        [ "model" .= model
        , "store" .= False
        , "include" .= (["reasoning.encrypted_content"] :: [Text])
        , "max_output_tokens" .= (2048 :: Int)
        , "instructions" .= Prompt.instructions prompt
        , "input"
            .= ( object ["role" .= ("user" :: Text), "content" .= Prompt.context prompt]
                    : history
               )
        , "parallel_tool_calls" .= False
        , "tools"
            .= [ tool "create_goal" "Create one specific goal for yourself." "description" 1000
               , tool
                    "complete_goal"
                    "Complete one of your own active goals using its decimal ID."
                    "goal_id"
                    20
               , tool "save_memory" "Save a useful recollection for yourself." "content" 2000
               ]
        ]
    where
        tool :: Text -> Text -> Text -> Int -> Value
        tool name description field limit =
            object
                [ "type" .= ("function" :: Text)
                , "name" .= name
                , "description" .= description
                , "strict" .= True
                , "parameters"
                    .= object
                        [ "type" .= ("object" :: Text)
                        , "additionalProperties" .= False
                        , "required" .= [field]
                        , "properties"
                            .= object
                                [
                                    ( Key.fromText field
                                    , object
                                        [ "type" .= ("string" :: Text)
                                        , "minLength" .= (1 :: Int)
                                        , "maxLength" .= limit
                                        ]
                                    )
                                ]
                        ]
                ]


generate :: Client -> Prompt.Prompt -> [Value] -> IO Outcome
generate client@(Client manager request _) prompt history =
    do
        -- Do not render HttpException: it includes request headers and credentials.
        result <-
            try
                ( httpLbs
                    request
                        { requestBody = RequestBodyLBS (encode (requestValue client prompt history))
                        }
                    manager
                )
        response <- case result of
            Left (_ :: HttpException) ->
                ioError
                    ( userError
                        "OpenAI connection failed or timed out. No final reply was saved; earlier tool actions may remain saved."
                    )

            Right value ->
                pure value
        unless
            (statusCode (responseStatus response) == 200)
            ( ioError
                ( userError
                    ( "OpenAI returned HTTP "
                        ++ show (statusCode (responseStatus response))
                        ++ ". Check model access, quota, and worker configuration."
                    )
                )
            )
        either (ioError . userError) pure (parseResponse (responseBody response))


parseResponse :: Lazy.ByteString -> Either String Outcome
parseResponse body =
    do
        value <-
            either (const (Left "OpenAI returned invalid JSON.")) Right (eitherDecode body)
        parseEither parseEnvelope value


parseEnvelope :: Value -> Parser Outcome
parseEnvelope =
    withObject "response" $ \o -> do
        status <- o .: "status" :: Parser Text
        unless
            (status == "completed")
            ( fail "OpenAI response was not completed; earlier tool actions may remain saved."
            )
        output <- o .: "output" :: Parser [Value]
        pieces <- mapM parseItem output
        let
            calls =
                concatMap fst pieces
            response =
                T.strip (T.intercalate "\n" (concatMap snd pieces))
            ids =
                map callId calls
        unless
            (length ids == length (nub ids))
            (fail "Duplicate tool call IDs in response.")
        unless
            (T.length response <= 12000 && (not (T.null response) || not (null calls)))
            (fail "OpenAI response had no reply or tools, or exceeded the reply limit.")
        pure (Outcome (MessageContent.MessageContent response) calls output)


parseItem :: Value -> Parser ([ToolCall], [Text])
parseItem =
    withObject "output item" $ \o -> do
        kind <- o .: "type" :: Parser Text
        case kind of
            "reasoning" -> pure ([], [])

            "function_call" -> do
                identifier <- o .: "call_id"
                name <- o .: "name"
                args <- o .: "arguments"
                unless
                    ( not (T.null identifier)
                        && T.length identifier <= 256
                        && T.length name <= 100
                        && T.length args <= 16000
                    )
                    (fail "Invalid tool call envelope.")
                pure ([ToolCall identifier name args], [])

            "message" -> do
                content <- o .: "content" :: Parser [Value]
                texts <-
                    mapM
                        ( withObject "content" $ \c -> do
                            contentType <- c .: "type" :: Parser Text
                            if contentType == "output_text"
                                then c .: "text"
                                else fail "OpenAI refused or returned unsupported content."
                        )
                        content
                pure ([], texts)

            _ ->
                fail "OpenAI returned an unsupported output item."


-- Invalid arguments become tool errors, allowing the person to correct them.
-- The raw arguments never determine the generation or person being mutated.
parseAction :: ToolCall -> Either ActionError Action
parseAction call =
    do
        value <-
            either
                (const (Left InvalidArgumentsJson))
                Right
                (eitherDecodeStrict (Text.encodeUtf8 (arguments call)))
        fields <- case value of
            Object o ->
                Right o

            _ ->
                Left ArgumentsNotObject
        unless (KeyMap.size fields == 1) (Left UnexpectedArgumentCount)
        let
            field :: Key.Key -> Int -> Either ActionError Text
            field key limit =
                do
                    valueText <- case KeyMap.lookup key fields of
                        Nothing ->
                            Left (MissingArgument (Key.toText key))

                        Just (String text) ->
                            Right (T.strip text)

                        Just _ ->
                            Left (ArgumentNotText (Key.toText key))
                    unless
                        (not (T.null valueText) && T.length valueText <= limit)
                        (Left (InvalidArgumentLength (Key.toText key) limit))
                    pure valueText
        case toolName call of
            "create_goal" ->
                CreateGoal . GoalDescription.GoalDescription <$> field "description" 1000

            "save_memory" ->
                SaveMemory . MemoryContent.MemoryContent <$> field "content" 2000

            "complete_goal" -> do
                completed <- field "goal_id" 20
                case readMaybe (T.unpack completed) :: Maybe Integer of
                    Just n
                        | T.all (\c -> c >= '0' && c <= '9') completed
                            && n > 0
                            && n <= toInteger (maxBound :: Word64) ->
                            pure (CompleteGoal (GoalId.GoalId (fromInteger n)))

                    _ ->
                        Left InvalidCompletedGoalId

            _ ->
                Left UnknownTool
