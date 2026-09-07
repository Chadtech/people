{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module People.OpenAI (Client, Outcome (..), connect, requestValue, generate, parseResponse) where

import qualified GoalDescription
import qualified MemoryContent
import qualified MessageContent
import qualified Note
import People.DomainInstances ()

import Control.Exception (try)
import Control.Monad (unless)
import Data.Aeson (
    Value,
    eitherDecode,
    eitherDecodeStrict,
    encode,
    object,
    withObject,
    (.:),
    (.=),
 )
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import qualified GoalId
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified People.Prompt as Prompt
import Text.Read (readMaybe)


-- Never derive Show: the client contains the secret Authorization header.
data Client = Client Manager Request Text


data Outcome = Outcome
    { reply :: MessageContent.MessageContent
    , newGoal :: GoalDescription.GoalDescription
    , reflection :: MemoryContent.MemoryContent
    , completeGoal :: Maybe GoalId.GoalId
    , sharedNote :: Note.Note
    }
    deriving (Eq, Show)


connect :: String -> Text -> IO Client
connect apiKey model = do
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


requestValue :: Client -> Prompt.Prompt -> Value
requestValue (Client _ _ model) prompt =
    object
        [ "model" .= model
        , "store" .= False
        , "max_output_tokens" .= (2048 :: Int)
        , "instructions" .= Prompt.instructions prompt
        , "input"
            .= [object ["role" .= ("user" :: Text), "content" .= Prompt.context prompt]]
        , "text"
            .= object
                [ "format"
                    .= object
                        [ "type" .= ("json_schema" :: Text)
                        , "name" .= ("person_turn" :: Text)
                        , "strict" .= True
                        , "schema"
                            .= object
                                [ "type" .= ("object" :: Text)
                                , "additionalProperties" .= False
                                , "required" .= fields
                                , "properties"
                                    .= object
                                        [(fromStringKey field, object ["type" .= ("string" :: Text)]) | field <- fields]
                                ]
                        ]
                ]
        ]
    where
        fields :: [Text]
        fields = ["reply", "new_goal", "reflection", "complete_goal", "shared_note"]
        fromStringKey :: Text -> Data.Aeson.Key.Key
        fromStringKey = Data.Aeson.Key.fromText


generate :: Client -> Prompt.Prompt -> IO Outcome
generate client@(Client manager request _) prompt = do
    -- Do not render HttpException: it includes request headers and credentials.
    result <-
        try
            ( httpLbs
                request{requestBody = RequestBodyLBS (encode (requestValue client prompt))}
                manager
            )
    response <- case result of
        Left (_ :: HttpException) ->
            ioError
                ( userError
                    "OpenAI connection failed or timed out. No result was saved; retry explicitly."
                )
        Right value -> pure value
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
parseResponse body = do
    value <-
        either (const (Left "OpenAI returned invalid JSON.")) Right (eitherDecode body)
    parseEither parseEnvelope value


parseEnvelope :: Value -> Parser Outcome
parseEnvelope = withObject "response" $ \o -> do
    status <- o .: "status" :: Parser Text
    unless
        (status == "completed")
        (fail "OpenAI response was not completed; no actions were applied.")
    output <- o .: "output" :: Parser [Value]
    texts <- concat <$> mapM outputTexts output
    case texts of
        [text] -> case eitherDecodeStrict (Text.encodeUtf8 text) of
            Left _ -> fail "OpenAI response did not contain a valid person-turn object."
            Right value -> parseOutcome value
        _ ->
            fail
                "OpenAI response did not contain exactly one text result (it may have refused)."


outputTexts :: Value -> Parser [Text]
outputTexts = withObject "output item" $ \o -> do
    kind <- o .: "type" :: Parser Text
    if kind /= "message"
        then pure []
        else do
            content <- o .: "content" :: Parser [Value]
            concat
                <$> mapM
                    ( withObject
                        "content"
                        ( \c -> do
                            contentType <- c .: "type" :: Parser Text
                            if contentType == "output_text"
                                then (: []) <$> c .: "text"
                                else fail "OpenAI refused or returned unsupported content."
                        )
                    )
                    content


parseOutcome :: Value -> Parser Outcome
parseOutcome = withObject "person turn" $ \o -> do
    unless
        (KeyMap.size o == 5)
        (fail "Person-turn output contains unexpected fields.")
    response <- T.strip <$> o .: "reply"
    goal <- T.strip <$> o .: "new_goal"
    memory <- T.strip <$> o .: "reflection"
    completed <- T.strip <$> o .: "complete_goal"
    note <- T.strip <$> o .: "shared_note"
    unless
        ( not (T.null response)
            && T.length response <= 12000
            && T.length goal <= 1000
            && T.length memory <= 2000
            && T.length note <= 12000
        )
        ( fail "Person-turn output exceeded the application limits or had an empty reply."
        )
    goalId <-
        if T.null completed
            then pure Nothing
            else case readMaybe (T.unpack completed) :: Maybe Integer of
                Just n
                    | T.all (\c -> c >= '0' && c <= '9') completed
                        && n > 0
                        && n <= toInteger (maxBound :: Word64) ->
                        pure (Just (GoalId.GoalId (fromInteger n)))
                _ -> fail "Invalid completed goal ID."
    pure
        ( Outcome
            (MessageContent.MessageContent response)
            (GoalDescription.GoalDescription goal)
            (MemoryContent.MemoryContent memory)
            goalId
            (Note.Note note)
        )
