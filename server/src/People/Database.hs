{-# LANGUAGE OverloadedStrings #-}

module People.Database (Database, Transaction, connect, runTransaction) where

import People.DomainInstances ()

import qualified Acadia.Bytes.Decode as Decode
import Acadia.Transaction (Transaction (..))
import Control.Monad (unless)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as Lazy
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)


-- All domain data goes through generated Acadia endpoints.
-- The worker has no independent file or SQL persistence path.
data Database = Database Manager Request


connect :: String -> IO Database
connect baseUrl = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest (baseUrl ++ "/_endpoints")
    pure
        ( Database
            manager
            request
                { method = "POST"
                , requestHeaders =
                    [ ("Content-Type", "application/octet-stream")
                    , ("Accept", "application/octet-stream")
                    , ("Origin", BS.pack baseUrl)
                    ]
                , responseTimeout = responseTimeoutMicro 10000000
                , redirectCount = 0
                }
        )


runTransaction :: Database -> Transaction a -> IO a
runTransaction (Database manager request) (Transaction encoder decoder) = do
    response <-
        httpLbs
            request
                { requestBody = RequestBodyLBS (Builder.toLazyByteString encoder)
                }
            manager
    unless
        (statusCode (responseStatus response) == 200)
        ( ioError
            ( userError
                ( "Acadia rejected the transaction (HTTP "
                    ++ show (statusCode (responseStatus response))
                    ++ "). Refresh the saved state before retrying."
                )
            )
        )
    let
        bytes :: BS.ByteString
        bytes = Lazy.toStrict (responseBody response)
    -- Acadia 0.3.0 emits two zero row-chunk markers for an empty Rows result.
    -- Its Elm decoder accepts the extra marker; the Haskell decoder requires
    -- exact consumption. Normalize only this observed eight-zero-byte reply.
    decoded <-
        Decode.fromByteString
            decoder
            (if bytes == BS.replicate 8 '\0' then BS.take 4 bytes else bytes)
    maybe
        ( ioError
            ( userError
                "Acadia response could not be decoded; regenerate both client \
                \bindings from the served schema."
            )
        )
        pure
        decoded
