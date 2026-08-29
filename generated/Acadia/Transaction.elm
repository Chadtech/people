module Acadia.Transaction exposing (Transaction(..), attempt)

import Bytes.Decode as D
import Bytes.Encode as E
import Http


type Transaction a =
  Transaction E.Encoder (D.Decoder a)


attempt : String -> (Maybe a -> msg) -> Transaction a -> Cmd msg
attempt url toMsg (Transaction encoder decoder) =
  Http.post
    { url = url
    , body = Http.bytesBody "application/octet-stream" (E.encode encoder)
    , expect = Http.expectBytes (toMsg << Result.toMaybe) decoder
    }
