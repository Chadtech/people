module Acadia.Time exposing (Posix, toMicroseconds, fromMicroseconds, encodeLE, encodeBE, decodeLE, decodeBE)

import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E
import Acadia.Int64 as Int64 exposing (Int64)



-- POSIX

type Posix = Posix Int64


-- POSTGRES MICROSECONDS

toMicroseconds : Posix -> Int64
toMicroseconds (Posix t) =
  t

fromMicroseconds : Int64 -> Posix
fromMicroseconds us =
  Posix us


-- BINARY

encodeLE : Posix -> E.Encoder
encodeLE (Posix t) = Int64.encodeLE t

encodeBE : Posix -> E.Encoder
encodeBE (Posix t) = Int64.encodeBE t

decodeLE : D.Decoder Posix
decodeLE = D.map Posix Int64.decodeLE

decodeBE : D.Decoder Posix
decodeBE = D.map Posix Int64.decodeBE
