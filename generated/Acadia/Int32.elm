module Acadia.Int32 exposing (Int32, toInt, fromInt, toString, encodeLE, encodeBE, decodeLE, decodeBE)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E

type Int32 = Int32 Int

toInt : Int32 -> Int
toInt (Int32 n) = n

fromInt : Int -> Int32
fromInt n =
  Int32 (Bitwise.or n 0)

toString : Int32 -> String
toString (Int32 n) = String.fromInt n

encodeLE : Int32 -> E.Encoder
encodeLE (Int32 n) = E.signedInt32 LE n

encodeBE : Int32 -> E.Encoder
encodeBE (Int32 n) = E.signedInt32 BE n

decodeLE : D.Decoder Int32
decodeLE = D.map Int32 (D.signedInt32 LE)

decodeBE : D.Decoder Int32
decodeBE = D.map Int32 (D.signedInt32 BE)
