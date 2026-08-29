module Acadia.Int16 exposing (Int16, toInt, fromInt, toString, encodeLE, encodeBE, decodeLE, decodeBE)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E

type Int16 = Int16 Int

toInt : Int16 -> Int
toInt (Int16 n) = n

fromInt : Int -> Int16
fromInt n =
  n
    |> Bitwise.shiftLeftBy 16
    |> Bitwise.shiftRightBy 16
    |> Int16

toString : Int16 -> String
toString (Int16 n) = String.fromInt n

encodeLE : Int16 -> E.Encoder
encodeLE (Int16 n) = E.signedInt16 LE n

encodeBE : Int16 -> E.Encoder
encodeBE (Int16 n) = E.signedInt16 BE n

decodeLE : D.Decoder Int16
decodeLE = D.map Int16 (D.signedInt16 LE)

decodeBE : D.Decoder Int16
decodeBE = D.map Int16 (D.signedInt16 BE)

