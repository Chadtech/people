module Acadia.UInt16 exposing (UInt16, toInt, fromInt, toString, encodeLE, encodeBE, decodeLE, decodeBE)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E

type UInt16 = UInt16 Int

toInt : UInt16 -> Int
toInt (UInt16 n) = n

fromInt : Int -> UInt16
fromInt n =
  UInt16 (Bitwise.and n 0xFFFF)

toString : UInt16 -> String
toString (UInt16 n) = String.fromInt n

encodeLE : UInt16 -> E.Encoder
encodeLE (UInt16 n) = E.unsignedInt16 LE n

encodeBE : UInt16 -> E.Encoder
encodeBE (UInt16 n) = E.unsignedInt16 BE n

decodeLE : D.Decoder UInt16
decodeLE = D.map UInt16 (D.unsignedInt16 LE)

decodeBE : D.Decoder UInt16
decodeBE = D.map UInt16 (D.unsignedInt16 BE)

