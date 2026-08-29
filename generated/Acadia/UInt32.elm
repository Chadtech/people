module Acadia.UInt32 exposing (UInt32, toInt, fromInt, toString, encodeLE, encodeBE, decodeLE, decodeBE)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E

type UInt32 = UInt32 Int

toInt : UInt32 -> Int
toInt (UInt32 n) = n

fromInt : Int -> UInt32
fromInt n =
  UInt32 (Bitwise.and n 0xFFFFFFFF)

toString : UInt32 -> String
toString (UInt32 n) = String.fromInt (if n < 0 then 0xFFFFFFFF + (n + 1) else n)

encodeLE : UInt32 -> E.Encoder
encodeLE (UInt32 n) = E.unsignedInt32 LE n

encodeBE : UInt32 -> E.Encoder
encodeBE (UInt32 n) = E.unsignedInt32 BE n

decodeLE : D.Decoder UInt32
decodeLE = D.map UInt32 (D.unsignedInt32 LE)

decodeBE : D.Decoder UInt32
decodeBE = D.map UInt32 (D.unsignedInt32 BE)

