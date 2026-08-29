module Acadia.UInt8 exposing (UInt8, toInt, fromInt, toString, encode, decode)

import Bitwise
import Bytes.Decode as D
import Bytes.Encode as E

type UInt8 = UInt8 Int

toInt : UInt8 -> Int
toInt (UInt8 n) = n

fromInt : Int -> UInt8
fromInt n =
  UInt8 (Bitwise.and n 0xFF)

toString : UInt8 -> String
toString (UInt8 n) = String.fromInt n

encode : UInt8 -> E.Encoder
encode (UInt8 n) = E.unsignedInt8 n

decode : D.Decoder UInt8
decode = D.map UInt8 D.unsignedInt8

