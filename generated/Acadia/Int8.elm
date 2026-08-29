module Acadia.Int8 exposing (Int8, toInt, fromInt, toString, encode, decode)

import Bitwise
import Bytes.Decode as D
import Bytes.Encode as E

type Int8 = Int8 Int

toInt : Int8 -> Int
toInt (Int8 n) = n

fromInt : Int -> Int8
fromInt n =
  n
    |> Bitwise.shiftLeftBy 24
    |> Bitwise.shiftRightBy 24
    |> Int8

toString : Int8 -> String
toString (Int8 n) = String.fromInt n

encode : Int8 -> E.Encoder
encode (Int8 n) = E.signedInt8 n

decode : D.Decoder Int8
decode = D.map Int8 D.signedInt8
