module Acadia.Int64 exposing (Int64, toInt, fromInt, toString, fromString, encodeLE, encodeBE, decodeLE, decodeBE)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E


-- INT64

type Int64 = Int64 Int Int


-- CASTS

toInt : Int64 -> Int
toInt (Int64 _ lo) = lo

fromInt : Int -> Int64
fromInt n =
  if n >= 0
  then Int64 0 (Bitwise.or n 0)
  else Int64 0xFFFFFFFF (Bitwise.or n 0)


-- TO STRING
--
-- See comment in Acadia.UInt64 for justification of this function.

toString : Int64 -> String
toString (Int64 hi lo) =
  if hi >= 0x80000000
  then "-" ++ toStringHelp (0xFFFFFFFF - hi) (0x100000000 - lo)
  else toStringHelp hi lo

toStringHelp : Int -> Int -> String
toStringHelp hi lo =
  let
    mod = modBy 1000000000
    upper = floor ((toFloat hi * 0x100000000) / 1000000000 + toFloat lo / 1000000000)
    lower =
      modBy 1000000000 <|
        modBy 1000000000 (modBy 1000000000 hi * 0x1194D800)
        +
        modBy 1000000000 lo
  in
  if upper == 0
  then String.fromInt lower
  else String.fromInt upper ++ String.padLeft 9 '0' (String.fromInt lower)


-- FROM STRING

type State = OK Int Int | ERR

fromString : String -> Maybe Int64
fromString str =
  case String.uncons str of
    Just ('-',chars) ->
      case String.foldl step (OK 0 0) chars of
        OK hi lo ->
          if hi <= 0x7FFFFFFF || (hi == 0x80000000 && lo == 0)
          then
            Just <|
              if lo == 0
              then Int64 (Bitwise.complement hi + 1) 0
              else Int64 (Bitwise.complement hi) (Bitwise.complement lo + 1)
          else
            Nothing

        ERR ->
          Nothing

    Just _ ->
      case String.foldl step (OK 0 0) str of
        OK hi lo ->
          if hi <= 0x7FFFFFFF
          then Just (Int64 hi lo)
          else Nothing

        ERR ->
          Nothing

    Nothing ->
      Nothing

step : Char -> State -> State
step char state =
  case state of
    OK hi lo ->
      if Char.isDigit char
      then carry (10 * hi) ((10 * lo) + (Char.toCode char - 0x30))
      else ERR

    ERR ->
      ERR

carry : Int -> Int -> State
carry hi lo =
  if lo <= 0xFFFFFFFF
  then
    if hi <= 0xFFFFFFFF
    then OK hi lo
    else ERR
  else
    let hi_carry = hi + (lo // 0x100000000) in
    if hi_carry <= 0xFFFFFFFF
    then OK hi_carry (modBy 0x100000000 lo)
    else ERR


-- BINARY FORMAT

encodeLE : Int64 -> E.Encoder
encodeLE (Int64 hi lo) = E.sequence [ E.unsignedInt32 LE lo, E.unsignedInt32 LE hi ]

encodeBE : Int64 -> E.Encoder
encodeBE (Int64 hi lo) = E.sequence [ E.unsignedInt32 BE hi, E.unsignedInt32 BE lo ]

decodeLE : D.Decoder Int64
decodeLE = D.map2 (\lo hi -> Int64 hi lo) (D.unsignedInt32 LE) (D.unsignedInt32 LE)

decodeBE : D.Decoder Int64
decodeBE = D.map2 Int64 (D.unsignedInt32 BE) (D.unsignedInt32 BE)

