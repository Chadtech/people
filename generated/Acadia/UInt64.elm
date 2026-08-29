module Acadia.UInt64 exposing (UInt64, toInt, fromInt, toString, fromString, encodeLE, encodeBE, decodeLE, decodeBE)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E


-- UINT64

type UInt64 = UInt64 Int Int


-- CAST

toInt : UInt64 -> Int
toInt (UInt64 _ lo) = lo

fromInt : Int -> UInt64
fromInt n =
  if n >= 0
  then UInt64 0 (Bitwise.or n 0)
  else UInt64 0xFFFFFFFF (Bitwise.or n 0)


-- TO STRING
--
-- The full UInt64 number is `hi * 0x100000000 + lo` which could be split up
-- into two parts:
--
--   upper = (hi * 0x100000000 + lo) / 1000000000
--   lower = (hi * 0x100000000 + lo) % 1000000000
--
-- Such that the decimal formats of upper and lower can just be appended with
-- appropriate zero padding. Can these formulas be made to fit in a Float64
-- without losing any information?
--
--   upper = (hi * 0x100000000 + lo) / 1000000000
--         = (hi * 0x100000000) / 1000000000 + lo / 1000000000
--
-- I believe that the multiplication just goes into the exponent cleanly since
-- it is a power of two, so it is better to do the division afterwards. Modulus
-- distributes a bit funny:
--
--   lower = (hi * HEX + lo) % DEC
--         = ((hi * HEX) % DEC + (lo % DEC)) % DEC
--         = (((hi % DEC) * (HEX % DEC)) % DEC + (lo % DEC)) % DEC
--         = (((hi % DEC) * 0x1194D800 ) % DEC + (lo % DEC)) % DEC
--
toString : UInt64 -> String
toString (UInt64 hi lo) =
  if hi == 0
  then String.fromInt lo
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

fromString : String -> Maybe UInt64
fromString str =
  case String.foldl step (OK 0 0) str of
    OK hi lo -> Just (UInt64 hi lo)
    ERR      -> Nothing

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

encodeLE : UInt64 -> E.Encoder
encodeLE (UInt64 hi lo) = E.sequence [ E.unsignedInt32 LE lo, E.unsignedInt32 LE hi ]

encodeBE : UInt64 -> E.Encoder
encodeBE (UInt64 hi lo) = E.sequence [ E.unsignedInt32 BE hi, E.unsignedInt32 BE lo ]

decodeLE : D.Decoder UInt64
decodeLE = D.map2 (\lo hi -> UInt64 hi lo) (D.unsignedInt32 LE) (D.unsignedInt32 LE)

decodeBE : D.Decoder UInt64
decodeBE = D.map2 UInt64 (D.unsignedInt32 BE) (D.unsignedInt32 BE)
