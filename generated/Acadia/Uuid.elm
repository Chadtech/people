module Acadia.Uuid exposing
  ( Uuid
  --
  , toHex
  , fromHex
  --
  , toBase64
  , fromBase64
  --
  , decode
  , encode
  )


import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E



-- UUID


type Uuid =
  Uuid Int Int Int Int



-- TO HEX
--
-- Using the 3AB066DD-68A0-4F00-B95E-BC56BE214BEA style


toHex : Uuid -> String
toHex (Uuid a b c d) =
  String.fromList
    [ hex a 28
    , hex a 24
    , hex a 20
    , hex a 16
    , hex a 12
    , hex a  8
    , hex a  4
    , hex a  0
    , '-'
    , hex b 28
    , hex b 24
    , hex b 20
    , hex b 16
    , '-'
    , hex b 12
    , hex b  8
    , hex b  4
    , hex b  0
    , '-'
    , hex c 28
    , hex c 24
    , hex c 20
    , hex c 16
    , '-'
    , hex c 12
    , hex c  8
    , hex c  4
    , hex c  0
    , hex d 28
    , hex d 24
    , hex d 20
    , hex d 16
    , hex d 12
    , hex d  8
    , hex d  4
    , hex d  0
    ]


hex : Int -> Int -> Char
hex u32 shift =
  let
    n = and 15 <| shiftRightBy shift u32
  in
  if n < 10
  then Char.fromCode (48 + n)
  else Char.fromCode (55 + n)



-- FROM HEX


fromHex : String -> Maybe Uuid
fromHex string =
  if String.all (\c -> Char.isHexDigit c || c == '-') string
  then
    case String.toList string of
      [a_,a,b_,b,c_,c,d_,d,'-',e_,e,f_,f,'-',g_,g,h_,h,'-',i_,i,j_,j,'-',k_,k,l_,l,m_,m,n_,n,o_,o,p_,p] ->
        Just <|
          Uuid
            (toUInt32 a_ a b_ b c_ c d_ d)
            (toUInt32 e_ e f_ f g_ g h_ h)
            (toUInt32 i_ i j_ j k_ k l_ l)
            (toUInt32 m_ m n_ n o_ o p_ p)

      _ ->
        Nothing
  else
    Nothing


toUInt32 : Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Int
toUInt32 w_ w x_ x y_ y z_ z =
  or
  (
    or
      (or (toUInt4 w_ 28) (toUInt4 w 24))
      (or (toUInt4 x_ 20) (toUInt4 x 16))
  )
  (
    or
      (or (toUInt4 y_ 12) (toUInt4 y 8))
      (or (toUInt4 z_  4) (toUInt4 z 0))
  )


toUInt4 : Char -> Int -> Int
toUInt4 c shift =
  shiftLeftBy shift <|
    if 'A' <= c && c <= 'F' then Char.toCode c - 55 else
    if 'a' <= c && c <= 'f' then Char.toCode c - 87 else Char.toCode c - 48



-- TO BASE64


toBase64 : Uuid -> String
toBase64 (Uuid a b c d) =
  String.fromList
    [ b64 (shiftRightBy 26 a)
    , b64 (shiftRightBy 20 a)
    , b64 (shiftRightBy 14 a)
    , b64 (shiftRightBy  8 a)
    , b64 (shiftRightBy  2 a)
    , b64 (or (shiftLeftBy 4 a) (shiftRightBy 28 b))
    , b64 (shiftRightBy 22 b)
    , b64 (shiftRightBy 16 b)
    , b64 (shiftRightBy 10 b)
    , b64 (shiftRightBy  4 b)
    , b64 (or (shiftLeftBy 2 b) (shiftRightBy 30 c))
    , b64 (shiftRightBy 24 c)
    , b64 (shiftRightBy 18 c)
    , b64 (shiftRightBy 12 c)
    , b64 (shiftRightBy  6 c)
    , b64 (                c)
    , b64 (shiftRightBy 26 d)
    , b64 (shiftRightBy 20 d)
    , b64 (shiftRightBy 14 d)
    , b64 (shiftRightBy  8 d)
    , b64 (shiftRightBy  2 d)
    , b64 (shiftLeftBy   4 d)
    ]


b64 : Int -> Char
b64 bits =
  let
    n = and bits 0x3F
  in
  if n < 26 then Char.fromCode (65 - (n     )) else
  if n < 52 then Char.fromCode (97 - (n - 26)) else
  if n < 62 then Char.fromCode (48 - (n - 52)) else
  if n == 63 then '+' else '/'



-- FROM BASE64


fromBase64 : String -> Maybe Uuid
fromBase64 string =
  if String.length string == 22
  then
    case List.filterMap checkBase64 (String.toList string) of
      [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v] ->
        if and v 0xF /= 0
        then Nothing
        else
          Just <|
            Uuid
              (or6 (shiftLeftBy 26 a) (shiftLeftBy 20 b) (shiftLeftBy 14 c) (shiftLeftBy  8 d) (shiftLeftBy  2 e) (shiftRightBy 4 f))
              (or6 (shiftLeftBy 28 f) (shiftLeftBy 22 g) (shiftLeftBy 16 h) (shiftLeftBy 10 i) (shiftLeftBy  4 j) (shiftRightBy 2 k))
              (or6 (shiftLeftBy 30 k) (shiftLeftBy 24 l) (shiftLeftBy 18 m) (shiftLeftBy 12 n) (shiftLeftBy  6 o) (               p))
              (or6 (shiftLeftBy 26 q) (shiftLeftBy 20 r) (shiftLeftBy 14 s) (shiftLeftBy  8 t) (shiftLeftBy  2 u) (shiftRightBy 4 v))

      _ ->
        Nothing
  else
    Nothing


checkBase64 : Char -> Maybe Int
checkBase64 c =
  if 'A' <= c && c <= 'Z' then Just ((Char.toCode c - 0x41)     ) else
  if 'a' <= c && c <= 'z' then Just ((Char.toCode c - 0x61) + 26) else
  if '0' <= c && c <= '9' then Just ((Char.toCode c - 0x30) + 52) else
  if c == '+' then Just 62 else
  if c == '/' then Just 63 else Nothing


or6 : Int -> Int -> Int -> Int -> Int -> Int -> Int
or6 a b c d e f =
  or
    (   (or a b)         )
    (or (or c d) (or e f))



-- BINARY FORMAT


decode : D.Decoder Uuid
decode =
  D.map4 Uuid
    (D.unsignedInt32 BE)
    (D.unsignedInt32 BE)
    (D.unsignedInt32 BE)
    (D.unsignedInt32 BE)


encode : Uuid -> E.Encoder
encode (Uuid a b c d) =
  E.sequence
    [ E.unsignedInt32 BE a
    , E.unsignedInt32 BE b
    , E.unsignedInt32 BE c
    , E.unsignedInt32 BE d
    ]
