module Acadia.Float64 exposing (Float64, toFloat, fromFloat, toString, encodeLE, encodeBE, decodeLE, decodeBE)

import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E

type Float64 = Float64 Float

toFloat : Float64 -> Float
toFloat (Float64 n) = n

fromFloat : Float -> Float64
fromFloat = Float64

toString : Float64 -> String
toString (Float64 n) = String.fromFloat n

encodeLE : Float64 -> E.Encoder
encodeLE (Float64 n) = E.float64 LE n

encodeBE : Float64 -> E.Encoder
encodeBE (Float64 n) = E.float64 BE n

decodeLE : D.Decoder Float64
decodeLE = D.map Float64 (D.float64 LE)

decodeBE : D.Decoder Float64
decodeBE = D.map Float64 (D.float64 BE)

