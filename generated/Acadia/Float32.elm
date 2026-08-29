module Acadia.Float32 exposing (Float32, toFloat, fromFloat, toString, encodeLE, encodeBE, decodeLE, decodeBE)

import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E

type Float32 = Float32 Float

toFloat : Float32 -> Float
toFloat (Float32 n) = n

fromFloat : Float -> Float32
fromFloat = Float32

toString : Float32 -> String
toString (Float32 n) = String.fromFloat n

encodeLE : Float32 -> E.Encoder
encodeLE (Float32 n) = E.float32 LE n

encodeBE : Float32 -> E.Encoder
encodeBE (Float32 n) = E.float32 BE n

decodeLE : D.Decoder Float32
decodeLE = D.map Float32 (D.float32 LE)

decodeBE : D.Decoder Float32
decodeBE = D.map Float32 (D.float32 BE)
