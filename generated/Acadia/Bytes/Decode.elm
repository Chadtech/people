module Acadia.Bytes.Decode exposing
  ( Decoder
  --
  , bool
  , charBE
  , charLE
  --
  , int8
  , int16BE
  , int32BE
  , int64BE
  , int16LE
  , int32LE
  , int64LE
  --
  , uint8
  , uint16BE
  , uint32BE
  , uint64BE
  , uint16LE
  , uint32LE
  , uint64LE
  --
  , float32BE
  , float64BE
  , float32LE
  , float64LE
  --
  , string
  , uuid
  , timeBE
  , timeLE
  --
  , map
  , map2
  , map3
  , fail
  , succeed
  , andThen
  , list
  )


import Acadia.Int8 as Int8 exposing (Int8)
import Acadia.Int16 as Int16 exposing (Int16)
import Acadia.Int32 as Int32 exposing (Int32)
import Acadia.Int64 as Int64 exposing (Int64)
import Acadia.UInt8 as UInt8 exposing (UInt8)
import Acadia.UInt16 as UInt16 exposing (UInt16)
import Acadia.UInt32 as UInt32 exposing (UInt32)
import Acadia.UInt64 as UInt64 exposing (UInt64)
import Acadia.Float32 as Float32 exposing (Float32)
import Acadia.Float64 as Float64 exposing (Float64)
import Acadia.Time as Time
import Acadia.Uuid as Uuid
import Char
import Bytes exposing (Endianness(..))
import Bytes.Decode as D



-- DECODER


type alias Decoder a = D.Decoder a



-- BOOL


bool : D.Decoder Bool
bool =
  D.unsignedInt8
    |> D.andThen (\b -> if b <= 1 then D.succeed (b == 1) else D.fail)



-- CHAR


charBE : D.Decoder Char
charBE =
  D.unsignedInt32 BE
    |> D.andThen (\c -> if c <= 0x10FFFF then D.succeed (Char.fromCode c) else D.fail)


charLE : D.Decoder Char
charLE =
  D.unsignedInt32 LE
    |> D.andThen (\c -> if c <= 0x10FFFF then D.succeed (Char.fromCode c) else D.fail)



-- INT


int8 : D.Decoder Int8
int8 = Int8.decode

int16BE : D.Decoder Int16
int16BE = Int16.decodeBE

int32BE : D.Decoder Int32
int32BE = Int32.decodeBE

int64BE : D.Decoder Int64
int64BE = Int64.decodeBE

int16LE : D.Decoder Int16
int16LE = Int16.decodeLE

int32LE : D.Decoder Int32
int32LE = Int32.decodeLE

int64LE : D.Decoder Int64
int64LE = Int64.decodeLE



-- UINT


uint8 : D.Decoder UInt8
uint8 = UInt8.decode

uint16BE : D.Decoder UInt16
uint16BE = UInt16.decodeBE

uint32BE : D.Decoder UInt32
uint32BE = UInt32.decodeBE

uint64BE : D.Decoder UInt64
uint64BE = UInt64.decodeBE

uint16LE : D.Decoder UInt16
uint16LE = UInt16.decodeLE

uint32LE : D.Decoder UInt32
uint32LE = UInt32.decodeLE

uint64LE : D.Decoder UInt64
uint64LE = UInt64.decodeLE



-- FLOAT


float32BE : D.Decoder Float32
float32BE = Float32.decodeBE

float32LE : D.Decoder Float32
float32LE = Float32.decodeLE

float64BE : D.Decoder Float64
float64BE = Float64.decodeBE

float64LE : D.Decoder Float64
float64LE = Float64.decodeLE



-- STRING


string : UInt32 -> D.Decoder String
string n = D.string (UInt32.toInt n)



-- UUID


uuid : D.Decoder Uuid.Uuid
uuid =
  Uuid.decode



-- TIME


timeBE : D.Decoder Time.Posix
timeBE = Time.decodeBE

timeLE : D.Decoder Time.Posix
timeLE = Time.decodeLE



-- HELPERS


map : (a -> b) -> D.Decoder a -> D.Decoder b
map = D.map

map2 : (a -> b -> v) -> D.Decoder a -> D.Decoder b -> D.Decoder v
map2 = D.map2

map3 : (a -> b -> c -> v) -> D.Decoder a -> D.Decoder b -> D.Decoder c -> D.Decoder v
map3 = D.map3

fail : D.Decoder a
fail = D.fail

succeed : a -> D.Decoder a
succeed = D.succeed

andThen : (a -> D.Decoder b) -> D.Decoder a -> D.Decoder b
andThen = D.andThen

list : D.Decoder a -> D.Decoder (List a)
list decoder =
  D.loop [] <| \revs ->
    D.andThen
    (
      \size ->
        case size of
          0 -> D.succeed (D.Done (List.reverse revs))
          _ -> D.map D.Loop (listHelp size revs decoder)
    )
    (D.unsignedInt32 LE)

listHelp : Int -> List a -> D.Decoder a -> D.Decoder (List a)
listHelp want revs decoder =
  D.loop (0,revs) <| \(have,xs) ->
    if have < want
    then D.map (\x -> D.Loop (have + 1, x::xs)) decoder
    else D.succeed (D.Done xs)
