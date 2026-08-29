module Acadia.Bytes.Encode exposing
  ( Encoder
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
  , sequence
  --
  , getSizeString
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
import Bytes exposing (Endianness(..))
import Bytes.Encode as E


-- ENCODER

type alias Encoder = E.Encoder


-- BOOL

bool : Bool -> E.Encoder
bool b = E.unsignedInt8 (if b then 1 else 0)


-- CHAR

charBE : Char -> E.Encoder
charBE c = E.unsignedInt32 BE (Char.toCode c)

charLE : Char -> E.Encoder
charLE c = E.unsignedInt32 LE (Char.toCode c)


-- INT

int8 : Int8 -> E.Encoder
int8 = Int8.encode

int16BE : Int16 -> E.Encoder
int16BE = Int16.encodeBE

int32BE : Int32 -> E.Encoder
int32BE = Int32.encodeBE

int64BE : Int64 -> E.Encoder
int64BE = Int64.encodeBE

int16LE : Int16 -> E.Encoder
int16LE = Int16.encodeLE

int32LE : Int32 -> E.Encoder
int32LE = Int32.encodeLE

int64LE : Int64 -> E.Encoder
int64LE = Int64.encodeLE


-- UINT

uint8 : UInt8 -> E.Encoder
uint8 = UInt8.encode

uint16BE : UInt16 -> E.Encoder
uint16BE = UInt16.encodeBE

uint32BE : UInt32 -> E.Encoder
uint32BE = UInt32.encodeBE

uint64BE : UInt64 -> E.Encoder
uint64BE = UInt64.encodeBE

uint16LE : UInt16 -> E.Encoder
uint16LE = UInt16.encodeLE

uint32LE : UInt32 -> E.Encoder
uint32LE = UInt32.encodeLE

uint64LE : UInt64 -> E.Encoder
uint64LE = UInt64.encodeLE


-- FLOAT

float32BE : Float32 -> E.Encoder
float32BE = Float32.encodeBE

float64BE : Float64 -> E.Encoder
float64BE = Float64.encodeBE

float32LE : Float32 -> E.Encoder
float32LE = Float32.encodeLE

float64LE : Float64 -> E.Encoder
float64LE = Float64.encodeLE


-- OTHER

string : String -> E.Encoder
string =
  E.string

uuid : Uuid.Uuid -> E.Encoder
uuid =
  Uuid.encode

timeBE : Time.Posix -> E.Encoder
timeBE =
  Time.encodeBE

timeLE : Time.Posix -> E.Encoder
timeLE =
  Time.encodeLE


-- HELPERS

sequence : List E.Encoder -> E.Encoder
sequence =
  E.sequence

getSizeString : String -> UInt32
getSizeString str =
  UInt32.fromInt (E.getStringWidth str)

