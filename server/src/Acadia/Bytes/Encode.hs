{-# LANGUAGE PackageImports #-}
module Acadia.Bytes.Encode
  ( B.Builder
  , bool
  , charBE
  , charLE
  , B.int8
  , B.int16BE
  , B.int32BE
  , B.int64BE
  , B.int16LE
  , B.int32LE
  , B.int64LE
  , uint8
  , uint16BE
  , uint32BE
  , uint64BE
  , uint16LE
  , uint32LE
  , uint64LE
  , float32BE
  , float64BE
  , float32LE
  , float64LE
  , string
  , uuid
  , timeBE
  , timeLE
  --
  , sequence
  --
  , getSizeString
  )
  where


import "base" Prelude hiding (sequence)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Word (Word8, Word16, Word32, Word64)

import qualified Acadia.Time as Time
import qualified Acadia.Uuid as Uuid


-- Compatibility copy of the Acadia 0.3.0 generated runtime.
-- Protocol lengths count UTF-8 bytes, not Text characters. This source directory
-- precedes generated/ so regeneration cannot undo the fix. Keep the rest of this
-- file aligned with the generated runtime when upgrading Acadia.
getSizeString :: Text.Text -> Word32
getSizeString text =
  fromIntegral (BS.length (Text.encodeUtf8 text))


sequence :: [B.Builder] -> B.Builder
sequence =
  mconcat


bool :: Bool -> B.Builder
bool b = B.word8 (if b then 1 else 0)

charBE :: Char -> B.Builder
charLE :: Char -> B.Builder

charBE c = B.word32BE (fromIntegral (Char.ord c))
charLE c = B.word32LE (fromIntegral (Char.ord c))


uint8    :: Word8  -> B.Builder
uint16BE :: Word16 -> B.Builder
uint32BE :: Word32 -> B.Builder
uint64BE :: Word64 -> B.Builder
uint16LE :: Word16 -> B.Builder
uint32LE :: Word32 -> B.Builder
uint64LE :: Word64 -> B.Builder

uint8    = B.word8
uint16BE = B.word16BE
uint32BE = B.word32BE
uint64BE = B.word64BE
uint16LE = B.word16LE
uint32LE = B.word32LE
uint64LE = B.word64LE

float32BE :: Float  -> B.Builder
float64BE :: Double -> B.Builder
float32LE :: Float  -> B.Builder
float64LE :: Double -> B.Builder

float32BE = B.floatBE
float64BE = B.doubleBE
float32LE = B.floatLE
float64LE = B.doubleLE


string :: Text.Text -> B.Builder
string =
  Text.encodeUtf8Builder

uuid :: Uuid.Uuid -> B.Builder
uuid (Uuid.Uuid x y) =
  B.word64BE x <> B.word64BE y

timeBE :: Time.Posix -> B.Builder
timeBE time =
  B.int64BE (Time.toMicroseconds time)

timeLE :: Time.Posix -> B.Builder
timeLE time =
  B.int64LE (Time.toMicroseconds time)
