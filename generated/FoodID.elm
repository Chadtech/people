module FoodID exposing (FoodID(..))

import Acadia.Bytes.Decode as D
import Acadia.Bytes.Encode as E
import Acadia.Float32 as Float32
import Acadia.Float64 as Float64
import Acadia.Int8 as Int8
import Acadia.Int16 as Int16
import Acadia.Int32 as Int32
import Acadia.Int64 as Int64
import Acadia.Password as Password
import Acadia.Time as Time
import Acadia.Transaction
import Acadia.UInt8 as UInt8
import Acadia.UInt16 as UInt16
import Acadia.UInt32 as UInt32
import Acadia.UInt64 as UInt64
import Acadia.Uuid as Uuid

type FoodID =
  FoodID UInt64.UInt64

