module Backend exposing (Person,getPeople,createNewPerson)

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
import PersonId

type alias Person =
  { id : PersonId.PersonId, name : String }

e_ARG_0 =
  \s -> E.sequence [E.uint32BE (E.getSizeString s),E.string s]

d_ARG_0 =
  D.andThen (\n -> if Int32.toInt n < 0 then D.fail else D.string (UInt32.fromInt (Int32.toInt n))) D.int32BE

d_ARG_1 =
  D.andThen (\n -> if Int32.toInt n /= 0 then D.fail else D.succeed ()) D.int32BE

getPeople : Acadia.Transaction.Transaction (List String)
getPeople =
  Acadia.Transaction.Transaction (E.sequence [E.uint32BE (UInt32.fromInt 0),E.uint32BE (UInt32.fromInt 0)]) (D.list d_ARG_0)

createNewPerson : String -> Acadia.Transaction.Transaction ()
createNewPerson =
  \v0 -> Acadia.Transaction.Transaction (E.sequence [E.uint32BE (UInt32.fromInt 0),E.uint32BE (UInt32.fromInt 1),e_ARG_0 v0]) d_ARG_1

