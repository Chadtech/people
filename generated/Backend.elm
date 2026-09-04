module Backend exposing (Person,PersonPageFlags,getPeople,loadPersonPage,createNewPerson)

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

type alias PersonPageFlags =
  { person : Person }

e_ARG_1 =
  \s -> E.sequence [E.uint32BE (E.getSizeString s),E.string s]

e_ARG_0 =
  \(PersonId.PersonId u) -> E.sequence [E.uint32BE (UInt32.fromInt 8),E.uint64BE u]

d_ARG_0 =
  D.andThen (\n -> if Int32.toInt n < 0 then D.fail else D.string (UInt32.fromInt (Int32.toInt n))) D.int32BE

d_ARG_1 =
  D.andThen (\n -> if Int32.toInt n < 1 then D.fail else d_VARIANT_0 (UInt32.fromInt (Int32.toInt n))) D.int32BE

d_ARG_2 =
  D.map (\v -> PersonId.PersonId v) (D.andThen (\n -> if Int32.toInt n /= 8 then D.fail else D.uint64BE) D.int32BE)

d_VARIANT_0 =
  \size -> D.andThen (\tag -> 
  case UInt8.toInt tag of
    0 -> if UInt32.toInt size < 9 then D.fail else D.andThen (\x_0_person_id_0 -> 
      let
        o0 = UInt32.fromInt 9
      in
      D.andThen (\x_0_person_name -> D.succeed (Maybe.Just { person = { id = PersonId.PersonId x_0_person_id_0 , name = x_0_person_name } })) (D.string (UInt32.fromInt (UInt32.toInt size - UInt32.toInt o0)))) D.uint64LE
    1 -> if UInt32.toInt size /= 1 then D.fail else D.succeed Maybe.Nothing
    _ -> D.fail) D.uint8

getPeople : Acadia.Transaction.Transaction (List String)
getPeople =
  Acadia.Transaction.Transaction (E.sequence [E.uint32BE (UInt32.fromInt 0),E.uint32BE (UInt32.fromInt 0)]) (D.list d_ARG_0)

loadPersonPage : PersonId.PersonId -> Acadia.Transaction.Transaction (Maybe.Maybe PersonPageFlags)
loadPersonPage =
  \v0 -> Acadia.Transaction.Transaction (E.sequence [E.uint32BE (UInt32.fromInt 0),E.uint32BE (UInt32.fromInt 1),e_ARG_0 v0]) d_ARG_1

createNewPerson : String -> Acadia.Transaction.Transaction PersonId.PersonId
createNewPerson =
  \v0 -> Acadia.Transaction.Transaction (E.sequence [E.uint32BE (UInt32.fromInt 0),E.uint32BE (UInt32.fromInt 2),e_ARG_1 v0]) d_ARG_2

