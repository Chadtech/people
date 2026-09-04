module Route exposing (Route(..), fromUrl, href, toString)

import Acadia.UInt64 as UInt64
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attr
import PersonId exposing (PersonId(..))
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser)


type Route
    = NewPerson
    | Person PersonId


fromUrl : Url -> Maybe Route
fromUrl url =
    P.parse parser url


toString : Route -> String
toString route =
    let
        parts : List String
        parts =
            case route of
                NewPerson ->
                    [ "person", "new" ]

                Person personId ->
                    [ "person", personIdToString personId ]
    in
    String.join "/" parts


href : Route -> Attribute msg
href route =
    Attr.href (toString route)


personIdToString : PersonId -> String
personIdToString (PersonId value) =
    UInt64.toString value


parser : Parser (Route -> a) a
parser =
    P.oneOf
        [ P.map NewPerson (P.s "person" </> P.s "new")

        --, P.map Person (P.s "person" </> P.custom "personId" PersonId.fromString)
        ]
