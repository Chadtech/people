module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , toString
    )

import ConversationId exposing (ConversationId)
import ConversationId.Util as ConversationIdUtil
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attr
import PersonId exposing (PersonId)
import PersonId.Util as PersonIdUtil
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser)


type Route
    = Conversations
    | Conversation ConversationId
    | AllPersons
    | NewPerson
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
                Conversations ->
                    [ "conversation" ]

                Conversation id ->
                    [ "conversation", ConversationIdUtil.toString id ]

                AllPersons ->
                    [ "person", "all" ]

                NewPerson ->
                    [ "person", "new" ]

                Person personId ->
                    [ "person", PersonIdUtil.toString personId ]
    in
    String.join "/" parts


href : Route -> Attribute msg
href route =
    Attr.href ("/" ++ toString route)


parser : Parser (Route -> a) a
parser =
    P.oneOf
        [ P.map Conversations (P.s "conversation")
        , P.map Conversation (P.s "conversation" </> P.custom "conversationId" ConversationIdUtil.fromString)
        , P.map AllPersons (P.s "person" </> P.s "all")
        , P.map NewPerson (P.s "person" </> P.s "new")
        , P.map Person (P.s "person" </> P.custom "personId" PersonIdUtil.fromString)
        ]
