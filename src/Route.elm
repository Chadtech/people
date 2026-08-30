module Route exposing (Route(..), fromUrl, toString)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = NewPerson


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


toString : Route -> String
toString route =
    case route of
        NewPerson ->
            "/people/new"


parser : Parser (Route -> a) a
parser =
    Parser.map NewPerson (Parser.s "people" </> Parser.s "new")
