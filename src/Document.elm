module Document exposing (Document, map, toBrowserDocument)

import Browser
import Html.Styled as H


type alias Document msg =
    { title : String
    , body : List (H.Html msg)
    }


map : (a -> msg) -> Document a -> Document msg
map toMsg document =
    { title = document.title
    , body = List.map (H.map toMsg) document.body
    }


toBrowserDocument : Document msg -> Browser.Document msg
toBrowserDocument document =
    { title = document.title
    , body = List.map H.toUnstyled document.body
    }
