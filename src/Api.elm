module Api exposing (attempt)

import Acadia.Transaction


endpointUrl : String
endpointUrl =
    "/_endpoints"


attempt : (Maybe a -> msg) -> Acadia.Transaction.Transaction a -> Cmd msg
attempt =
    Acadia.Transaction.attempt endpointUrl
