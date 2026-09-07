module Remote exposing (Remote(..))


type Remote data
    = Failed
    | NotFound
    | Found data
