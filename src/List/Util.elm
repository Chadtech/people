module List.Util exposing (find, findUnique)


find : (a -> Bool) -> List a -> Maybe a
find predicate items =
    case items of
        [] ->
            Nothing

        item :: rest ->
            if predicate item then
                Just item

            else
                find predicate rest


{-| Resolve exactly one match, rejecting ambiguous data as well as missing data.
-}
findUnique : (a -> Bool) -> List a -> Maybe a
findUnique predicate items =
    case items of
        [] ->
            Nothing

        item :: rest ->
            if predicate item then
                case find predicate rest of
                    Nothing ->
                        Just item

                    Just _ ->
                        Nothing

            else
                findUnique predicate rest
