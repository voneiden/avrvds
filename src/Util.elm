module Util exposing (..)


findOne : (b -> Bool) -> List b -> Maybe b
findOne f choices =
    case List.filter f choices of
        [ v ] ->
            Just v

        _ ->
            Nothing


