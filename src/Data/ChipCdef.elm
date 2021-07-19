module Data.ChipCdef exposing (..)

import Json.Decode as Decode exposing (Decoder, Error(..), index, list, map2, string)
import Json.Decode.Pipeline exposing (required)
import String


type alias ChipCdef =
    { registers: List String
    , constants : List (String, String)
    }

decodeTuple : Decoder (String, String)
decodeTuple =
    map2 Tuple.pair
        (index 0 string)
        (index 1 string)

chipCDEFDecoder : Decoder ChipCdef
chipCDEFDecoder =
  Decode.succeed ChipCdef
    |> required "registers" (list string)
    |> required "constants" (list decodeTuple)
