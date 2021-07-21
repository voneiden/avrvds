module Data.ChipCdef exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Error(..), andThen, index, list, map, map2, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Parser exposing (Parser)
import Parser.Dict
import String

type ConstantCdef
    = ConstantCdef String String

type RegisterCdef
    = RegisterCdef String

type alias ChipCdef =
    { registers: List String
    , constants : List (String, String)
    --, fastRegisters : Dict String RegisterCdef
    --, fastConstants : Dict String ConstantCdef
    , registerParser : Parser RegisterCdef
    , constantParser : Parser ConstantCdef
    }

registersDecoder : Decoder (Dict String RegisterCdef)
registersDecoder = map (List.map  (\r -> (r, RegisterCdef r)) >> Dict.fromList) (list string)

constantsDecoder : Decoder (Dict String ConstantCdef)
constantsDecoder = map (List.map  (\(name, info)  -> (name, ConstantCdef name info)) >> Dict.fromList) (list decodeTuple)

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
    |> required "registers" (registersDecoder |> andThen (succeed << Parser.Dict.fromDict))
    |> required "constants" (constantsDecoder |> andThen (succeed << Parser.Dict.fromDict))
