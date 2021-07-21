module Data.Source exposing (Source, ChipSource, sourceDecoder)

import Json.Decode exposing (Decoder, andThen, fail, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Url as Url exposing (Url)

type alias Source =
    { defaultChip: String
    , chips : List ChipSource
    }

type alias ChipSource =
    { name: String
    , datasheet: Url
    }

validateDefaultChip : Source -> Decoder Source
validateDefaultChip source =
    case List.filter (\x -> x.name == source.defaultChip) source.chips of
        [_] ->
            succeed source
        _ -> fail "defaultChip has no matching definition in chips[]"

sourceDecoder : Decoder Source
sourceDecoder =
    succeed Source
        |> required "defaultChip" string
        |> required "chips" (list chipSourceDecoder)
        |> andThen validateDefaultChip

decodeUrl : String -> Decoder Url
decodeUrl urlString =
    case Url.fromString urlString of
        Just url ->
            succeed url
        Nothing ->
            fail <| "\"" ++ urlString ++ "\" is not a valid url"


chipSourceDecoder : Decoder ChipSource
chipSourceDecoder =
    succeed ChipSource
        |> required "name" string
        |> required "datasheet" (string |> andThen decodeUrl)
