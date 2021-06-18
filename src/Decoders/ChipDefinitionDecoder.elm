module Decoders.ChipDefinitionDecoder exposing (..)

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder, Error(..), array, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias ChipDefinition =
    { modules : Array ChipModule
    , pinouts : Array ChipPinout
    }

type alias ChipModule =
    { name : String
    , registers : List Register
    }

chipModuleDecoder : Decoder ChipModule
chipModuleDecoder =
    Decode.succeed ChipModule
      |> required "name" string
      |> required "registers" (list registerDecoder)

type alias Register =
    { name : String
    , fields : List Bitfield
    }

registerDecoder : Decoder Register
registerDecoder =
    Decode.succeed Register
        |> required "name" string
        |> required "fields" (list bitfieldDecoder)

type alias Bitfield =
    { name: String
    , caption: String
    , mask: String
    , rw: String
    , description: String
    }

bitfieldDecoder : Decoder Bitfield
bitfieldDecoder =
    Decode.succeed Bitfield
        |> required "name" string
        |> required "caption" string
        |> required "mask" string
        |> required "rw" string
        |> required "description" string



type alias ChipPinout =
    { name : String
    , pins: Array ChipPin
    }


chipPinoutDecoder : Decoder ChipPinout
chipPinoutDecoder =
    Decode.succeed ChipPinout
      |> required "name" string
      |> required "pins" (array chipPinDecoder)

type alias ChipPin =
    { pad : String
    , pos : Int
    }

chipPinDecoder : Decoder ChipPin
chipPinDecoder =
    Decode.succeed ChipPin
      |> required "pad" string
      |> required "pos" int

chipDefinitionDecoder : Decoder ChipDefinition
chipDefinitionDecoder =
  Decode.succeed ChipDefinition
    |> required "modules" (array chipModuleDecoder)
    |> required "pinouts" (array chipPinoutDecoder)
