module Decoders.ChipDefinitionDecoder exposing (..)

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder, Error(..), andThen, array, int, list, nullable, string)
import Json.Decode.Pipeline exposing (required)
import String exposing (startsWith)


type alias ChipDefinition =
    { modules : Array ChipModule
    , pinouts : Array ChipPinout
    }

type alias ChipModule =
    { id: String
    , name : String
    , caption: String
    , registerGroups : Maybe (List RegisterGroup)
    }

chipModuleDecoder : Decoder ChipModule
chipModuleDecoder =
    Decode.succeed ChipModule
        |> required "id" string
        |> required "name" string
        |> required "caption" string
        |> required "register_groups" (nullable (list registerGroupDecoder))


type alias RegisterGroup =
    { name : String
    , caption : String
    , size : String
    , registers : List Register
    }

registerGroupDecoder : Decoder RegisterGroup
registerGroupDecoder =
    Decode.succeed RegisterGroup
        |> required "name" string
        |> required "caption" string
        |> required "size" string
        |> required "registers" (list registerDecoder)

type alias Register =
    { name : String
    , bitfields : Maybe (List Bitfield)
    }

registerDecoder : Decoder Register
registerDecoder =
    Decode.succeed Register
        |> required "name" string
        |> required "bitfields" (nullable (list bitfieldDecoder))

type alias Bitfield =
    { name: String
    , caption: String
    , mask: String
    , rw: String
    , description: Maybe String
    }

bitfieldDecoder : Decoder Bitfield
bitfieldDecoder =
    Decode.succeed Bitfield
        |> required "name" string
        |> required "caption" string
        |> required "mask" string
        |> required "rw" string
        |> required "description" (nullable string)


type PinoutType
    = SOIC

pinoutTypeDecoder : String -> Decoder PinoutType
pinoutTypeDecoder name =
    if startsWith "SOIC" name then
        Decode.succeed SOIC
    else
        Decode.fail <| "Unsupported PinoutType: " ++ name

type alias ChipPinout =
    { name : String
    , pinoutType: PinoutType
    , pins: List ChipPin
    }


chipPinoutDecoder : Decoder ChipPinout
chipPinoutDecoder =
    Decode.succeed ChipPinout
      |> required "name" string
      |> required "name" (string |> andThen pinoutTypeDecoder)
      |> required "pins" (list chipPinDecoder)

type alias ChipPin =
    { pad : String
    , position : Int
    }

chipPinDecoder : Decoder ChipPin
chipPinDecoder =
    Decode.succeed ChipPin
      |> required "pad" string
      |> required "position" int

chipDefinitionDecoder : Decoder ChipDefinition
chipDefinitionDecoder =
  Decode.succeed ChipDefinition
    |> required "modules" (array chipModuleDecoder)
    |> required "pinouts" (array chipPinoutDecoder)
