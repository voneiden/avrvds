module Decoders.ChipDefinitionDecoder exposing (..)

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder, Error(..), andThen, array, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (required)
import String exposing (startsWith)


type alias ChipDefinition =
    { variants: Array ChipVariant
    , pinouts : Array ChipPinout
    , devices: Array ChipDevice
    , modules : Array ChipModule
    }

chipDefinitionDecoder : Decoder ChipDefinition
chipDefinitionDecoder =
  Decode.succeed ChipDefinition
    |> required "variants" (array chipVariantDecoder)
    |> required "pinouts" (array chipPinoutDecoder)
    |> required "devices" (array chipDeviceDecoder)
    |> required "modules" (array chipModuleDecoder)

-- Variant decoding

type alias ChipVariant =
    { orderCode: String
    , package : String
    , pinout: String
    , speedMax : Int
    , tempMax : Int
    , tempMin : Int
    , vccMax : Float
    , vccMin : Float
    }


chipVariantDecoder : Decoder ChipVariant
chipVariantDecoder =
    Decode.succeed ChipVariant
      |> required "order_code" string
      |> required "package" string
      |> required "pinout" string
      |> required "speed_max" int
      |> required "temp_max" int
      |> required "temp_min" int
      |> required "vcc_max" float
      |> required "vcc_min" float

-- Pinout decoding

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
    { pad : Pad
    , position : Int
    }

chipPinDecoder : Decoder ChipPin
chipPinDecoder =
    Decode.succeed ChipPin
      |> required "pad" (string |> andThen padDecoder)
      |> required "position" int


-- Device decoding

type alias ChipDevice =
    { name : String
    , modules : List ChipDeviceModule
    }

chipDeviceDecoder : Decoder ChipDevice
chipDeviceDecoder =
    Decode.succeed ChipDevice
      |> required "name" string
      |> required "modules" (list chipDeviceModuleDecoder)

type DeviceModuleCategory =
    PORT | INTERFACE | ANALOG | TIMER | EVENT | LOGIC | PTC | OTHER

type alias ChipDeviceModule =
    { name : String
    , group : DeviceModuleCategory
    , instances : List Instance
    }
moduleCategoryDecoder : String -> Decoder DeviceModuleCategory
moduleCategoryDecoder name =
    case name of
        "PORT" -> Decode.succeed PORT
        "TWI" -> Decode.succeed INTERFACE
        "SPI" -> Decode.succeed INTERFACE
        "USART" -> Decode.succeed INTERFACE
        "ADC" -> Decode.succeed ANALOG
        "AC" -> Decode.succeed ANALOG
        "DAC" -> Decode.succeed ANALOG
        "TCA" -> Decode.succeed TIMER
        "TCB" -> Decode.succeed TIMER
        "TCD" -> Decode.succeed TIMER
        "EVSYS" -> Decode.succeed EVENT
        "CCL" -> Decode.succeed LOGIC
        "PTC" -> Decode.succeed PTC
        _ -> Decode.succeed OTHER

chipDeviceModuleDecoder : Decoder ChipDeviceModule
chipDeviceModuleDecoder =
    Decode.succeed ChipDeviceModule
      |> required "name" string
      |> required "name"  (string |> andThen moduleCategoryDecoder)
      |> required "instances" (list instanceDecoder)


type alias Instance =
    { name: String
    , signals: Maybe (List Signal)
    }

instanceDecoder : Decoder Instance
instanceDecoder =
    Decode.succeed Instance
      |> required "name" string
      |> required "signals" (nullable (list signalDecoder))

type Pad
    = VDD | GND
    | PA0 | PA1 | PA2 | PA3 | PA4 | PA5 | PA6 | PA7
    | PB0 | PB1 | PB2 | PB3 | PB4 | PB5 | PB6 | PB7

padDecoder : String -> Decoder Pad
padDecoder pad =
    case pad of
    "VDD" -> Decode.succeed VDD
    "GND" -> Decode.succeed GND
    "PA0" -> Decode.succeed PA0
    "PA1" -> Decode.succeed PA1
    "PA2" -> Decode.succeed PA2
    "PA3" -> Decode.succeed PA3
    "PA4" -> Decode.succeed PA4
    "PA5" -> Decode.succeed PA5
    "PA6" -> Decode.succeed PA6
    "PA7" -> Decode.succeed PA7
    "PB0" -> Decode.succeed PB0
    "PB1" -> Decode.succeed PB1
    "PB2" -> Decode.succeed PB2
    "PB3" -> Decode.succeed PB3
    "PB4" -> Decode.succeed PB4
    "PB5" -> Decode.succeed PB5
    "PB6" -> Decode.succeed PB6
    "PB7" -> Decode.succeed PB7
    _ ->
        Decode.fail <| "Unsupported pad: " ++ pad

padToString : Pad -> String
padToString pad =
    case pad of
        VDD -> "VDD"
        GND -> "GND"
        PA0 -> "PA0"
        PA1 -> "PA1"
        PA2 -> "PA2"
        PA3 -> "PA3"
        PA4 -> "PA4"
        PA5 -> "PA5"
        PA6 -> "PA6"
        PA7 -> "PA7"
        PB0 -> "PB0"
        PB1 -> "PB1"
        PB2 -> "PB2"
        PB3 -> "PB3"
        PB4 -> "PB4"
        PB5 -> "PB5"
        PB6 -> "PB6"
        PB7 -> "PB7"



type alias Signal =
    { function : String
    , group : String
    , index : Maybe Int
    , pad : Pad
    }

signalDecoder : Decoder Signal
signalDecoder =
    Decode.succeed Signal
      |> required "function" string
      |> required "group" string
      |> required "index" (nullable int)
      |> required "pad" (string |> andThen padDecoder)


-- Module decoding

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

