module Data.Chip exposing (..)

import Array exposing (Array)
import Data.ChipTypes exposing (DeviceModuleCategory(..), Module(..), Pad, PinoutType, pinoutTypeDecoder)
import Data.Util.DeviceModuleCategory as DeviceModuleCategory
import Data.Util.Module as Module
import Data.Util.Pad as Pad
import Json.Decode as Decode exposing (Decoder, Error(..), andThen, array, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (required, resolve)
import List exposing (map)
import String exposing (dropRight, right, toInt)
import Util.BitMask exposing (BitMask, decodeBitMask, decodeBits, decodeHex)


type alias ChipDefinition =
    { variants: Array ChipVariant
    , pinouts : Array ChipPinout
    , devices: Array ChipDevice
    , modules : List ChipModule
    }

chipDefinitionDecoder : Decoder ChipDefinition
chipDefinitionDecoder =
  Decode.succeed ChipDefinition
    |> required "variants" (array chipVariantDecoder)
    |> required "pinouts" (array chipPinoutDecoder)
    |> required "devices" (array chipDeviceDecoder)
    |> required "modules" (list chipModuleDecoder)

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
      |> required "pad" (string |> andThen Pad.decode)
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





type alias ChipDeviceModule =
    { name : Module
    , group : DeviceModuleCategory
    , instances : List Instance
    }

moduleCategoryDecoder : Module -> Decoder DeviceModuleCategory
moduleCategoryDecoder name =
    case name of
        PORT -> Decode.succeed IO
        TWI -> Decode.succeed INTERFACE
        SPI -> Decode.succeed INTERFACE
        USART -> Decode.succeed INTERFACE
        ADC -> Decode.succeed ANALOG
        AC -> Decode.succeed ANALOG
        DAC -> Decode.succeed ANALOG
        TCA -> Decode.succeed TIMER
        TCB -> Decode.succeed TIMER
        TCD -> Decode.succeed TIMER
        EVSYS -> Decode.succeed EVENT
        CCL -> Decode.succeed LOGIC
        PTC -> Decode.succeed TOUCH
        CLKCTRL -> Decode.succeed CLOCKCONTROL
        CPU -> Decode.succeed DEBUG
        _ -> Decode.succeed OTHER

chipDeviceModuleDecoder : Decoder ChipDeviceModule
chipDeviceModuleDecoder =
    let
        mapSignal : Module -> DataSignal -> Signal
        mapSignal module_ dataSignal =
            Signal dataSignal.function module_ dataSignal.group dataSignal.index dataSignal.pad

        mapInstance : Module -> DataInstance -> Instance
        mapInstance module_ dataInstance =
            Instance dataInstance.name <| Maybe.map (map (mapSignal module_)) dataInstance.signals

        toChipDeviceModule : Module -> DeviceModuleCategory -> List DataInstance -> Decoder ChipDeviceModule
        toChipDeviceModule module_ moduleCategory instances =
            Decode.succeed <| ChipDeviceModule module_ moduleCategory (map (mapInstance module_) instances)
    in
        Decode.succeed toChipDeviceModule
          |> required "name" (string |> andThen Module.decode)
          |> required "name"  (string |> andThen Module.decode |> andThen moduleCategoryDecoder)
          |> required "instances" (list instanceDecoder)
          |> resolve


type alias DataInstance =
    { name: String
    , signals: Maybe (List DataSignal)
    }

type alias Instance =
    { name: String
    , signals: Maybe (List Signal)
    }

instanceDecoder : Decoder DataInstance
instanceDecoder =
    Decode.succeed DataInstance
      |> required "name" string
      |> required "signals" (nullable (list signalDecoder))



type alias DataSignal =
    { function : String
    , group : String
    , index : Maybe Int
    , pad : Pad
    }

type alias Signal =
    { function : String
    , deviceModule: Module
    , group : String
    , index : Maybe Int
    , pad : Pad
    }

removeTrailingNumber : String -> String
removeTrailingNumber s =
    case toInt <| right 1 s of
        Just _ -> dropRight 1 s
        Nothing -> s



signalDecoder : Decoder DataSignal
signalDecoder =
    Decode.succeed DataSignal
      |> required "function" string
      |> required "group" string
      |> required "index" (nullable int)
      |> required "pad" (string |> andThen Pad.decode)


-- Module decoding

type alias ChipModule =
    { id: String
    , name : Module
    , caption: String
    , registerGroups : Maybe (List RegisterGroup)
    }

chipModuleDecoder : Decoder ChipModule
chipModuleDecoder =
    Decode.succeed ChipModule
        |> required "id" string
        |> required "name" (string |> andThen Module.decode)
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
    , description: Maybe String
    , bitfields : Maybe (List Bitfield)
    }

registerDecoder : Decoder Register
registerDecoder =
    Decode.succeed Register
        |> required "name" string
        |> required "description" (nullable string)
        |> required "bitfields" (nullable (list bitfieldDecoder))

type alias Bitfield =
    { name: String
    , caption: String
    , mask: BitMask
    , rw: String
    , description: Maybe String
    }

bitfieldDecoder : Decoder Bitfield
bitfieldDecoder =
    Decode.succeed Bitfield
        |> required "name" string
        |> required "caption" string
        |> required "mask" (string |> andThen decodeHex |> andThen decodeBits |> andThen decodeBitMask)
        |> required "rw" string
        |> required "description" (nullable string)

