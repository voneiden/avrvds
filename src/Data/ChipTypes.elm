module Data.ChipTypes exposing (..)

import Json.Decode as Decode exposing (Decoder, Error(..))
import String exposing (startsWith)

-- @enum
type Pad
    = VDD | GND
    | PA0 | PA1 | PA2 | PA3 | PA4 | PA5 | PA6 | PA7
    | PB0 | PB1 | PB2 | PB3 | PB4 | PB5 | PB6 | PB7


-- @enum
type Module
    = PORT | TWI | SPI | USART |  ADC | AC | DAC |
    TCA | TCB | TCD | EVSYS | CCL | PTC | BOD |
    CLKCTRL | CPU | CPUINT | CRCSCAN | FUSE | GPIO |
    LOCKBIT | NVMCTRL | PORTMUX | RSTCTRL | RTC |
    SIGROW | SLPCTRL | SYSCFG | USERROW | VPORT |
    VREF | WDT

-- @enum
type DeviceModuleCategory =
    IO | INTERFACE | ANALOG | TIMER | EVENT | LOGIC | TOUCH | CLOCKCONTROL | DEBUG | OTHER

-- Pinout decoding

type PinoutType
    = SOIC

pinoutTypeDecoder : String -> Decoder PinoutType
pinoutTypeDecoder name =
    if startsWith "SOIC" name then
        Decode.succeed SOIC
    else
        Decode.fail <| "Unsupported PinoutType: " ++ name
