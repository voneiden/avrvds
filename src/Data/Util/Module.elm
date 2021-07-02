-- Do not edit!
-- This file was generated by elm_type_boiler

module Data.Util.Module exposing (..)
import Data.ChipTypes exposing (Module(..))
import Json.Decode as Decode exposing (Decoder)
list : List Module
list =
    [PORT
    ,TWI
    ,SPI
    ,USART
    ,ADC
    ,AC
    ,DAC
    ,TCA
    ,TCB
    ,TCD
    ,EVSYS
    ,CCL
    ,PTC
    ,BOD
    ,CLKCTRL
    ,CPU
    ,CPUINT
    ,CRCSCAN
    ,FUSE
    ,GPIO
    ,LOCKBIT
    ,NVMCTRL
    ,PORTMUX
    ,RSTCTRL
    ,RTC
    ,SIGROW
    ,SLPCTRL
    ,SYSCFG
    ,USERROW
    ,VPORT
    ,VREF
    ,WDT
    ]


toString : Module -> String
toString moduleValue =
    case moduleValue of
        PORT -> "PORT"
        TWI -> "TWI"
        SPI -> "SPI"
        USART -> "USART"
        ADC -> "ADC"
        AC -> "AC"
        DAC -> "DAC"
        TCA -> "TCA"
        TCB -> "TCB"
        TCD -> "TCD"
        EVSYS -> "EVSYS"
        CCL -> "CCL"
        PTC -> "PTC"
        BOD -> "BOD"
        CLKCTRL -> "CLKCTRL"
        CPU -> "CPU"
        CPUINT -> "CPUINT"
        CRCSCAN -> "CRCSCAN"
        FUSE -> "FUSE"
        GPIO -> "GPIO"
        LOCKBIT -> "LOCKBIT"
        NVMCTRL -> "NVMCTRL"
        PORTMUX -> "PORTMUX"
        RSTCTRL -> "RSTCTRL"
        RTC -> "RTC"
        SIGROW -> "SIGROW"
        SLPCTRL -> "SLPCTRL"
        SYSCFG -> "SYSCFG"
        USERROW -> "USERROW"
        VPORT -> "VPORT"
        VREF -> "VREF"
        WDT -> "WDT"


fromString : String -> Maybe Module
fromString moduleValue =
    case moduleValue of
        "PORT" -> Just PORT
        "TWI" -> Just TWI
        "SPI" -> Just SPI
        "USART" -> Just USART
        "ADC" -> Just ADC
        "AC" -> Just AC
        "DAC" -> Just DAC
        "TCA" -> Just TCA
        "TCB" -> Just TCB
        "TCD" -> Just TCD
        "EVSYS" -> Just EVSYS
        "CCL" -> Just CCL
        "PTC" -> Just PTC
        "BOD" -> Just BOD
        "CLKCTRL" -> Just CLKCTRL
        "CPU" -> Just CPU
        "CPUINT" -> Just CPUINT
        "CRCSCAN" -> Just CRCSCAN
        "FUSE" -> Just FUSE
        "GPIO" -> Just GPIO
        "LOCKBIT" -> Just LOCKBIT
        "NVMCTRL" -> Just NVMCTRL
        "PORTMUX" -> Just PORTMUX
        "RSTCTRL" -> Just RSTCTRL
        "RTC" -> Just RTC
        "SIGROW" -> Just SIGROW
        "SLPCTRL" -> Just SLPCTRL
        "SYSCFG" -> Just SYSCFG
        "USERROW" -> Just USERROW
        "VPORT" -> Just VPORT
        "VREF" -> Just VREF
        "WDT" -> Just WDT
        _ -> Nothing


decode : String -> Decoder Module
decode moduleValue =
    case moduleValue of
        "PORT" -> Decode.succeed PORT
        "TWI" -> Decode.succeed TWI
        "SPI" -> Decode.succeed SPI
        "USART" -> Decode.succeed USART
        "ADC" -> Decode.succeed ADC
        "AC" -> Decode.succeed AC
        "DAC" -> Decode.succeed DAC
        "TCA" -> Decode.succeed TCA
        "TCB" -> Decode.succeed TCB
        "TCD" -> Decode.succeed TCD
        "EVSYS" -> Decode.succeed EVSYS
        "CCL" -> Decode.succeed CCL
        "PTC" -> Decode.succeed PTC
        "BOD" -> Decode.succeed BOD
        "CLKCTRL" -> Decode.succeed CLKCTRL
        "CPU" -> Decode.succeed CPU
        "CPUINT" -> Decode.succeed CPUINT
        "CRCSCAN" -> Decode.succeed CRCSCAN
        "FUSE" -> Decode.succeed FUSE
        "GPIO" -> Decode.succeed GPIO
        "LOCKBIT" -> Decode.succeed LOCKBIT
        "NVMCTRL" -> Decode.succeed NVMCTRL
        "PORTMUX" -> Decode.succeed PORTMUX
        "RSTCTRL" -> Decode.succeed RSTCTRL
        "RTC" -> Decode.succeed RTC
        "SIGROW" -> Decode.succeed SIGROW
        "SLPCTRL" -> Decode.succeed SLPCTRL
        "SYSCFG" -> Decode.succeed SYSCFG
        "USERROW" -> Decode.succeed USERROW
        "VPORT" -> Decode.succeed VPORT
        "VREF" -> Decode.succeed VREF
        "WDT" -> Decode.succeed WDT
        _ -> Decode.fail <| "Unsupported moduleValue: " ++ moduleValue
