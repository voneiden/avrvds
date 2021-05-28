module Test exposing (..)

import Html exposing (text)
import Xml.Decode exposing (..)

import Xml.Decode exposing (..)

import Result exposing (withDefault)
import Debug exposing (toString)

-- <memory-segment exec="0" name="EEPROM" pagesize="0x20" rw="RW" size="0x0080" start="0x00001400" type="eeprom"/>
type alias MemorySegment =
    { exec : String
    , name : String
    , pageSize: Maybe String
    , rw : String
    , size : String
    , start : String
    , segmentType : String
    }

memorySegmentDecoder : Decoder MemorySegment
memorySegmentDecoder =
    succeed MemorySegment
        |> map2 (|>) (stringAttr "exec")
        |> map2 (|>) (stringAttr "name")
        |> map2 (|>) (maybe (stringAttr "pagesize"))
        |> map2 (|>) (stringAttr "rw")
        |> map2 (|>) (stringAttr "size")
        |> map2 (|>) (stringAttr "start")
        |> map2 (|>) (stringAttr "type")

-- <address-space endianness="little" id="data" name="data" size="0xA000" start="0x0000">
type alias AddressSpace =
    { endianness : String
    , id : String
    , name : String
    , size : String
    , start : String
    , memorySegments : List MemorySegment
    }

addressSpaceDecoder : Decoder AddressSpace
addressSpaceDecoder =
    succeed AddressSpace
        |> map2 (|>) (stringAttr "endianness")
        |> map2 (|>) (stringAttr "id")
        |> map2 (|>) (stringAttr "name")
        |> map2 (|>) (stringAttr "size")
        |> map2 (|>) (stringAttr "start")
        |> requiredPath [ "memory-segment"] (list memorySegmentDecoder)

--<peripherals>
--        <module id="I2106" name="AC">
--          <instance name="AC0">
--            <register-group address-space="data" name="AC0" name-in-module="AC" offset="0x0670"/>
--            <signals>
--              <signal function="AC0" group="N" index="0" pad="PA6"/>
--              <signal function="AC0" group="OUT" index="0" pad="PA5"/>
--              <signal function="AC0" group="P" index="0" pad="PA7"/>
--            </signals>
--          </instance>
--        </module>
type alias Signal =
    { function : String
    , group : String
    , index : String
    , pad : String
    }

signalDecoder : Decoder Signal
signalDecoder =
    succeed Signal
        |> map2 (|>) (stringAttr "function")
        |> map2 (|>) (stringAttr "group")
        |> map2 (|>) (stringAttr "index")
        |> map2 (|>) (stringAttr "pad")

type alias RegisterGroupReference =
    { addressSpace : String
    , name : String
    , nameInModule: String
    , offset: String
    }

registerGroupDecoder : Decoder RegisterGroupReference
registerGroupDecoder =
    succeed RegisterGroupReference
        |> map2 (|>) (stringAttr "address-space")
        |> map2 (|>) (stringAttr "name")
        |> map2 (|>) (stringAttr "name-in-module")
        |> map2 (|>) (stringAttr "offset")

type alias Instance =
    { name : String
    , registerGroup : Maybe RegisterGroupReference
    , signals : Maybe (List Signal)
    }

instanceDecoder : Decoder Instance
instanceDecoder =
    succeed Instance
        |> map2 (|>) (stringAttr "name")
        |> possiblePath [ "register-group" ] (single registerGroupDecoder)
        |> possiblePath [ "signals", "signal" ] (list signalDecoder)

type alias ModuleReference =
    { id : String
    , name : String
    , instances: List Instance
    }

moduleReferenceDecoder : Decoder ModuleReference
moduleReferenceDecoder =
    succeed ModuleReference
        |> map2 (|>) (stringAttr "id")
        |> map2 (|>) (stringAttr "name")
        |> requiredPath [ "instance" ] (list instanceDecoder)

--<interrupts>
--        <interrupt index="1" module-instance="CRCSCAN" name="NMI"/>
--   <interfaces>
--        <interface name="UPDI" type="updi"/>
--      </interfaces>
--      <property-groups>
--        <property-group name="OCD_FEATURES">
--          <property name="BREAK_PIN" value="PA1"/>

type alias Interrupt =
    { index : String
    , moduleInstance : String
    , name : String
    }

interruptDecoder : Decoder Interrupt
interruptDecoder =
    succeed Interrupt
        |> map2 (|>) (stringAttr "index")
        |> map2 (|>) (stringAttr "module-instance")
        |> map2 (|>) (stringAttr "name")


type alias Property =
    { name : String
    , value : String
    }


propertyDecoder : Decoder Property
propertyDecoder =
    succeed Property
        |> map2 (|>) (stringAttr "name")
        |> map2 (|>) (stringAttr "value")

type alias PropertyGroup =
    { name : String
    , properties : List Property
    }

propertyGroupDecoder : Decoder PropertyGroup
propertyGroupDecoder =
    succeed PropertyGroup
        |> map2 (|>) (stringAttr "name")
        |> requiredPath [ "property" ] (list propertyDecoder)

type alias Interface =
    { name : String
    , interfaceType : String
    }


interfaceDecoder : Decoder Interface
interfaceDecoder =
    succeed Interface
        |> map2 (|>) (stringAttr "name")
        |> map2 (|>) (stringAttr "type")

type alias Device =
    { addressSpaces : List AddressSpace
    , modulesReferences : List ModuleReference
    , interrupts : List Interrupt
    , interfaces : List Interface
    , propertyGroups : List PropertyGroup
    }

deviceDecoder : Decoder Device
deviceDecoder =
    succeed Device
        |> requiredPath [ "address-spaces", "address-space" ] (list addressSpaceDecoder)
        |> requiredPath [ "peripherals", "module" ] (list moduleReferenceDecoder)
        |> requiredPath [ "interrupts", "interrupt" ] (list interruptDecoder)
        |> requiredPath [ "interfaces", "interface" ] (list interfaceDecoder)
        |> requiredPath [ "property-groups", "property-group" ] (list propertyGroupDecoder)

--<module caption="Analog Comparator" id="I2106" name="AC">
--      <register-group caption="Analog Comparator" name="AC" size="0x8">
--        <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
--          <bitfield caption="Enable" mask="0x1" name="ENABLE" rw="RW"/>
--          <bitfield caption="Hysteresis Mode" mask="0x6" name="HYSMODE" rw="RW" values="AC_HYSMODE"/>
--          <bitfield caption="Interrupt Mode" mask="0x30" name="INTMODE" rw="RW" values="AC_INTMODE"/>
--          <bitfield caption="Low Power Mode" mask="0x8" name="LPMODE" rw="RW" values="AC_LPMODE"/>
--          <bitfield caption="Output Buffer Enable" mask="0x40" name="OUTEN" rw="RW"/>
--          <bitfield caption="Run in Standby Mode" mask="0x80" name="RUNSTDBY" rw="RW"/>
--        </register>
--        <register caption="Interrupt Control" initval="0x00" name="INTCTRL" offset="0x6" rw="RW" size="1">
--          <bitfield caption="Analog Comparator 0 Interrupt Enable" mask="0x1" name="CMP" rw="RW"/>
--        </register>
--        <register caption="Mux Control A" initval="0x00" name="MUXCTRLA" offset="0x2" rw="RW" size="1">
--          <bitfield caption="Invert AC Output" mask="0x80" name="INVERT" rw="RW"/>
--          <bitfield caption="Negative Input MUX Selection" mask="0x3" name="MUXNEG" rw="RW" values="AC_MUXNEG"/>
--          <bitfield caption="Positive Input MUX Selection" mask="0x8" name="MUXPOS" rw="RW" values="AC_MUXPOS"/>
--        </register>
--        <register caption="Status" initval="0x00" name="STATUS" offset="0x7" rw="RW" size="1">
--          <bitfield caption="Analog Comparator Interrupt Flag" mask="0x1" name="CMP" rw="RW"/>
--          <bitfield caption="Analog Comparator State" mask="0x10" name="STATE" rw="R"/>
--        </register>
--      </register-group>
--      <value-group caption="Hysteresis Mode select" name="AC_HYSMODE">
--        <value caption="No hysteresis" name="OFF" value="0x00"/>
--        <value caption="10mV hysteresis" name="10mV" value="0x01"/>
--        <value caption="25mV hysteresis" name="25mV" value="0x02"/>
--        <value caption="50mV hysteresis" name="50mV" value="0x03"/>
--      </value-group>
--    </module>

type alias Bitfield =
    { caption : String
    , mask : String
    , name : String
    , rw : String
    }

bitfieldDecoder : Decoder Bitfield
bitfieldDecoder =
    succeed Bitfield
        |> map2 (|>) (stringAttr "caption")
        |> map2 (|>) (stringAttr "mask")
        |> map2 (|>) (stringAttr "name")
        |> map2 (|>) (stringAttr "rw")

type alias ModuleRegister =
    { caption : String
    , initval : String
    , name : String
    , offset : String
    , rw : String
    , size : String
    , bitfields : List Bitfield
    }

moduleRegisterDecoder : Decoder ModuleRegister
moduleRegisterDecoder =
    succeed ModuleRegister
        |> map2 (|>) (stringAttr "caption")
        |> map2 (|>) (stringAttr "initval")
        |> map2 (|>) (stringAttr "name")
        |> map2 (|>) (stringAttr "offset")
        |> map2 (|>) (stringAttr "rw")
        |> map2 (|>) (stringAttr "size")
        |> requiredPath [ "bitfields" ] (list bitfieldDecoder)


type alias ModuleRegisterGroup =
    { caption : String
    , name : String
    , size : String
    }

moduleRegisterGroupDecoder : Decoder ModuleRegisterGroup
moduleRegisterGroupDecoder =
    succeed ModuleRegisterGroup
        |> map2 (|>) (stringAttr "caption")
        |> map2 (|>) (stringAttr "name")
        |> map2 (|>) (stringAttr "size")

type alias ModuleValue =
    { caption : String
    , name : String
    , value : String
    }

moduleValueDecoder : Decoder ModuleValue
moduleValueDecoder =
    succeed ModuleValue
        |> map2 (|>) (stringAttr "caption")
        |> map2 (|>) (stringAttr "name")
        |> map2 (|>) (stringAttr "value")

type alias ModuleValueGroup =
    { caption : String
    , name : String
    , moduleValues : List ModuleValue
    }

moduleValueGroupDecoder : Decoder ModuleValueGroup
moduleValueGroupDecoder =
    succeed ModuleValueGroup
        |> map2 (|>) (stringAttr "caption")
        |> map2 (|>) (stringAttr "name")
        |> requiredPath [ "module-values", "module-value" ]  (list moduleValueDecoder)

type alias Module =
    { registerGroups : List ModuleRegisterGroup
    , valueGroups : List ModuleValueGroup
    }

moduleDecoder : Decoder Module
moduleDecoder =
    succeed Module
        |> requiredPath [ "register-group" ] (list moduleRegisterGroupDecoder)
        |> requiredPath [ "value-group" ] (list moduleValueGroupDecoder)

type alias Variant =
    { orderCode : String
    , package : String
    , pinout : String
    , speedMax : String
    , tempMax : String
    , tempMin : String
    , vccMax : String
    , vccMin : String
    }

variantDecoder : Decoder Variant
variantDecoder =
    succeed Variant
        |> map2 (|>) (stringAttr "ordercode")
        |> map2 (|>) (stringAttr "package")
        |> map2 (|>) (stringAttr "pinout")
        |> map2 (|>) (stringAttr "speedmax")
        |> map2 (|>) (stringAttr "tempmax")
        |> map2 (|>) (stringAttr "tempmin")
        |> map2 (|>) (stringAttr "vccmax")
        |> map2 (|>) (stringAttr "vccmin")

type alias Atdf =
    { variants : List Variant
    , devices : List Device
    , modules : List Module
    }



atdfDecoder : Decoder Atdf
atdfDecoder =
    succeed Atdf
        |> requiredPath [ "variants", "variant" ] (list variantDecoder)
        |> requiredPath [ "devices", "device" ] (list deviceDecoder)
        |> requiredPath [ "modules", "module" ] (list moduleDecoder)


main = text (toString (run atdfDecoder """<?xml version="1.0" encoding="UTF-8"?><avr-tools-device-file xmlns:NumHelper="NumHelper" xmlns:xalan="http://xml.apache.org/xalan" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" schema-version="0.3" xsi:noNamespaceSchemaLocation="schema/avr_tools_device_file.xsd">
                                            <variants>
                                              <variant ordercode="ATtiny814-SSFR" package="SOIC14" pinout="SOIC14" speedmax="20000000" tempmax="125" tempmin="-40" vccmax="5.5" vccmin="1.8"/>
                                              <variant ordercode="ATtiny814-SSNR" package="SOIC14" pinout="SOIC14" speedmax="20000000" tempmax="105" tempmin="-40" vccmax="5.5" vccmin="1.8"/>
                                              <variant ordercode="ATtiny814-SSNRES" package="SOIC14" pinout="SOIC14" speedmax="20000000" tempmax="105" tempmin="-40" vccmax="5.5" vccmin="1.8"/>
                                            </variants>
                                            <devices>
                                              <device architecture="AVR8X" family="AVR TINY" name="ATtiny814">
                                                <address-spaces>
                                                  <address-space endianness="little" id="data" name="data" size="0xA000" start="0x0000">
                                                    <memory-segment exec="0" name="EEPROM" pagesize="0x20" rw="RW" size="0x0080" start="0x00001400" type="eeprom"/>
                                                    <memory-segment exec="0" name="FUSES" pagesize="0x20" rw="RW" size="0xA" start="0x00001280" type="fuses"/>
                                                    <memory-segment exec="0" name="INTERNAL_SRAM" rw="RW" size="0x0200" start="0x3e00" type="ram"/>
                                                    <memory-segment exec="0" name="IO" rw="RW" size="0x1100" start="0x00000000" type="io"/>
                                                    <memory-segment exec="0" name="LOCKBITS" pagesize="0x20" rw="RW" size="0x1" start="0x0000128A" type="lockbits"/>
                                                    <memory-segment exec="0" name="MAPPED_PROGMEM" pagesize="0x40" rw="RW" size="0x2000" start="0x00008000" type="other"/>
                                                    <memory-segment exec="0" name="PROD_SIGNATURES" pagesize="0x40" rw="R" size="0x3D" start="0x00001103" type="signatures"/>
                                                    <memory-segment exec="0" name="SIGNATURES" pagesize="0x40" rw="R" size="0x3" start="0x00001100" type="signatures"/>
                                                    <memory-segment exec="0" name="USER_SIGNATURES" pagesize="0x20" rw="RW" size="0x20" start="0x00001300" type="user_signatures"/>
                                                  </address-space>
                                                  <address-space endianness="little" id="prog" name="prog" size="0x2000" start="0x0000">
                                                    <memory-segment exec="1" name="PROGMEM" pagesize="0x40" rw="RW" size="0x2000" start="0x00000000" type="flash"/>
                                                  </address-space>
                                                </address-spaces>
                                                <peripherals>
                                                  <module id="I2106" name="AC">
                                                    <instance name="AC0">
                                                      <register-group address-space="data" name="AC0" name-in-module="AC" offset="0x0670"/>
                                                      <signals>
                                                        <signal function="AC0" group="N" index="0" pad="PA6"/>
                                                        <signal function="AC0" group="OUT" index="0" pad="PA5"/>
                                                        <signal function="AC0" group="P" index="0" pad="PA7"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2132" name="ADC">
                                                    <instance name="ADC0">
                                                      <register-group address-space="data" name="ADC0" name-in-module="ADC" offset="0x0600"/>
                                                      <signals>
                                                        <signal function="AIN0" group="AIN" index="0" pad="PA0"/>
                                                        <signal function="AIN0" group="AIN" index="1" pad="PA1"/>
                                                        <signal function="AIN0" group="AIN" index="2" pad="PA2"/>
                                                        <signal function="AIN0" group="AIN" index="3" pad="PA3"/>
                                                        <signal function="AIN0" group="AIN" index="4" pad="PA4"/>
                                                        <signal function="AIN0" group="AIN" index="5" pad="PA5"/>
                                                        <signal function="AIN0" group="AIN" index="6" pad="PA6"/>
                                                        <signal function="AIN0" group="AIN" index="7" pad="PA7"/>
                                                        <signal function="AIN0" group="AIN" index="10" pad="PB1"/>
                                                        <signal function="AIN0" group="AIN" index="11" pad="PB0"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2114" name="BOD">
                                                    <instance name="BOD">
                                                      <register-group address-space="data" name="BOD" name-in-module="BOD" offset="0x0080"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2128" name="CCL">
                                                    <instance name="CCL">
                                                      <register-group address-space="data" name="CCL" name-in-module="CCL" offset="0x01C0"/>
                                                      <signals>
                                                        <signal function="CCL" group="LUT0_IN" index="0" pad="PA0"/>
                                                        <signal function="CCL" group="LUT0_IN" index="1" pad="PA1"/>
                                                        <signal function="CCL" group="LUT0_IN" index="2" pad="PA2"/>
                                                        <signal field="PORTMUX.CTRLA.LUT0" function="CCL" group="LUT0_OUT" index="0" pad="PA4"/>
                                                        <signal field="PORTMUX.CTRLA.LUT1" function="CCL" group="LUT1_OUT" index="0" pad="PA7"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="CLKCTRL">
                                                    <instance name="CLKCTRL">
                                                      <register-group address-space="data" name="CLKCTRL" name-in-module="CLKCTRL" offset="0x0060"/>
                                                      <signals>
                                                        <signal function="CLKCTRL" group="CLKI" pad="PA3"/>
                                                        <signal function="CLKCTRL" group="TOSC1" pad="PB3"/>
                                                        <signal function="CLKCTRL" group="TOSC2" pad="PB2"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2100" name="CPU">
                                                    <instance name="CPU">
                                                      <register-group address-space="data" name="CPU" name-in-module="CPU" offset="0x0030"/>
                                                      <signals>
                                                        <signal field="PORTMUX.CTRLA.EXTBRK" function="BREAK" group="BREAK" pad="PA1"/>
                                                      </signals>
                                                      <parameters>
                                                        <param name="CORE_VERSION" value="V4"/>
                                                      </parameters>
                                                    </instance>
                                                  </module>
                                                  <module id="I2104" name="CPUINT">
                                                    <instance name="CPUINT">
                                                      <register-group address-space="data" name="CPUINT" name-in-module="CPUINT" offset="0x0110"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2122" name="CRCSCAN">
                                                    <instance name="CRCSCAN">
                                                      <register-group address-space="data" name="CRCSCAN" name-in-module="CRCSCAN" offset="0x0120"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2121" name="DAC">
                                                    <instance name="DAC0">
                                                      <register-group address-space="data" name="DAC0" name-in-module="DAC" offset="0x0680"/>
                                                      <signals>
                                                        <signal function="DAC0" group="OUT" index="0" pad="PA6"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="EVSYS">
                                                    <instance name="EVSYS">
                                                      <register-group address-space="data" name="EVSYS" name-in-module="EVSYS" offset="0x0180"/>
                                                      <signals>
                                                        <signal field="EVSYS.ASYNCCH0.ASYNCCH0" function="EVAINCH0" group="EVAPA" index="0" pad="PA0"/>
                                                        <signal field="EVSYS.ASYNCCH0.ASYNCCH0" function="EVAINCH0" group="EVAPA" index="1" pad="PA1"/>
                                                        <signal field="EVSYS.ASYNCCH0.ASYNCCH0" function="EVAINCH0" group="EVAPA" index="2" pad="PA2"/>
                                                        <signal field="EVSYS.ASYNCCH0.ASYNCCH0" function="EVAINCH0" group="EVAPA" index="3" pad="PA3"/>
                                                        <signal field="EVSYS.ASYNCCH0.ASYNCCH0" function="EVAINCH0" group="EVAPA" index="4" pad="PA4"/>
                                                        <signal field="EVSYS.ASYNCCH0.ASYNCCH0" function="EVAINCH0" group="EVAPA" index="5" pad="PA5"/>
                                                        <signal field="EVSYS.ASYNCCH0.ASYNCCH0" function="EVAINCH0" group="EVAPA" index="6" pad="PA6"/>
                                                        <signal field="EVSYS.ASYNCCH0.ASYNCCH0" function="EVAINCH0" group="EVAPA" index="7" pad="PA7"/>
                                                        <signal field="EVSYS.ASYNCCH1.ASYNCCH1" function="EVAINCH1" group="EVAPB" index="0" pad="PB0"/>
                                                        <signal field="EVSYS.ASYNCCH1.ASYNCCH1" function="EVAINCH1" group="EVAPB" index="1" pad="PB1"/>
                                                        <signal field="EVSYS.ASYNCCH1.ASYNCCH1" function="EVAINCH1" group="EVAPB" index="2" pad="PB2"/>
                                                        <signal field="EVSYS.ASYNCCH1.ASYNCCH1" function="EVAINCH1" group="EVAPB" index="3" pad="PB3"/>
                                                        <signal field="PORTMUX.CTRLA.EVOUT0" function="EVSYS" group="EVOUT" index="0" pad="PA2"/>
                                                        <signal field="PORTMUX.CTRLA.EVOUT1" function="EVSYS" group="EVOUT" index="1" pad="PB2"/>
                                                        <signal field="EVSYS.SYNCCH0.SYNCCH0" function="EVSINCH0" group="EVSPA" index="0" pad="PA0"/>
                                                        <signal field="EVSYS.SYNCCH0.SYNCCH0" function="EVSINCH0" group="EVSPA" index="1" pad="PA1"/>
                                                        <signal field="EVSYS.SYNCCH0.SYNCCH0" function="EVSINCH0" group="EVSPA" index="2" pad="PA2"/>
                                                        <signal field="EVSYS.SYNCCH0.SYNCCH0" function="EVSINCH0" group="EVSPA" index="3" pad="PA3"/>
                                                        <signal field="EVSYS.SYNCCH0.SYNCCH0" function="EVSINCH0" group="EVSPA" index="4" pad="PA4"/>
                                                        <signal field="EVSYS.SYNCCH0.SYNCCH0" function="EVSINCH0" group="EVSPA" index="5" pad="PA5"/>
                                                        <signal field="EVSYS.SYNCCH0.SYNCCH0" function="EVSINCH0" group="EVSPA" index="6" pad="PA6"/>
                                                        <signal field="EVSYS.SYNCCH0.SYNCCH0" function="EVSINCH0" group="EVSPA" index="7" pad="PA7"/>
                                                        <signal field="EVSYS.SYNCCH1.SYNCCH1" function="EVSINCH1" group="EVSPB" index="0" pad="PB0"/>
                                                        <signal field="EVSYS.SYNCCH1.SYNCCH1" function="EVSINCH1" group="EVSPB" index="1" pad="PB1"/>
                                                        <signal field="EVSYS.SYNCCH1.SYNCCH1" function="EVSINCH1" group="EVSPB" index="2" pad="PB2"/>
                                                        <signal field="EVSYS.SYNCCH1.SYNCCH1" function="EVSINCH1" group="EVSPB" index="3" pad="PB3"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="FUSE">
                                                    <instance name="FUSE">
                                                      <register-group address-space="data" name="FUSE" name-in-module="FUSE" offset="0x1280"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="GPIO">
                                                    <instance name="GPIO">
                                                      <register-group address-space="data" name="GPIO" name-in-module="GPIO" offset="0x001C"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="LOCKBIT">
                                                    <instance name="LOCKBIT">
                                                      <register-group address-space="data" name="LOCKBIT" name-in-module="LOCKBIT" offset="0x128A"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2109" name="NVMCTRL">
                                                    <instance name="NVMCTRL">
                                                      <register-group address-space="data" name="NVMCTRL" name-in-module="NVMCTRL" offset="0x1000"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2103" name="PORT">
                                                    <instance name="PORTA">
                                                      <register-group address-space="data" name="PORTA" name-in-module="PORT" offset="0x0400"/>
                                                      <signals>
                                                        <signal function="IOPORT" group="PIN" index="0" pad="PA0"/>
                                                        <signal function="IOPORT" group="PIN" index="1" pad="PA1"/>
                                                        <signal function="IOPORT" group="PIN" index="2" pad="PA2"/>
                                                        <signal function="IOPORT" group="PIN" index="3" pad="PA3"/>
                                                        <signal function="IOPORT" group="PIN" index="4" pad="PA4"/>
                                                        <signal function="IOPORT" group="PIN" index="5" pad="PA5"/>
                                                        <signal function="IOPORT" group="PIN" index="6" pad="PA6"/>
                                                        <signal function="IOPORT" group="PIN" index="7" pad="PA7"/>
                                                      </signals>
                                                    </instance>
                                                    <instance name="PORTB">
                                                      <register-group address-space="data" name="PORTB" name-in-module="PORT" offset="0x0420"/>
                                                      <signals>
                                                        <signal function="IOPORT" group="PIN" index="0" pad="PB0"/>
                                                        <signal function="IOPORT" group="PIN" index="1" pad="PB1"/>
                                                        <signal function="IOPORT" group="PIN" index="2" pad="PB2"/>
                                                        <signal function="IOPORT" group="PIN" index="3" pad="PB3"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="PORTMUX">
                                                    <instance name="PORTMUX">
                                                      <register-group address-space="data" name="PORTMUX" name-in-module="PORTMUX" offset="0x0200"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2120" name="PTC">
                                                    <instance name="PTC">
                                                      <signals>
                                                        <signal function="PTC_DS" group="DS" index="0" pad="PB2"/>
                                                        <signal function="PTC_X" group="X" index="0" pad="PA4"/>
                                                        <signal function="PTC_X" group="X" index="1" pad="PA5"/>
                                                        <signal function="PTC_X" group="X" index="2" pad="PA6"/>
                                                        <signal function="PTC_X" group="X" index="3" pad="PA7"/>
                                                        <signal function="PTC_X" group="X" index="4" pad="PB1"/>
                                                        <signal function="PTC_X" group="X" index="5" pad="PB0"/>
                                                        <signal function="PTC_Y" group="Y" index="0" pad="PA4"/>
                                                        <signal function="PTC_Y" group="Y" index="1" pad="PA5"/>
                                                        <signal function="PTC_Y" group="Y" index="2" pad="PA6"/>
                                                        <signal function="PTC_Y" group="Y" index="3" pad="PA7"/>
                                                        <signal function="PTC_Y" group="Y" index="4" pad="PB1"/>
                                                        <signal function="PTC_Y" group="Y" index="5" pad="PB0"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2111" name="RSTCTRL">
                                                    <instance name="RSTCTRL">
                                                      <register-group address-space="data" name="RSTCTRL" name-in-module="RSTCTRL" offset="0x0040"/>
                                                      <signals>
                                                        <signal function="OTHER" group="RESET" pad="PA0"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2116" name="RTC">
                                                    <instance name="RTC">
                                                      <register-group address-space="data" name="RTC" name-in-module="RTC" offset="0x0140"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="SIGROW">
                                                    <instance name="SIGROW">
                                                      <register-group address-space="data" name="SIGROW" name-in-module="SIGROW" offset="0x1100"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2112" name="SLPCTRL">
                                                    <instance name="SLPCTRL">
                                                      <register-group address-space="data" name="SLPCTRL" name-in-module="SLPCTRL" offset="0x0050"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2107" name="SPI">
                                                    <instance name="SPI0">
                                                      <register-group address-space="data" name="SPI0" name-in-module="SPI" offset="0x0820"/>
                                                      <signals>
                                                        <signal field="PORTMUX.CTRLB.SPI0" function="SPI0" group="MISO" pad="PA2"/>
                                                        <signal field="PORTMUX.CTRLB.SPI0" function="SPI0" group="MOSI" pad="PA1"/>
                                                        <signal field="PORTMUX.CTRLB.SPI0" function="SPI0" group="SCK" pad="PA3"/>
                                                        <signal field="PORTMUX.CTRLB.SPI0" function="SPI0" group="SS" pad="PA4"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="SYSCFG">
                                                    <instance name="SYSCFG">
                                                      <register-group address-space="data" name="SYSCFG" name-in-module="SYSCFG" offset="0x0F00"/>
                                                      <signals>
                                                        <signal function="OTHER" group="UPDI" pad="PA0"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2117" name="TCA">
                                                    <instance name="TCA0">
                                                      <register-group address-space="data" name="TCA0" name-in-module="TCA" offset="0x0A00"/>
                                                      <signals>
                                                        <signal field="PORTMUX.CTRLC.TCA00" function="TCA0_ALT" group="WO" index="0" pad="PB3"/>
                                                        <signal field="PORTMUX.CTRLC.TCA00" function="TCA0" group="WO" index="0" pad="PB0"/>
                                                        <signal field="PORTMUX.CTRLC.TCA01" function="TCA0" group="WO" index="1" pad="PB1"/>
                                                        <signal field="PORTMUX.CTRLC.TCA02" function="TCA0" group="WO" index="2" pad="PB2"/>
                                                        <signal field="PORTMUX.CTRLC.TCA03" function="TCA0" group="WO" index="3" pad="PA3"/>
                                                        <signal field="PORTMUX.CTRLC.TCA04" function="TCA0" group="WO" index="4" pad="PA4"/>
                                                        <signal field="PORTMUX.CTRLC.TCA05" function="TCA0" group="WO" index="5" pad="PA5"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2119" name="TCB">
                                                    <instance name="TCB0">
                                                      <register-group address-space="data" name="TCB0" name-in-module="TCB" offset="0x0A40"/>
                                                      <signals>
                                                        <signal field="PORTMUX.CTRLD.TCB0" function="TCB0" group="WO" index="0" pad="PA5"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2129" name="TCD">
                                                    <instance name="TCD0">
                                                      <register-group address-space="data" name="TCD0" name-in-module="TCD" offset="0x0A80"/>
                                                      <signals>
                                                        <signal function="TCD0" group="WOA" pad="PA4"/>
                                                        <signal function="TCD0" group="WOB" pad="PA5"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2110" name="TWI">
                                                    <instance name="TWI0">
                                                      <register-group address-space="data" name="TWI0" name-in-module="TWI" offset="0x0810"/>
                                                      <signals>
                                                        <signal field="PORTMUX.CTRLB.TWI0" function="TWI0_ALT" group="SCL" pad="PA2"/>
                                                        <signal field="PORTMUX.CTRLB.TWI0" function="TWI0" group="SCL" pad="PB0"/>
                                                        <signal field="PORTMUX.CTRLB.TWI0" function="TWI0_ALT" group="SDA" pad="PA1"/>
                                                        <signal field="PORTMUX.CTRLB.TWI0" function="TWI0" group="SDA" pad="PB1"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2108" name="USART">
                                                    <instance name="USART0">
                                                      <register-group address-space="data" name="USART0" name-in-module="USART" offset="0x0800"/>
                                                      <signals>
                                                        <signal field="PORTMUX.CTRLB.USART0" function="USART0_ALT" group="RXD" pad="PA2"/>
                                                        <signal field="PORTMUX.CTRLB.USART0" function="USART0" group="RXD" pad="PB3"/>
                                                        <signal field="PORTMUX.CTRLB.USART0" function="USART0_ALT" group="TXD" pad="PA1"/>
                                                        <signal field="PORTMUX.CTRLB.USART0" function="USART0" group="TXD" pad="PB2"/>
                                                        <signal field="PORTMUX.CTRLB.USART0" function="USART0_ALT" group="XCK" pad="PA3"/>
                                                        <signal field="PORTMUX.CTRLB.USART0" function="USART0" group="XCK" pad="PB1"/>
                                                        <signal field="PORTMUX.CTRLB.USART0" function="USART0_ALT" group="XDIR" pad="PA4"/>
                                                        <signal field="PORTMUX.CTRLB.USART0" function="USART0" group="XDIR" pad="PB0"/>
                                                      </signals>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="USERROW">
                                                    <instance name="USERROW">
                                                      <register-group address-space="data" name="USERROW" name-in-module="USERROW" offset="0x1300"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2103" name="VPORT">
                                                    <instance name="VPORTA">
                                                      <register-group address-space="data" name="VPORTA" name-in-module="VPORT" offset="0x0000"/>
                                                    </instance>
                                                    <instance name="VPORTB">
                                                      <register-group address-space="data" name="VPORTB" name-in-module="VPORT" offset="0x0004"/>
                                                    </instance>
                                                    <instance name="VPORTC">
                                                      <register-group address-space="data" name="VPORTC" name-in-module="VPORT" offset="0x0008"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2600" name="VREF">
                                                    <instance name="VREF">
                                                      <register-group address-space="data" name="VREF" name-in-module="VREF" offset="0x00A0"/>
                                                    </instance>
                                                  </module>
                                                  <module id="I2127" name="WDT">
                                                    <instance name="WDT">
                                                      <register-group address-space="data" name="WDT" name-in-module="WDT" offset="0x0100"/>
                                                    </instance>
                                                  </module>
                                                </peripherals>
                                                <interrupts>
                                                  <interrupt index="1" module-instance="CRCSCAN" name="NMI"/>
                                                  <interrupt index="2" module-instance="BOD" name="VLM"/>
                                                  <interrupt index="3" module-instance="PORTA" name="PORT"/>
                                                  <interrupt index="4" module-instance="PORTB" name="PORT"/>
                                                  <interrupt index="6" module-instance="RTC" name="CNT"/>
                                                  <interrupt index="7" module-instance="RTC" name="PIT"/>
                                                  <interrupt index="8" module-instance="TCA0" name="LUNF"/>
                                                  <interrupt index="8" module-instance="TCA0" name="OVF"/>
                                                  <interrupt index="9" module-instance="TCA0" name="HUNF"/>
                                                  <interrupt index="10" module-instance="TCA0" name="CMP0"/>
                                                  <interrupt index="10" module-instance="TCA0" name="LCMP0"/>
                                                  <interrupt index="11" module-instance="TCA0" name="CMP1"/>
                                                  <interrupt index="11" module-instance="TCA0" name="LCMP1"/>
                                                  <interrupt index="12" module-instance="TCA0" name="CMP2"/>
                                                  <interrupt index="12" module-instance="TCA0" name="LCMP2"/>
                                                  <interrupt index="13" module-instance="TCB0" name="INT"/>
                                                  <interrupt index="14" module-instance="TCD0" name="OVF"/>
                                                  <interrupt index="15" module-instance="TCD0" name="TRIG"/>
                                                  <interrupt index="16" module-instance="AC0" name="AC"/>
                                                  <interrupt index="17" module-instance="ADC0" name="RESRDY"/>
                                                  <interrupt index="18" module-instance="ADC0" name="WCOMP"/>
                                                  <interrupt index="19" module-instance="TWI0" name="TWIS"/>
                                                  <interrupt index="20" module-instance="TWI0" name="TWIM"/>
                                                  <interrupt index="21" module-instance="SPI0" name="INT"/>
                                                  <interrupt index="22" module-instance="USART0" name="RXC"/>
                                                  <interrupt index="23" module-instance="USART0" name="DRE"/>
                                                  <interrupt index="24" module-instance="USART0" name="TXC"/>
                                                  <interrupt index="25" module-instance="NVMCTRL" name="EE"/>
                                                </interrupts>
                                                <interfaces>
                                                  <interface name="UPDI" type="updi"/>
                                                </interfaces>
                                                <property-groups>
                                                  <property-group name="OCD_FEATURES">
                                                    <property name="BREAK_PIN" value="PA1"/>
                                                    <property name="BREAK_PIN_ALT" value="PC4"/>
                                                  </property-group>
                                                  <property-group name="PROGRAMMING_INFO">
                                                    <property name="FUSE_ENABLED_VALUE" value="1"/>
                                                  </property-group>
                                                  <property-group name="UPDI_INTERFACE">
                                                    <property name="PROGMEM_OFFSET" value="0x00008000"/>
                                                  </property-group>
                                                  <property-group name="SIGNATURES">
                                                    <property name="SIGNATURE0" value="0x1E"/>
                                                    <property name="SIGNATURE1" value="0x93"/>
                                                    <property name="SIGNATURE2" value="0x22"/>
                                                  </property-group>
                                                </property-groups>
                                              </device>
                                            </devices>
                                            <modules>
                                              <module caption="Analog Comparator" id="I2106" name="AC">
                                                <register-group caption="Analog Comparator" name="AC" size="0x8">
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Hysteresis Mode" mask="0x6" name="HYSMODE" rw="RW" values="AC_HYSMODE"/>
                                                    <bitfield caption="Interrupt Mode" mask="0x30" name="INTMODE" rw="RW" values="AC_INTMODE"/>
                                                    <bitfield caption="Low Power Mode" mask="0x8" name="LPMODE" rw="RW" values="AC_LPMODE"/>
                                                    <bitfield caption="Output Buffer Enable" mask="0x40" name="OUTEN" rw="RW"/>
                                                    <bitfield caption="Run in Standby Mode" mask="0x80" name="RUNSTDBY" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Control" initval="0x00" name="INTCTRL" offset="0x6" rw="RW" size="1">
                                                    <bitfield caption="Analog Comparator 0 Interrupt Enable" mask="0x1" name="CMP" rw="RW"/>
                                                  </register>
                                                  <register caption="Mux Control A" initval="0x00" name="MUXCTRLA" offset="0x2" rw="RW" size="1">
                                                    <bitfield caption="Invert AC Output" mask="0x80" name="INVERT" rw="RW"/>
                                                    <bitfield caption="Negative Input MUX Selection" mask="0x3" name="MUXNEG" rw="RW" values="AC_MUXNEG"/>
                                                    <bitfield caption="Positive Input MUX Selection" mask="0x8" name="MUXPOS" rw="RW" values="AC_MUXPOS"/>
                                                  </register>
                                                  <register caption="Status" initval="0x00" name="STATUS" offset="0x7" rw="RW" size="1">
                                                    <bitfield caption="Analog Comparator Interrupt Flag" mask="0x1" name="CMP" rw="RW"/>
                                                    <bitfield caption="Analog Comparator State" mask="0x10" name="STATE" rw="R"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Hysteresis Mode select" name="AC_HYSMODE">
                                                  <value caption="No hysteresis" name="OFF" value="0x00"/>
                                                  <value caption="10mV hysteresis" name="10mV" value="0x01"/>
                                                  <value caption="25mV hysteresis" name="25mV" value="0x02"/>
                                                  <value caption="50mV hysteresis" name="50mV" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Interrupt Mode select" name="AC_INTMODE">
                                                  <value caption="Any Edge" name="BOTHEDGE" value="0x00"/>
                                                  <value caption="Negative Edge" name="NEGEDGE" value="0x02"/>
                                                  <value caption="Positive Edge" name="POSEDGE" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Low Power Mode select" name="AC_LPMODE">
                                                  <value caption="Low power mode disabled" name="DIS" value="0x0"/>
                                                  <value caption="Low power mode enabled" name="EN" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Negative Input MUX Selection select" name="AC_MUXNEG">
                                                  <value caption="Negative Pin 0" name="PIN0" value="0x00"/>
                                                  <value caption="Negative Pin 1" name="PIN1" value="0x01"/>
                                                  <value caption="Voltage Reference" name="VREF" value="0x02"/>
                                                  <value caption="DAC output" name="DAC" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Positive Input MUX Selection select" name="AC_MUXPOS">
                                                  <value caption="Positive Pin 0" name="PIN0" value="0x00"/>
                                                  <value caption="Positive Pin 1" name="PIN1" value="0x01"/>
                                                </value-group>
                                              </module>
                                              <module caption="Analog to Digital Converter" id="I2132" name="ADC">
                                                <register-group caption="Analog to Digital Converter" name="ADC" size="0x18">
                                                  <register caption="Calibration" initval="0x00" name="CALIB" offset="0x16" rw="RW" size="1">
                                                    <bitfield caption="Duty Cycle" mask="0x1" name="DUTYCYC" rw="RW" values="ADC_DUTYCYC"/>
                                                  </register>
                                                  <register caption="Command" initval="0x00" name="COMMAND" offset="0x08" rw="RW" size="1">
                                                    <bitfield caption="Start Conversion Operation" mask="0x1" name="STCONV" rw="RW"/>
                                                  </register>
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x00" rw="RW" size="1">
                                                    <bitfield caption="ADC Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="ADC Freerun mode" mask="0x2" name="FREERUN" rw="RW"/>
                                                    <bitfield caption="ADC Resolution" mask="0x4" name="RESSEL" rw="RW" values="ADC_RESSEL"/>
                                                    <bitfield caption="Run standby mode" mask="0x80" name="RUNSTBY" rw="RW"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x01" rw="RW" size="1">
                                                    <bitfield caption="Accumulation Samples" mask="0x7" name="SAMPNUM" rw="RW" values="ADC_SAMPNUM"/>
                                                  </register>
                                                  <register caption="Control C" initval="0x00" name="CTRLC" offset="0x02" rw="RW" size="1">
                                                    <bitfield caption="Clock Pre-scaler" mask="0x7" name="PRESC" rw="RW" values="ADC_PRESC"/>
                                                    <bitfield caption="Reference Selection" mask="0x30" name="REFSEL" rw="RW" values="ADC_REFSEL"/>
                                                    <bitfield caption="Sample Capacitance Selection" mask="0x40" name="SAMPCAP" rw="RW"/>
                                                  </register>
                                                  <register caption="Control D" initval="0x00" name="CTRLD" offset="0x03" rw="RW" size="1">
                                                    <bitfield caption="Automatic Sampling Delay Variation" mask="0x10" name="ASDV" rw="RW" values="ADC_ASDV"/>
                                                    <bitfield caption="Initial Delay Selection" mask="0xe0" name="INITDLY" rw="RW" values="ADC_INITDLY"/>
                                                    <bitfield caption="Sampling Delay Selection" mask="0xf" name="SAMPDLY" rw="RW"/>
                                                  </register>
                                                  <register caption="Control E" initval="0x00" name="CTRLE" offset="0x04" rw="RW" size="1">
                                                    <bitfield caption="Window Comparator Mode" mask="0x7" name="WINCM" rw="RW" values="ADC_WINCM"/>
                                                  </register>
                                                  <register caption="Debug Control" initval="0x00" name="DBGCTRL" offset="0x0C" rw="RW" size="1">
                                                    <bitfield caption="Debug run" mask="0x1" name="DBGRUN" rw="RW"/>
                                                  </register>
                                                  <register caption="Event Control" initval="0x00" name="EVCTRL" offset="0x09" rw="RW" size="1">
                                                    <bitfield caption="Start Event Input Enable" mask="0x1" name="STARTEI" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Control" initval="0x00" name="INTCTRL" offset="0x0A" rw="RW" size="1">
                                                    <bitfield caption="Result Ready Interrupt Enable" mask="0x1" name="RESRDY" rw="RW"/>
                                                    <bitfield caption="Window Comparator Interrupt Enable" mask="0x2" name="WCMP" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x0B" rw="RW" size="1">
                                                    <bitfield caption="Result Ready Flag" mask="0x1" name="RESRDY" rw="RW"/>
                                                    <bitfield caption="Window Comparator Flag" mask="0x2" name="WCMP" rw="RW"/>
                                                  </register>
                                                  <register caption="Positive mux input" initval="0x00" name="MUXPOS" offset="0x06" rw="RW" size="1">
                                                    <bitfield caption="Analog Channel Selection Bits" mask="0x1f" name="MUXPOS" rw="RW" values="ADC_MUXPOS"/>
                                                  </register>
                                                  <register caption="ADC Accumulator Result" name="RES" offset="0x10" rw="R" size="2"/>
                                                  <register caption="Sample Control" initval="0x00" name="SAMPCTRL" offset="0x05" rw="RW" size="1">
                                                    <bitfield caption="Sample lenght" mask="0x1f" name="SAMPLEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Temporary Data" initval="0x00" name="TEMP" offset="0x0D" rw="RW" size="1">
                                                    <bitfield caption="Temporary" mask="0xff" name="TEMP" rw="RW"/>
                                                  </register>
                                                  <register caption="Window comparator high threshold" name="WINHT" offset="0x14" rw="RW" size="2"/>
                                                  <register caption="Window comparator low threshold" name="WINLT" offset="0x12" rw="RW" size="2"/>
                                                </register-group>
                                                <value-group caption="Duty Cycle select" name="ADC_DUTYCYC">
                                                  <value caption="50% Duty cycle" name="DUTY50" value="0x0"/>
                                                  <value caption="25% Duty cycle" name="DUTY25" value="0x1"/>
                                                </value-group>
                                                <value-group caption="ADC Resolution select" name="ADC_RESSEL">
                                                  <value caption="10-bit mode" name="10BIT" value="0x0"/>
                                                  <value caption="8-bit mode" name="8BIT" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Accumulation Samples select" name="ADC_SAMPNUM">
                                                  <value caption="1 ADC sample" name="ACC1" value="0x00"/>
                                                  <value caption="Accumulate 2 samples" name="ACC2" value="0x01"/>
                                                  <value caption="Accumulate 4 samples" name="ACC4" value="0x02"/>
                                                  <value caption="Accumulate 8 samples" name="ACC8" value="0x03"/>
                                                  <value caption="Accumulate 16 samples" name="ACC16" value="0x04"/>
                                                  <value caption="Accumulate 32 samples" name="ACC32" value="0x05"/>
                                                  <value caption="Accumulate 64 samples" name="ACC64" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Clock Pre-scaler select" name="ADC_PRESC">
                                                  <value caption="CLK_PER divided by 2" name="DIV2" value="0x00"/>
                                                  <value caption="CLK_PER divided by 4" name="DIV4" value="0x01"/>
                                                  <value caption="CLK_PER divided by 8" name="DIV8" value="0x02"/>
                                                  <value caption="CLK_PER divided by 16" name="DIV16" value="0x03"/>
                                                  <value caption="CLK_PER divided by 32" name="DIV32" value="0x04"/>
                                                  <value caption="CLK_PER divided by 64" name="DIV64" value="0x05"/>
                                                  <value caption="CLK_PER divided by 128" name="DIV128" value="0x06"/>
                                                  <value caption="CLK_PER divided by 256" name="DIV256" value="0x07"/>
                                                </value-group>
                                                <value-group caption="Reference Selection select" name="ADC_REFSEL">
                                                  <value caption="Internal reference" name="INTREF" value="0x00"/>
                                                  <value caption="VDD" name="VDDREF" value="0x01"/>
                                                </value-group>
                                                <value-group caption="Automatic Sampling Delay Variation select" name="ADC_ASDV">
                                                  <value caption="The Automatic Sampling Delay Variation is disabled" name="ASVOFF" value="0x0"/>
                                                  <value caption="The Automatic Sampling Delay Variation is enabled" name="ASVON" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Initial Delay Selection select" name="ADC_INITDLY">
                                                  <value caption="Delay 0 CLK_ADC cycles" name="DLY0" value="0x00"/>
                                                  <value caption="Delay 16 CLK_ADC cycles" name="DLY16" value="0x01"/>
                                                  <value caption="Delay 32 CLK_ADC cycles" name="DLY32" value="0x02"/>
                                                  <value caption="Delay 64 CLK_ADC cycles" name="DLY64" value="0x03"/>
                                                  <value caption="Delay 128 CLK_ADC cycles" name="DLY128" value="0x04"/>
                                                  <value caption="Delay 256 CLK_ADC cycles" name="DLY256" value="0x05"/>
                                                </value-group>
                                                <value-group caption="Window Comparator Mode select" name="ADC_WINCM">
                                                  <value caption="No Window Comparison" name="NONE" value="0x00"/>
                                                  <value caption="Below Window" name="BELOW" value="0x01"/>
                                                  <value caption="Above Window" name="ABOVE" value="0x02"/>
                                                  <value caption="Inside Window" name="INSIDE" value="0x03"/>
                                                  <value caption="Outside Window" name="OUTSIDE" value="0x04"/>
                                                </value-group>
                                                <value-group caption="Analog Channel Selection Bits select" name="ADC_MUXPOS">
                                                  <value caption="ADC input pin 0" name="AIN0" value="0x00"/>
                                                  <value caption="ADC input pin 1" name="AIN1" value="0x01"/>
                                                  <value caption="ADC input pin 2" name="AIN2" value="0x02"/>
                                                  <value caption="ADC input pin 3" name="AIN3" value="0x03"/>
                                                  <value caption="ADC input pin 4" name="AIN4" value="0x04"/>
                                                  <value caption="ADC input pin 5" name="AIN5" value="0x05"/>
                                                  <value caption="ADC input pin 6" name="AIN6" value="0x06"/>
                                                  <value caption="ADC input pin 7" name="AIN7" value="0x07"/>
                                                  <value caption="ADC input pin 8" name="AIN8" value="0x08"/>
                                                  <value caption="ADC input pin 9" name="AIN9" value="0x09"/>
                                                  <value caption="ADC input pin 10" name="AIN10" value="0x0A"/>
                                                  <value caption="ADC input pin 11" name="AIN11" value="0x0B"/>
                                                  <value caption="DAC0" name="DAC0" value="0x1C"/>
                                                  <value caption="Internal Ref" name="INTREF" value="0x1D"/>
                                                  <value caption="Temp sensor" name="TEMPSENSE" value="0x1E"/>
                                                  <value caption="GND" name="GND" value="0x1F"/>
                                                </value-group>
                                              </module>
                                              <module caption="Bod interface" id="I2114" name="BOD">
                                                <register-group caption="Bod interface" name="BOD" size="0x10">
                                                  <register caption="Control A" initval="0x05" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Operation in active mode" mask="0xc" name="ACTIVE" rw="R" values="BOD_ACTIVE"/>
                                                    <bitfield caption="Sample frequency" mask="0x10" name="SAMPFREQ" rw="R" values="BOD_SAMPFREQ"/>
                                                    <bitfield caption="Operation in sleep mode" mask="0x3" name="SLEEP" rw="RW" values="BOD_SLEEP"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="Bod level" mask="0x7" name="LVL" rw="R" values="BOD_LVL"/>
                                                  </register>
                                                  <register caption="Voltage level monitor interrupt Control" initval="0x00" name="INTCTRL" offset="0x9" rw="RW" size="1">
                                                    <bitfield caption="Configuration" mask="0x6" name="VLMCFG" rw="RW" values="BOD_VLMCFG"/>
                                                    <bitfield caption="voltage level monitor interrrupt enable" mask="0x1" name="VLMIE" rw="RW"/>
                                                  </register>
                                                  <register caption="Voltage level monitor interrupt Flags" initval="0x00" name="INTFLAGS" offset="0xA" rw="RW" size="1">
                                                    <bitfield caption="Voltage level monitor interrupt flag" mask="0x1" name="VLMIF" rw="RW"/>
                                                  </register>
                                                  <register caption="Voltage level monitor status" initval="0x00" name="STATUS" offset="0xB" rw="RW" size="1">
                                                    <bitfield caption="Voltage level monitor status" mask="0x1" name="VLMS" rw="R"/>
                                                  </register>
                                                  <register caption="Voltage level monitor Control" initval="0x00" name="VLMCTRLA" offset="0x8" rw="RW" size="1">
                                                    <bitfield caption="voltage level monitor level" mask="0x3" name="VLMLVL" rw="RW" values="BOD_VLMLVL"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Operation in active mode select" name="BOD_ACTIVE">
                                                  <value caption="Disabled" name="DIS" value="0x00"/>
                                                  <value caption="Enabled" name="ENABLED" value="0x01"/>
                                                  <value caption="Sampled" name="SAMPLED" value="0x02"/>
                                                  <value caption="Enabled with wakeup halt" name="ENWAKE" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Sample frequency select" name="BOD_SAMPFREQ">
                                                  <value caption="1kHz sampling" name="1KHZ" value="0x0"/>
                                                  <value caption="125Hz sampling" name="125Hz" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Operation in sleep mode select" name="BOD_SLEEP">
                                                  <value caption="Disabled" name="DIS" value="0x00"/>
                                                  <value caption="Enabled" name="ENABLED" value="0x01"/>
                                                  <value caption="Sampled" name="SAMPLED" value="0x02"/>
                                                </value-group>
                                                <value-group caption="Bod level select" name="BOD_LVL">
                                                  <value caption="1.8 V" name="BODLEVEL0" value="0x00"/>
                                                  <value caption="2.1 V" name="BODLEVEL1" value="0x01"/>
                                                  <value caption="2.6 V" name="BODLEVEL2" value="0x02"/>
                                                  <value caption="2.9 V" name="BODLEVEL3" value="0x03"/>
                                                  <value caption="3.3 V" name="BODLEVEL4" value="0x04"/>
                                                  <value caption="3.7 V" name="BODLEVEL5" value="0x05"/>
                                                  <value caption="4.0 V" name="BODLEVEL6" value="0x06"/>
                                                  <value caption="4.2 V" name="BODLEVEL7" value="0x07"/>
                                                </value-group>
                                                <value-group caption="Configuration select" name="BOD_VLMCFG">
                                                  <value caption="Interrupt when supply goes below VLM level" name="BELOW" value="0x00"/>
                                                  <value caption="Interrupt when supply goes above VLM level" name="ABOVE" value="0x01"/>
                                                  <value caption="Interrupt when supply crosses VLM level" name="CROSS" value="0x02"/>
                                                </value-group>
                                                <value-group caption="voltage level monitor level select" name="BOD_VLMLVL">
                                                  <value caption="VLM threshold 5% above BOD level" name="5ABOVE" value="0x0"/>
                                                  <value caption="VLM threshold 15% above BOD level" name="15ABOVE" value="0x1"/>
                                                  <value caption="VLM threshold 25% above BOD level" name="25ABOVE" value="0x2"/>
                                                </value-group>
                                              </module>
                                              <module caption="Configurable Custom Logic" id="I2128" name="CCL">
                                                <register-group caption="Configurable Custom Logic" name="CCL" size="0x10">
                                                  <register caption="Control Register A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Run in Standby" mask="0x40" name="RUNSTDBY" rw="RW"/>
                                                  </register>
                                                  <register caption="LUT Control 0 A" initval="0x00" name="LUT0CTRLA" offset="0x5" rw="RW" size="1">
                                                    <bitfield caption="Clock Source Selection" mask="0x40" name="CLKSRC" rw="RW"/>
                                                    <bitfield caption="Edge Detection Enable" mask="0x80" name="EDGEDET" rw="RW" values="CCL_EDGEDET"/>
                                                    <bitfield caption="LUT Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Filter Selection" mask="0x30" name="FILTSEL" rw="RW" values="CCL_FILTSEL"/>
                                                    <bitfield caption="Output Enable" mask="0x8" name="OUTEN" rw="RW"/>
                                                  </register>
                                                  <register caption="LUT Control 0 B" initval="0x00" name="LUT0CTRLB" offset="0x6" rw="RW" size="1">
                                                    <bitfield caption="LUT Input 0 Source Selection" mask="0xf" name="INSEL0" rw="RW" values="CCL_INSEL0"/>
                                                    <bitfield caption="LUT Input 1 Source Selection" mask="0xf0" name="INSEL1" rw="RW" values="CCL_INSEL1"/>
                                                  </register>
                                                  <register caption="LUT Control 0 C" initval="0x00" name="LUT0CTRLC" offset="0x7" rw="RW" size="1">
                                                    <bitfield caption="LUT Input 2 Source Selection" mask="0xf" name="INSEL2" rw="RW" values="CCL_INSEL2"/>
                                                  </register>
                                                  <register caption="LUT Control 1 A" initval="0x00" name="LUT1CTRLA" offset="0x9" rw="RW" size="1">
                                                    <bitfield caption="Clock Source Selection" mask="0x40" name="CLKSRC" rw="RW"/>
                                                    <bitfield caption="Edge Detection Enable" mask="0x80" name="EDGEDET" rw="RW" values="CCL_EDGEDET"/>
                                                    <bitfield caption="LUT Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Filter Selection" mask="0x30" name="FILTSEL" rw="RW" values="CCL_FILTSEL"/>
                                                    <bitfield caption="Output Enable" mask="0x8" name="OUTEN" rw="RW"/>
                                                  </register>
                                                  <register caption="LUT Control 1 B" initval="0x00" name="LUT1CTRLB" offset="0xA" rw="RW" size="1">
                                                    <bitfield caption="LUT Input 0 Source Selection" mask="0xf" name="INSEL0" rw="RW" values="CCL_INSEL0"/>
                                                    <bitfield caption="LUT Input 1 Source Selection" mask="0xf0" name="INSEL1" rw="RW" values="CCL_INSEL1"/>
                                                  </register>
                                                  <register caption="LUT Control 1 C" initval="0x00" name="LUT1CTRLC" offset="0xB" rw="RW" size="1">
                                                    <bitfield caption="LUT Input 2 Source Selection" mask="0xf" name="INSEL2" rw="RW" values="CCL_INSEL2"/>
                                                  </register>
                                                  <register caption="Sequential Control 0" initval="0x00" name="SEQCTRL0" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="Sequential Selection" mask="0x7" name="SEQSEL" rw="RW" values="CCL_SEQSEL"/>
                                                  </register>
                                                  <register caption="Truth 0" name="TRUTH0" offset="0x8" rw="RW" size="1"/>
                                                  <register caption="Truth 1" name="TRUTH1" offset="0xC" rw="RW" size="1"/>
                                                </register-group>
                                                <value-group caption="Edge Detection Enable select" name="CCL_EDGEDET">
                                                  <value caption="Edge detector is disabled" name="DIS" value="0x0"/>
                                                  <value caption="Edge detector is enabled" name="EN" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Filter Selection select" name="CCL_FILTSEL">
                                                  <value caption="Filter disabled" name="DISABLE" value="0x0"/>
                                                  <value caption="Synchronizer enabled" name="SYNCH" value="0x1"/>
                                                  <value caption="Filter enabled" name="FILTER" value="0x2"/>
                                                </value-group>
                                                <value-group caption="LUT Input 0 Source Selection select" name="CCL_INSEL0">
                                                  <value caption="Masked input" name="MASK" value="0x00"/>
                                                  <value caption="Feedback input source" name="FEEDBACK" value="0x01"/>
                                                  <value caption="Linked LUT input source" name="LINK" value="0x02"/>
                                                  <value caption="Event input source 0" name="EVENT0" value="0x03"/>
                                                  <value caption="Event input source 1" name="EVENT1" value="0x04"/>
                                                  <value caption="IO pin LUTn-IN0 input source" name="IO" value="0x05"/>
                                                  <value caption="AC0 OUT input source" name="AC0" value="0x06"/>
                                                  <value caption="TCB0 WO input source" name="TCB0" value="0x07"/>
                                                  <value caption="TCA0 WO0 input source" name="TCA0" value="0x08"/>
                                                  <value caption="TCD0 WOA input source" name="TCD0" value="0x09"/>
                                                  <value caption="USART0 XCK input source" name="USART0" value="0x0A"/>
                                                  <value caption="SPI0 SCK source" name="SPI0" value="0x0B"/>
                                                </value-group>
                                                <value-group caption="LUT Input 1 Source Selection select" name="CCL_INSEL1">
                                                  <value caption="Masked input" name="MASK" value="0x00"/>
                                                  <value caption="Feedback input source" name="FEEDBACK" value="0x01"/>
                                                  <value caption="Linked LUT input source" name="LINK" value="0x02"/>
                                                  <value caption="Event input source 0" name="EVENT0" value="0x03"/>
                                                  <value caption="Event input source 1" name="EVENT1" value="0x04"/>
                                                  <value caption="IO pin LUTn-N1 input source" name="IO" value="0x05"/>
                                                  <value caption="AC0 OUT input source" name="AC0" value="0x06"/>
                                                  <value caption="TCB0 WO input source" name="TCB0" value="0x07"/>
                                                  <value caption="TCA0 WO1 input source" name="TCA0" value="0x08"/>
                                                  <value caption="TCD0 WOB input source" name="TCD0" value="0x09"/>
                                                  <value caption="USART0 TXD input source" name="USART0" value="0x0A"/>
                                                  <value caption="SPI0 MOSI input source" name="SPI0" value="0x0B"/>
                                                </value-group>
                                                <value-group caption="LUT Input 2 Source Selection select" name="CCL_INSEL2">
                                                  <value caption="Masked input" name="MASK" value="0x00"/>
                                                  <value caption="Feedback input source" name="FEEDBACK" value="0x01"/>
                                                  <value caption="Linked LUT input source" name="LINK" value="0x02"/>
                                                  <value caption="Event input source 0" name="EVENT0" value="0x03"/>
                                                  <value caption="Event input source 1" name="EVENT1" value="0x04"/>
                                                  <value caption="IO pin LUTn-IN2 input source" name="IO" value="0x05"/>
                                                  <value caption="AC0 OUT input source" name="AC0" value="0x06"/>
                                                  <value caption="TCB0 WO input source" name="TCB0" value="0x07"/>
                                                  <value caption="TCA0 WO2 input source" name="TCA0" value="0x08"/>
                                                  <value caption="TCD0 WOA input source" name="TCD0" value="0x09"/>
                                                  <value caption="SPI0 MISO source" name="SPI0" value="0x0B"/>
                                                </value-group>
                                                <value-group caption="Sequential Selection select" name="CCL_SEQSEL">
                                                  <value caption="Sequential logic disabled" name="DISABLE" value="0x00"/>
                                                  <value caption="D FlipFlop" name="DFF" value="0x01"/>
                                                  <value caption="JK FlipFlop" name="JK" value="0x02"/>
                                                  <value caption="D Latch" name="LATCH" value="0x03"/>
                                                  <value caption="RS Latch" name="RS" value="0x04"/>
                                                </value-group>
                                              </module>
                                              <module caption="Clock controller" id="I2600" name="CLKCTRL">
                                                <register-group caption="Clock controller" name="CLKCTRL" size="0x20">
                                                  <register caption="MCLK Control A" initval="0x00" name="MCLKCTRLA" offset="0x00" rw="RW" size="1">
                                                    <bitfield caption="System clock out" mask="0x80" name="CLKOUT" rw="RW"/>
                                                    <bitfield caption="clock select" mask="0x3" name="CLKSEL" rw="RW" values="CLKCTRL_CLKSEL"/>
                                                  </register>
                                                  <register caption="MCLK Control B" initval="0x00" name="MCLKCTRLB" offset="0x01" rw="RW" size="1">
                                                    <bitfield caption="Prescaler division" mask="0x1e" name="PDIV" rw="RW" values="CLKCTRL_PDIV"/>
                                                    <bitfield caption="Prescaler enable" mask="0x1" name="PEN" rw="RW"/>
                                                  </register>
                                                  <register caption="MCLK Lock" name="MCLKLOCK" offset="0x02" rw="RW" size="1">
                                                    <bitfield caption="lock ebable" mask="0x1" name="LOCKEN" rw="RW"/>
                                                  </register>
                                                  <register caption="MCLK Status" initval="0x00" name="MCLKSTATUS" offset="0x03" rw="R" size="1">
                                                    <bitfield caption="External Clock status" mask="0x80" name="EXTS" rw="R"/>
                                                    <bitfield caption="20MHz oscillator status" mask="0x10" name="OSC20MS" rw="R"/>
                                                    <bitfield caption="32KHz oscillator status" mask="0x20" name="OSC32KS" rw="R"/>
                                                    <bitfield caption="System Oscillator changing" mask="0x1" name="SOSC" rw="R"/>
                                                    <bitfield caption="32.768 kHz Crystal Oscillator status" mask="0x40" name="XOSC32KS" rw="R"/>
                                                  </register>
                                                  <register caption="OSC20M Calibration A" initval="0x00" name="OSC20MCALIBA" offset="0x11" rw="RW" size="1">
                                                    <bitfield caption="Calibration" mask="0x3f" name="CAL20M" rw="RW"/>
                                                  </register>
                                                  <register caption="OSC20M Calibration B" initval="0x00" name="OSC20MCALIBB" offset="0x12" rw="RW" size="1">
                                                    <bitfield caption="Lock" mask="0x80" name="LOCK" rw="RW"/>
                                                    <bitfield caption="Oscillator temperature coefficient" mask="0xf" name="TEMPCAL20M" rw="RW"/>
                                                  </register>
                                                  <register caption="OSC20M Control A" name="OSC20MCTRLA" offset="0x10" rw="RW" size="1">
                                                    <bitfield caption="Run standby" mask="0x2" name="RUNSTDBY" rw="RW"/>
                                                  </register>
                                                  <register caption="OSC32K Control A" initval="0x00" name="OSC32KCTRLA" offset="0x18" rw="RW" size="1">
                                                    <bitfield caption="Run standby" mask="0x2" name="RUNSTDBY" rw="RW"/>
                                                  </register>
                                                  <register caption="XOSC32K Control A" initval="0x00" name="XOSC32KCTRLA" offset="0x1C" rw="RW" size="1">
                                                    <bitfield caption="Crystal startup time" mask="0x30" name="CSUT" rw="RW" values="CLKCTRL_CSUT"/>
                                                    <bitfield caption="Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Run standby" mask="0x2" name="RUNSTDBY" rw="RW"/>
                                                    <bitfield caption="Select" mask="0x4" name="SEL" rw="RW"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="clock select select" name="CLKCTRL_CLKSEL">
                                                  <value caption="20MHz internal oscillator" name="OSC20M" value="0x00"/>
                                                  <value caption="32KHz internal Ultra Low Power oscillator" name="OSCULP32K" value="0x01"/>
                                                  <value caption="32.768kHz external crystal oscillator" name="XOSC32K" value="0x02"/>
                                                  <value caption="External clock" name="EXTCLK" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Prescaler division select" name="CLKCTRL_PDIV">
                                                  <value caption="2X" name="2X" value="0x00"/>
                                                  <value caption="4X" name="4X" value="0x01"/>
                                                  <value caption="8X" name="8X" value="0x02"/>
                                                  <value caption="16X" name="16X" value="0x03"/>
                                                  <value caption="32X" name="32X" value="0x04"/>
                                                  <value caption="64X" name="64X" value="0x05"/>
                                                  <value caption="6X" name="6X" value="0x08"/>
                                                  <value caption="10X" name="10X" value="0x09"/>
                                                  <value caption="12X" name="12X" value="0x0A"/>
                                                  <value caption="24X" name="24X" value="0x0B"/>
                                                  <value caption="48X" name="48X" value="0x0C"/>
                                                </value-group>
                                                <value-group caption="Crystal startup time select" name="CLKCTRL_CSUT">
                                                  <value caption="1K cycles" name="1K" value="0x00"/>
                                                  <value caption="16K cycles" name="16K" value="0x01"/>
                                                  <value caption="32K cycles" name="32K" value="0x02"/>
                                                  <value caption="64K cycles" name="64K" value="0x03"/>
                                                </value-group>
                                              </module>
                                              <module caption="CPU" id="I2100" name="CPU">
                                                <register-group caption="CPU" name="CPU" size="0x10">
                                                  <register caption="Configuration Change Protection" initval="0x00" name="CCP" offset="0x4" rw="RW" size="1">
                                                    <bitfield caption="CCP signature" mask="0xff" name="CCP" rw="RW" values="CPU_CCP"/>
                                                  </register>
                                                  <register caption="Stack Pointer High" name="SPH" offset="0xE" rw="RW" size="1"/>
                                                  <register caption="Stack Pointer Low" name="SPL" offset="0xD" rw="RW" size="1"/>
                                                  <register caption="Status Register" initval="0x00" name="SREG" offset="0xF" rw="RW" size="1">
                                                    <bitfield caption="Carry Flag" mask="0x1" name="C" rw="RW"/>
                                                    <bitfield caption="Half Carry Flag" mask="0x20" name="H" rw="RW"/>
                                                    <bitfield caption="Global Interrupt Enable Flag" mask="0x80" name="I" rw="RW"/>
                                                    <bitfield caption="Negative Flag" mask="0x4" name="N" rw="RW"/>
                                                    <bitfield caption="N Exclusive Or V Flag" mask="0x10" name="S" rw="RW"/>
                                                    <bitfield caption="Transfer Bit" mask="0x40" name="T" rw="RW"/>
                                                    <bitfield caption="Two's Complement Overflow Flag" mask="0x8" name="V" rw="RW"/>
                                                    <bitfield caption="Zero Flag" mask="0x2" name="Z" rw="RW"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="CCP signature select" name="CPU_CCP">
                                                  <value caption="SPM Instruction Protection" name="SPM" value="0x9D"/>
                                                  <value caption="IO Register Protection" name="IOREG" value="0xD8"/>
                                                </value-group>
                                              </module>
                                              <module caption="Interrupt Controller" id="I2104" name="CPUINT">
                                                <register-group caption="Interrupt Controller" name="CPUINT" size="0x4">
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Compact Vector Table" mask="0x20" name="CVT" rw="RW"/>
                                                    <bitfield caption="Interrupt Vector Select" mask="0x40" name="IVSEL" rw="RW"/>
                                                    <bitfield caption="Round-robin Scheduling Enable" mask="0x1" name="LVL0RR" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Level 0 Priority" initval="0x00" name="LVL0PRI" offset="0x2" rw="RW" size="1">
                                                    <bitfield caption="Interrupt Level Priority" mask="0xff" name="LVL0PRI" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Level 1 Priority Vector" initval="0x00" name="LVL1VEC" offset="0x3" rw="RW" size="1">
                                                    <bitfield caption="Interrupt Vector with High Priority" mask="0xff" name="LVL1VEC" rw="RW"/>
                                                  </register>
                                                  <register caption="Status" initval="0x00" name="STATUS" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="Level 0 Interrupt Executing" mask="0x1" name="LVL0EX" rw="R"/>
                                                    <bitfield caption="Level 1 Interrupt Executing" mask="0x2" name="LVL1EX" rw="R"/>
                                                    <bitfield caption="Non-maskable Interrupt Executing" mask="0x80" name="NMIEX" rw="R"/>
                                                  </register>
                                                </register-group>
                                              </module>
                                              <module caption="CRCSCAN" id="I2122" name="CRCSCAN">
                                                <register-group caption="CRCSCAN" name="CRCSCAN" size="0x4">
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Enable CRC scan" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Enable NMI Trigger" mask="0x2" name="NMIEN" rw="RW"/>
                                                    <bitfield caption="Reset CRC scan" mask="0x80" name="RESET" rw="RW"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="CRC Flash Access Mode" mask="0x30" name="MODE" rw="RW" values="CRCSCAN_MODE"/>
                                                    <bitfield caption="CRC Source" mask="0x3" name="SRC" rw="RW" values="CRCSCAN_SRC"/>
                                                  </register>
                                                  <register caption="Status" initval="0x00" name="STATUS" offset="0x2" rw="R" size="1">
                                                    <bitfield caption="CRC Busy" mask="0x1" name="BUSY" rw="R"/>
                                                    <bitfield caption="CRC Ok" mask="0x2" name="OK" rw="R"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="CRC Flash Access Mode select" name="CRCSCAN_MODE">
                                                  <value caption="Priority to flash" name="PRIORITY" value="0x0"/>
                                                  <value caption="Reserved" name="RESERVED" value="0x1"/>
                                                  <value caption="Lowest priority to flash" name="BACKGROUND" value="0x2"/>
                                                  <value caption="Continuous checks in background" name="CONTINUOUS" value="0x3"/>
                                                </value-group>
                                                <value-group caption="CRC Source select" name="CRCSCAN_SRC">
                                                  <value caption="CRC on entire flash" name="FLASH" value="0x0"/>
                                                  <value caption="CRC on boot and appl section of flash" name="APPLICATION" value="0x1"/>
                                                  <value caption="CRC on boot section of flash" name="BOOT" value="0x2"/>
                                                </value-group>
                                              </module>
                                              <module caption="Digital to Analog Converter" id="I2121" name="DAC">
                                                <register-group caption="Digital to Analog Converter" name="DAC" size="0x4">
                                                  <register caption="Control Register A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="DAC Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Output Buffer Enable" mask="0x40" name="OUTEN" rw="RW"/>
                                                    <bitfield caption="Run in Standby Mode" mask="0x80" name="RUNSTDBY" rw="RW"/>
                                                  </register>
                                                  <register caption="DATA Register" name="DATA" offset="0x1" rw="RW" size="1"/>
                                                </register-group>
                                              </module>
                                              <module caption="Event System" id="I2600" name="EVSYS">
                                                <register-group caption="Event System" name="EVSYS" size="0x40">
                                                  <register caption="Asynchronous Channel 0 Generator Selection" initval="0x00" name="ASYNCCH0" offset="0x02" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous Channel 0 Generator Selection" mask="0xff" name="ASYNCCH0" rw="RW" values="EVSYS_ASYNCCH0"/>
                                                  </register>
                                                  <register caption="Asynchronous Channel 1 Generator Selection" initval="0x00" name="ASYNCCH1" offset="0x03" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous Channel 1 Generator Selection" mask="0xff" name="ASYNCCH1" rw="RW" values="EVSYS_ASYNCCH1"/>
                                                  </register>
                                                  <register caption="Asynchronous Channel 2 Generator Selection" initval="0x00" name="ASYNCCH2" offset="0x04" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous Channel 2 Generator Selection" mask="0xff" name="ASYNCCH2" rw="RW" values="EVSYS_ASYNCCH2"/>
                                                  </register>
                                                  <register caption="Asynchronous Channel 3 Generator Selection" initval="0x00" name="ASYNCCH3" offset="0x05" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous Channel 3 Generator Selection" mask="0xff" name="ASYNCCH3" rw="RW" values="EVSYS_ASYNCCH3"/>
                                                  </register>
                                                  <register caption="Asynchronous Channel Strobe" initval="0x00" name="ASYNCSTROBE" offset="0x00" rw="W" size="1"/>
                                                  <register caption="Asynchronous User Ch 0 Input Selection - TCB0" initval="0x00" name="ASYNCUSER0" offset="0x12" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 0 Input Selection - TCB0" mask="0xff" name="ASYNCUSER0" rw="RW" values="EVSYS_ASYNCUSER0"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 1 Input Selection - ADC0" initval="0x00" name="ASYNCUSER1" offset="0x13" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 1 Input Selection - ADC0" mask="0xff" name="ASYNCUSER1" rw="RW" values="EVSYS_ASYNCUSER1"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 2 Input Selection - CCL LUT0 Event 0" initval="0x00" name="ASYNCUSER2" offset="0x14" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 2 Input Selection - CCL LUT0 Event 0" mask="0xff" name="ASYNCUSER2" rw="RW" values="EVSYS_ASYNCUSER2"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 3 Input Selection - CCL LUT1 Event 0" initval="0x00" name="ASYNCUSER3" offset="0x15" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 3 Input Selection - CCL LUT1 Event 0" mask="0xff" name="ASYNCUSER3" rw="RW" values="EVSYS_ASYNCUSER3"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 4 Input Selection - CCL LUT0 Event 1" initval="0x00" name="ASYNCUSER4" offset="0x16" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 4 Input Selection - CCL LUT0 Event 1" mask="0xff" name="ASYNCUSER4" rw="RW" values="EVSYS_ASYNCUSER4"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 5 Input Selection - CCL LUT1 Event 1" initval="0x00" name="ASYNCUSER5" offset="0x17" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 5 Input Selection - CCL LUT1 Event 1" mask="0xff" name="ASYNCUSER5" rw="RW" values="EVSYS_ASYNCUSER5"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 6 Input Selection - TCD0 Event 0" initval="0x00" name="ASYNCUSER6" offset="0x18" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 6 Input Selection - TCD0 Event 0" mask="0xff" name="ASYNCUSER6" rw="RW" values="EVSYS_ASYNCUSER6"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 7 Input Selection - TCD0 Event 1" initval="0x00" name="ASYNCUSER7" offset="0x19" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 7 Input Selection - TCD0 Event 1" mask="0xff" name="ASYNCUSER7" rw="RW" values="EVSYS_ASYNCUSER7"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 8 Input Selection - Event Out 0" initval="0x00" name="ASYNCUSER8" offset="0x1A" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 8 Input Selection - Event Out 0" mask="0xff" name="ASYNCUSER8" rw="RW" values="EVSYS_ASYNCUSER8"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 9 Input Selection - Event Out 1" initval="0x00" name="ASYNCUSER9" offset="0x1B" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 9 Input Selection - Event Out 1" mask="0xff" name="ASYNCUSER9" rw="RW" values="EVSYS_ASYNCUSER9"/>
                                                  </register>
                                                  <register caption="Asynchronous User Ch 10 Input Selection - Event Out 2" initval="0x00" name="ASYNCUSER10" offset="0x1C" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous User Ch 10 Input Selection - Event Out 2" mask="0xff" name="ASYNCUSER10" rw="RW" values="EVSYS_ASYNCUSER10"/>
                                                  </register>
                                                  <register caption="Synchronous Channel 0 Generator Selection" initval="0x00" name="SYNCCH0" offset="0x0A" rw="RW" size="1">
                                                    <bitfield caption="Synchronous Channel 0 Generator Selection" mask="0xff" name="SYNCCH0" rw="RW" values="EVSYS_SYNCCH0"/>
                                                  </register>
                                                  <register caption="Synchronous Channel 1 Generator Selection" initval="0x00" name="SYNCCH1" offset="0x0B" rw="RW" size="1">
                                                    <bitfield caption="Synchronous Channel 1 Generator Selection" mask="0xff" name="SYNCCH1" rw="RW" values="EVSYS_SYNCCH1"/>
                                                  </register>
                                                  <register caption="Synchronous Channel Strobe" initval="0x00" name="SYNCSTROBE" offset="0x01" rw="W" size="1"/>
                                                  <register caption="Synchronous User Ch 0 Input Selection - TCA0" initval="0x00" name="SYNCUSER0" offset="0x22" rw="RW" size="1">
                                                    <bitfield caption="Synchronous User Ch 0 Input Selection - TCA0" mask="0xff" name="SYNCUSER0" rw="RW" values="EVSYS_SYNCUSER0"/>
                                                  </register>
                                                  <register caption="Synchronous User Ch 1 Input Selection - USART0" initval="0x00" name="SYNCUSER1" offset="0x23" rw="RW" size="1">
                                                    <bitfield caption="Synchronous User Ch 1 Input Selection - USART0" mask="0xff" name="SYNCUSER1" rw="RW" values="EVSYS_SYNCUSER1"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Asynchronous Channel 0 Generator Selection select" name="EVSYS_ASYNCCH0">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Configurable Custom Logic LUT0" name="CCL_LUT0" value="0x01"/>
                                                  <value caption="Configurable Custom Logic LUT1" name="CCL_LUT1" value="0x02"/>
                                                  <value caption="Analog Comparator 0 out" name="AC0_OUT" value="0x03"/>
                                                  <value caption="Timer/Counter D0 compare B clear" name="TCD0_CMPBCLR" value="0x04"/>
                                                  <value caption="Timer/Counter D0 compare A set" name="TCD0_CMPASET" value="0x05"/>
                                                  <value caption="Timer/Counter D0 compare B set" name="TCD0_CMPBSET" value="0x06"/>
                                                  <value caption="Timer/Counter D0 program event" name="TCD0_PROGEV" value="0x07"/>
                                                  <value caption="Real Time Counter overflow" name="RTC_OVF" value="0x08"/>
                                                  <value caption="Real Time Counter compare" name="RTC_CMP" value="0x09"/>
                                                  <value caption="Asynchronous Event from Pin PA0" name="PORTA_PIN0" value="0x0A"/>
                                                  <value caption="Asynchronous Event from Pin PA1" name="PORTA_PIN1" value="0x0B"/>
                                                  <value caption="Asynchronous Event from Pin PA2" name="PORTA_PIN2" value="0x0C"/>
                                                  <value caption="Asynchronous Event from Pin PA3" name="PORTA_PIN3" value="0x0D"/>
                                                  <value caption="Asynchronous Event from Pin PA4" name="PORTA_PIN4" value="0x0E"/>
                                                  <value caption="Asynchronous Event from Pin PA5" name="PORTA_PIN5" value="0x0F"/>
                                                  <value caption="Asynchronous Event from Pin PA6" name="PORTA_PIN6" value="0x10"/>
                                                  <value caption="Asynchronous Event from Pin PA7" name="PORTA_PIN7" value="0x11"/>
                                                  <value caption="Unified Program and debug interface" name="UPDI" value="0x12"/>
                                                </value-group>
                                                <value-group caption="Asynchronous Channel 1 Generator Selection select" name="EVSYS_ASYNCCH1">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Configurable custom logic LUT0" name="CCL_LUT0" value="0x01"/>
                                                  <value caption="Configurable custom logic LUT1" name="CCL_LUT1" value="0x02"/>
                                                  <value caption="Analog Comparator 0 out" name="AC0_OUT" value="0x03"/>
                                                  <value caption="Timer/Counter D0 compare B clear" name="TCD0_CMPBCLR" value="0x04"/>
                                                  <value caption="Timer/Counter D0 compare A set" name="TCD0_CMPASET" value="0x05"/>
                                                  <value caption="Timer/Counter D0 compare B set" name="TCD0_CMPBSET" value="0x06"/>
                                                  <value caption="Timer/Counter D0 program event" name="TCD0_PROGEV" value="0x07"/>
                                                  <value caption="Real Time Counter overflow" name="RTC_OVF" value="0x08"/>
                                                  <value caption="Real Time Counter compare" name="RTC_CMP" value="0x09"/>
                                                  <value caption="Asynchronous Event from Pin PB0" name="PORTB_PIN0" value="0x0A"/>
                                                  <value caption="Asynchronous Event from Pin PB1" name="PORTB_PIN1" value="0x0B"/>
                                                  <value caption="Asynchronous Event from Pin PB2" name="PORTB_PIN2" value="0x0C"/>
                                                  <value caption="Asynchronous Event from Pin PB3" name="PORTB_PIN3" value="0x0D"/>
                                                  <value caption="Asynchronous Event from Pin PB4" name="PORTB_PIN4" value="0x0E"/>
                                                  <value caption="Asynchronous Event from Pin PB5" name="PORTB_PIN5" value="0x0F"/>
                                                  <value caption="Asynchronous Event from Pin PB6" name="PORTB_PIN6" value="0x10"/>
                                                  <value caption="Asynchronous Event from Pin PB7" name="PORTB_PIN7" value="0x11"/>
                                                </value-group>
                                                <value-group caption="Asynchronous Channel 2 Generator Selection select" name="EVSYS_ASYNCCH2">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Configurable Custom Logic LUT0" name="CCL_LUT0" value="0x01"/>
                                                  <value caption="Configurable Custom Logic LUT1" name="CCL_LUT1" value="0x02"/>
                                                  <value caption="Analog Comparator 0 out" name="AC0_OUT" value="0x03"/>
                                                  <value caption="Timer/Counter D0 compare B clear" name="TCD0_CMPBCLR" value="0x04"/>
                                                  <value caption="Timer/Counter D0 compare A set" name="TCD0_CMPASET" value="0x05"/>
                                                  <value caption="Timer/Counter D0 compare B set" name="TCD0_CMPBSET" value="0x06"/>
                                                  <value caption="Timer/Counter D0 program event" name="TCD0_PROGEV" value="0x07"/>
                                                  <value caption="Real Time Counter overflow" name="RTC_OVF" value="0x08"/>
                                                  <value caption="Real Time Counter compare" name="RTC_CMP" value="0x09"/>
                                                  <value caption="Asynchronous Event from Pin PC0" name="PORTC_PIN0" value="0x0A"/>
                                                  <value caption="Asynchronous Event from Pin PC1" name="PORTC_PIN1" value="0x0B"/>
                                                  <value caption="Asynchronous Event from Pin PC2" name="PORTC_PIN2" value="0x0C"/>
                                                  <value caption="Asynchronous Event from Pin PC3" name="PORTC_PIN3" value="0x0D"/>
                                                  <value caption="Asynchronous Event from Pin PC4" name="PORTC_PIN4" value="0x0E"/>
                                                  <value caption="Asynchronous Event from Pin PC5" name="PORTC_PIN5" value="0x0F"/>
                                                </value-group>
                                                <value-group caption="Asynchronous Channel 3 Generator Selection select" name="EVSYS_ASYNCCH3">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Configurable custom logic LUT0" name="CCL_LUT0" value="0x01"/>
                                                  <value caption="Configurable custom logic LUT1" name="CCL_LUT1" value="0x02"/>
                                                  <value caption="Analog Comparator 0 out" name="AC0_OUT" value="0x03"/>
                                                  <value caption="Timer/Counter type D compare B clear" name="TCD0_CMPBCLR" value="0x04"/>
                                                  <value caption="Timer/Counter type D compare A set" name="TCD0_CMPASET" value="0x05"/>
                                                  <value caption="Timer/Counter type D compare B set" name="TCD0_CMPBSET" value="0x06"/>
                                                  <value caption="Timer/Counter type D program event" name="TCD0_PROGEV" value="0x07"/>
                                                  <value caption="Real Time Counter overflow" name="RTC_OVF" value="0x08"/>
                                                  <value caption="Real Time Counter compare" name="RTC_CMP" value="0x09"/>
                                                  <value caption="Periodic Interrupt CLK_RTC div 8192" name="PIT_DIV8192" value="0x0A"/>
                                                  <value caption="Periodic Interrupt CLK_RTC div 4096" name="PIT_DIV4096" value="0x0B"/>
                                                  <value caption="Periodic Interrupt CLK_RTC div 2048" name="PIT_DIV2048" value="0x0C"/>
                                                  <value caption="Periodic Interrupt CLK_RTC div 1024" name="PIT_DIV1024" value="0x0D"/>
                                                  <value caption="Periodic Interrupt CLK_RTC div 512" name="PIT_DIV512" value="0x0E"/>
                                                  <value caption="Periodic Interrupt CLK_RTC div 256" name="PIT_DIV256" value="0x0F"/>
                                                  <value caption="Periodic Interrupt CLK_RTC div 128" name="PIT_DIV128" value="0x10"/>
                                                  <value caption="Periodic Interrupt CLK_RTC div 64" name="PIT_DIV64" value="0x11"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 0 Input Selection - TCB0 select" name="EVSYS_ASYNCUSER0">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 1 Input Selection - ADC0 select" name="EVSYS_ASYNCUSER1">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 2 Input Selection - CCL LUT0 Event 0 select" name="EVSYS_ASYNCUSER2">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 3 Input Selection - CCL LUT1 Event 0 select" name="EVSYS_ASYNCUSER3">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 4 Input Selection - CCL LUT0 Event 1 select" name="EVSYS_ASYNCUSER4">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 5 Input Selection - CCL LUT1 Event 1 select" name="EVSYS_ASYNCUSER5">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 6 Input Selection - TCD0 Event 0 select" name="EVSYS_ASYNCUSER6">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 7 Input Selection - TCD0 Event 1 select" name="EVSYS_ASYNCUSER7">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 8 Input Selection - Event Out 0 select" name="EVSYS_ASYNCUSER8">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 9 Input Selection - Event Out 1 select" name="EVSYS_ASYNCUSER9">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Asynchronous User Ch 10 Input Selection - Event Out 2 select" name="EVSYS_ASYNCUSER10">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                  <value caption="Asynchronous Event Channel 0" name="ASYNCCH0" value="0x03"/>
                                                  <value caption="Asynchronous Event Channel 1" name="ASYNCCH1" value="0x04"/>
                                                  <value caption="Asynchronous Event Channel 2" name="ASYNCCH2" value="0x05"/>
                                                  <value caption="Asynchronous Event Channel 3" name="ASYNCCH3" value="0x06"/>
                                                </value-group>
                                                <value-group caption="Synchronous Channel 0 Generator Selection select" name="EVSYS_SYNCCH0">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Timer/Counter B0" name="TCB0" value="0x01"/>
                                                  <value caption="Timer/Counter A0 overflow" name="TCA0_OVF_LUNF" value="0x02"/>
                                                  <value caption="Timer/Counter A0 underflow high byte (split mode)" name="TCA0_HUNF" value="0x03"/>
                                                  <value caption="Timer/Counter A0 compare 0" name="TCA0_CMP0" value="0x04"/>
                                                  <value caption="Timer/Counter A0 compare 1" name="TCA0_CMP1" value="0x05"/>
                                                  <value caption="Timer/Counter A0 compare 2" name="TCA0_CMP2" value="0x06"/>
                                                  <value caption="Synchronous Event from Pin PC0" name="PORTC_PIN0" value="0x07"/>
                                                  <value caption="Synchronous Event from Pin PC1" name="PORTC_PIN1" value="0x08"/>
                                                  <value caption="Synchronous Event from Pin PC2" name="PORTC_PIN2" value="0x09"/>
                                                  <value caption="Synchronous Event from Pin PC3" name="PORTC_PIN3" value="0x0A"/>
                                                  <value caption="Synchronous Event from Pin PC4" name="PORTC_PIN4" value="0x0B"/>
                                                  <value caption="Synchronous Event from Pin PC5" name="PORTC_PIN5" value="0x0C"/>
                                                  <value caption="Synchronous Event from Pin PA0" name="PORTA_PIN0" value="0x0D"/>
                                                  <value caption="Synchronous Event from Pin PA1" name="PORTA_PIN1" value="0x0E"/>
                                                  <value caption="Synchronous Event from Pin PA2" name="PORTA_PIN2" value="0x0F"/>
                                                  <value caption="Synchronous Event from Pin PA3" name="PORTA_PIN3" value="0x10"/>
                                                  <value caption="Synchronous Event from Pin PA4" name="PORTA_PIN4" value="0x11"/>
                                                  <value caption="Synchronous Event from Pin PA5" name="PORTA_PIN5" value="0x12"/>
                                                  <value caption="Synchronous Event from Pin PA6" name="PORTA_PIN6" value="0x13"/>
                                                  <value caption="Synchronous Event from Pin PA7" name="PORTA_PIN7" value="0x14"/>
                                                </value-group>
                                                <value-group caption="Synchronous Channel 1 Generator Selection select" name="EVSYS_SYNCCH1">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Timer/Counter B0" name="TCB0" value="0x01"/>
                                                  <value caption="Timer/Counter A0 overflow" name="TCA0_OVF_LUNF" value="0x02"/>
                                                  <value caption="Timer/Counter A0 underflow high byte (split mode)" name="TCA0_HUNF" value="0x03"/>
                                                  <value caption="Timer/Counter A0 compare 0" name="TCA0_CMP0" value="0x04"/>
                                                  <value caption="Timer/Counter A0 compare 1" name="TCA0_CMP1" value="0x05"/>
                                                  <value caption="Timer/Counter A0 compare 2" name="TCA0_CMP2" value="0x06"/>
                                                  <value caption="Synchronous Event from Pin PB0" name="PORTB_PIN0" value="0x08"/>
                                                  <value caption="Synchronous Event from Pin PB1" name="PORTB_PIN1" value="0x09"/>
                                                  <value caption="Synchronous Event from Pin PB2" name="PORTB_PIN2" value="0x0A"/>
                                                  <value caption="Synchronous Event from Pin PB3" name="PORTB_PIN3" value="0x0B"/>
                                                  <value caption="Synchronous Event from Pin PB4" name="PORTB_PIN4" value="0x0C"/>
                                                  <value caption="Synchronous Event from Pin PB5" name="PORTB_PIN5" value="0x0D"/>
                                                  <value caption="Synchronous Event from Pin PB6" name="PORTB_PIN6" value="0x0E"/>
                                                  <value caption="Synchronous Event from Pin PB7" name="PORTB_PIN7" value="0x0F"/>
                                                </value-group>
                                                <value-group caption="Synchronous User Ch 0 Input Selection - TCA0 select" name="EVSYS_SYNCUSER0">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                </value-group>
                                                <value-group caption="Synchronous User Ch 1 Input Selection - USART0 select" name="EVSYS_SYNCUSER1">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="Synchronous Event Channel 0" name="SYNCCH0" value="0x01"/>
                                                  <value caption="Synchronous Event Channel 1" name="SYNCCH1" value="0x02"/>
                                                </value-group>
                                              </module>
                                              <module caption="Fuses" id="I2600" name="FUSE">
                                                <register-group caption="Fuses" name="FUSE" size="0x09">
                                                  <register caption="Application Code Section End" initval="0x00" name="APPEND" offset="0x7" rw="RW" size="1"/>
                                                  <register caption="BOD Configuration" initval="0x00" name="BODCFG" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="BOD Operation in Active Mode" mask="0xc" name="ACTIVE" rw="RW" values="FUSE_ACTIVE"/>
                                                    <bitfield caption="BOD Level" mask="0xe0" name="LVL" rw="RW" values="FUSE_LVL"/>
                                                    <bitfield caption="BOD Sample Frequency" mask="0x10" name="SAMPFREQ" rw="RW" values="FUSE_SAMPFREQ"/>
                                                    <bitfield caption="BOD Operation in Sleep Mode" mask="0x3" name="SLEEP" rw="RW" values="FUSE_SLEEP"/>
                                                  </register>
                                                  <register caption="Boot Section End" initval="0x00" name="BOOTEND" offset="0x8" rw="RW" size="1"/>
                                                  <register caption="Oscillator Configuration" initval="0x02" name="OSCCFG" offset="0x2" rw="RW" size="1">
                                                    <bitfield caption="Frequency Select" mask="0x3" name="FREQSEL" rw="RW" values="FUSE_FREQSEL"/>
                                                    <bitfield caption="Oscillator Lock" mask="0x80" name="OSCLOCK" rw="RW"/>
                                                  </register>
                                                  <register caption="System Configuration 0" initval="0xC4" name="SYSCFG0" offset="0x5" rw="RW" size="1">
                                                    <bitfield caption="CRC Source" mask="0xc0" name="CRCSRC" rw="RW" values="FUSE_CRCSRC"/>
                                                    <bitfield caption="EEPROM Save" mask="0x1" name="EESAVE" rw="RW"/>
                                                    <bitfield caption="Reset Pin Configuration" mask="0xc" name="RSTPINCFG" rw="RW" values="FUSE_RSTPINCFG"/>
                                                  </register>
                                                  <register caption="System Configuration 1" initval="0x07" name="SYSCFG1" offset="0x6" rw="RW" size="1">
                                                    <bitfield caption="Startup Time" mask="0x7" name="SUT" rw="RW" values="FUSE_SUT"/>
                                                  </register>
                                                  <register caption="TCD0 Configuration" initval="0x00" name="TCD0CFG" offset="0x4" rw="RW" size="1">
                                                    <bitfield caption="Compare A Default Output Value" mask="0x1" name="CMPA" rw="RW"/>
                                                    <bitfield caption="Compare A Output Enable" mask="0x10" name="CMPAEN" rw="RW"/>
                                                    <bitfield caption="Compare B Default Output Value" mask="0x2" name="CMPB" rw="RW"/>
                                                    <bitfield caption="Compare B Output Enable" mask="0x20" name="CMPBEN" rw="RW"/>
                                                    <bitfield caption="Compare C Default Output Value" mask="0x4" name="CMPC" rw="RW"/>
                                                    <bitfield caption="Compare C Output Enable" mask="0x40" name="CMPCEN" rw="RW"/>
                                                    <bitfield caption="Compare D Default Output Value" mask="0x8" name="CMPD" rw="RW"/>
                                                    <bitfield caption="Compare D Output Enable" mask="0x80" name="CMPDEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Watchdog Configuration" initval="0x00" name="WDTCFG" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Watchdog Timeout Period" mask="0xf" name="PERIOD" rw="RW" values="FUSE_PERIOD"/>
                                                    <bitfield caption="Watchdog Window Timeout Period" mask="0xf0" name="WINDOW" rw="RW" values="FUSE_WINDOW"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="BOD Operation in Active Mode select" name="FUSE_ACTIVE">
                                                  <value caption="Disabled" name="DIS" value="0x00"/>
                                                  <value caption="Enabled" name="ENABLED" value="0x01"/>
                                                  <value caption="Sampled" name="SAMPLED" value="0x02"/>
                                                  <value caption="Enabled with wake-up halted until BOD is ready" name="ENWAKE" value="0x03"/>
                                                </value-group>
                                                <value-group caption="BOD Level select" name="FUSE_LVL">
                                                  <value caption="1.8 V" name="BODLEVEL0" value="0x00"/>
                                                  <value caption="2.6 V" name="BODLEVEL2" value="0x02"/>
                                                  <value caption="4.2 V" name="BODLEVEL7" value="0x07"/>
                                                </value-group>
                                                <value-group caption="BOD Sample Frequency select" name="FUSE_SAMPFREQ">
                                                  <value caption="1kHz sampling frequency" name="1KHz" value="0x0"/>
                                                  <value caption="125Hz sampling frequency" name="125Hz" value="0x1"/>
                                                </value-group>
                                                <value-group caption="BOD Operation in Sleep Mode select" name="FUSE_SLEEP">
                                                  <value caption="Disabled" name="DIS" value="0x00"/>
                                                  <value caption="Enabled" name="ENABLED" value="0x01"/>
                                                  <value caption="Sampled" name="SAMPLED" value="0x02"/>
                                                </value-group>
                                                <value-group caption="Frequency Select select" name="FUSE_FREQSEL">
                                                  <value caption="16 MHz" name="16MHZ" value="0x1"/>
                                                  <value caption="20 MHz" name="20MHZ" value="0x2"/>
                                                </value-group>
                                                <value-group caption="CRC Source select" name="FUSE_CRCSRC">
                                                  <value caption="The CRC is performed on the entire Flash (boot, application code and application data section)." name="FLASH" value="0x0"/>
                                                  <value caption="The CRC is performed on the boot section of Flash" name="BOOT" value="0x1"/>
                                                  <value caption="The CRC is performed on the boot and application code section of Flash" name="BOOTAPP" value="0x2"/>
                                                  <value caption="Disable CRC." name="NOCRC" value="0x3"/>
                                                </value-group>
                                                <value-group caption="Reset Pin Configuration select" name="FUSE_RSTPINCFG">
                                                  <value caption="GPIO mode" name="GPIO" value="0x0"/>
                                                  <value caption="UPDI mode" name="UPDI" value="0x1"/>
                                                  <value caption="Reset mode" name="RST" value="0x2"/>
                                                </value-group>
                                                <value-group caption="Startup Time select" name="FUSE_SUT">
                                                  <value caption="0 ms" name="0MS" value="0x00"/>
                                                  <value caption="1 ms" name="1MS" value="0x01"/>
                                                  <value caption="2 ms" name="2MS" value="0x02"/>
                                                  <value caption="4 ms" name="4MS" value="0x03"/>
                                                  <value caption="8 ms" name="8MS" value="0x04"/>
                                                  <value caption="16 ms" name="16MS" value="0x05"/>
                                                  <value caption="32 ms" name="32MS" value="0x06"/>
                                                  <value caption="64 ms" name="64MS" value="0x07"/>
                                                </value-group>
                                                <value-group caption="Watchdog Timeout Period select" name="FUSE_PERIOD">
                                                  <value caption="Watch-Dog timer Off" name="OFF" value="0x00"/>
                                                  <value caption="8 cycles (8ms)" name="8CLK" value="0x01"/>
                                                  <value caption="16 cycles (16ms)" name="16CLK" value="0x02"/>
                                                  <value caption="32 cycles (32ms)" name="32CLK" value="0x03"/>
                                                  <value caption="64 cycles (64ms)" name="64CLK" value="0x04"/>
                                                  <value caption="128 cycles (0.128s)" name="128CLK" value="0x05"/>
                                                  <value caption="256 cycles (0.256s)" name="256CLK" value="0x06"/>
                                                  <value caption="512 cycles (0.512s)" name="512CLK" value="0x07"/>
                                                  <value caption="1K cycles (1.0s)" name="1KCLK" value="0x08"/>
                                                  <value caption="2K cycles (2.0s)" name="2KCLK" value="0x09"/>
                                                  <value caption="4K cycles (4.1s)" name="4KCLK" value="0x0A"/>
                                                  <value caption="8K cycles (8.2s)" name="8KCLK" value="0x0B"/>
                                                </value-group>
                                                <value-group caption="Watchdog Window Timeout Period select" name="FUSE_WINDOW">
                                                  <value caption="Window mode off" name="OFF" value="0x00"/>
                                                  <value caption="8 cycles (8ms)" name="8CLK" value="0x01"/>
                                                  <value caption="16 cycles (16ms)" name="16CLK" value="0x02"/>
                                                  <value caption="32 cycles (32ms)" name="32CLK" value="0x03"/>
                                                  <value caption="64 cycles (64ms)" name="64CLK" value="0x04"/>
                                                  <value caption="128 cycles (0.128s)" name="128CLK" value="0x05"/>
                                                  <value caption="256 cycles (0.256s)" name="256CLK" value="0x06"/>
                                                  <value caption="512 cycles (0.512s)" name="512CLK" value="0x07"/>
                                                  <value caption="1K cycles (1.0s)" name="1KCLK" value="0x08"/>
                                                  <value caption="2K cycles (2.0s)" name="2KCLK" value="0x09"/>
                                                  <value caption="4K cycles (4.1s)" name="4KCLK" value="0x0A"/>
                                                  <value caption="8K cycles (8.2s)" name="8KCLK" value="0x0B"/>
                                                </value-group>
                                              </module>
                                              <module caption="General Purpose IO" id="I2600" name="GPIO">
                                                <register-group caption="General Purpose IO" name="GPIO" size="0x4">
                                                  <register caption="General Purpose IO Register 0" name="GPIOR0" offset="0x0" rw="RW" size="1"/>
                                                  <register caption="General Purpose IO Register 1" name="GPIOR1" offset="0x1" rw="RW" size="1"/>
                                                  <register caption="General Purpose IO Register 2" name="GPIOR2" offset="0x2" rw="RW" size="1"/>
                                                  <register caption="General Purpose IO Register 3" name="GPIOR3" offset="0x3" rw="RW" size="1"/>
                                                </register-group>
                                              </module>
                                              <module caption="Lockbit" id="I2600" name="LOCKBIT">
                                                <register-group caption="Lockbit" name="LOCKBIT" size="0x01">
                                                  <register caption="Lock bits" initval="0xC5" name="LOCKBIT" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Lock Bits" mask="0xff" name="LB" rw="RW" values="LOCKBIT_LB"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Lock Bits select" name="LOCKBIT_LB">
                                                  <value caption="Read and write lock" name="RWLOCK" value="0x3A"/>
                                                  <value caption="No locks" name="NOLOCK" value="0xC5"/>
                                                </value-group>
                                              </module>
                                              <module caption="Non-volatile Memory Controller" id="I2109" name="NVMCTRL">
                                                <register-group caption="Non-volatile Memory Controller" name="NVMCTRL" size="0x10">
                                                  <register caption="Address" name="ADDR" offset="0x8" rw="RW" size="2"/>
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Command" mask="0x7" name="CMD" rw="RW" values="NVMCTRL_CMD"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="Application code write protect" mask="0x1" name="APCWP" rw="RW"/>
                                                    <bitfield caption="Boot Lock" mask="0x2" name="BOOTLOCK" rw="RW"/>
                                                  </register>
                                                  <register caption="Data" name="DATA" offset="0x6" rw="RW" size="2"/>
                                                  <register caption="Interrupt Control" initval="0x00" name="INTCTRL" offset="0x3" rw="RW" size="1">
                                                    <bitfield caption="EEPROM Ready" mask="0x1" name="EEREADY" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x4" rw="RW" size="1">
                                                    <bitfield caption="EEPROM Ready" mask="0x1" name="EEREADY" rw="RW"/>
                                                  </register>
                                                  <register caption="Status" initval="0x00" name="STATUS" offset="0x2" rw="R" size="1">
                                                    <bitfield caption="EEPROM busy" mask="0x2" name="EEBUSY" rw="R"/>
                                                    <bitfield caption="Flash busy" mask="0x1" name="FBUSY" rw="R"/>
                                                    <bitfield caption="Write error" mask="0x4" name="WRERROR" rw="R"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Command select" name="NVMCTRL_CMD">
                                                  <value caption="No Command" name="NONE" value="0x00"/>
                                                  <value caption="Write page" name="PAGEWRITE" value="0x01"/>
                                                  <value caption="Erase page" name="PAGEERASE" value="0x02"/>
                                                  <value caption="Erase and write page" name="PAGEERASEWRITE" value="0x03"/>
                                                  <value caption="Page buffer clear" name="PAGEBUFCLR" value="0x04"/>
                                                  <value caption="Chip erase" name="CHIPERASE" value="0x05"/>
                                                  <value caption="EEPROM erase" name="EEERASE" value="0x06"/>
                                                  <value caption="Write fuse (PDI only)" name="FUSEWRITE" value="0x07"/>
                                                </value-group>
                                              </module>
                                              <module caption="I/O Ports" id="I2103" name="PORT">
                                                <register-group caption="I/O Ports" name="PORT" size="0x20">
                                                  <register caption="Data Direction" name="DIR" offset="0x00" rw="RW" size="1"/>
                                                  <register caption="Data Direction Clear" name="DIRCLR" offset="0x02" rw="RW" size="1"/>
                                                  <register caption="Data Direction Set" name="DIRSET" offset="0x01" rw="RW" size="1"/>
                                                  <register caption="Data Direction Toggle" name="DIRTGL" offset="0x03" rw="RW" size="1"/>
                                                  <register caption="Input Value" name="IN" offset="0x08" rw="RW" size="1"/>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x09" rw="RW" size="1">
                                                    <bitfield caption="Pin Interrupt" mask="0xff" name="INT" rw="RW"/>
                                                  </register>
                                                  <register caption="Output Value" name="OUT" offset="0x04" rw="RW" size="1"/>
                                                  <register caption="Output Value Clear" name="OUTCLR" offset="0x06" rw="RW" size="1"/>
                                                  <register caption="Output Value Set" name="OUTSET" offset="0x05" rw="RW" size="1"/>
                                                  <register caption="Output Value Toggle" name="OUTTGL" offset="0x07" rw="RW" size="1"/>
                                                  <register caption="Pin 0 Control" initval="0x00" name="PIN0CTRL" offset="0x10" rw="RW" size="1">
                                                    <bitfield caption="Inverted I/O Enable" mask="0x80" name="INVEN" rw="RW"/>
                                                    <bitfield caption="Input/Sense Configuration" mask="0x7" name="ISC" rw="RW" values="PORT_ISC"/>
                                                    <bitfield caption="Pullup enable" mask="0x8" name="PULLUPEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Pin 1 Control" initval="0x00" name="PIN1CTRL" offset="0x11" rw="RW" size="1">
                                                    <bitfield caption="Inverted I/O Enable" mask="0x80" name="INVEN" rw="RW"/>
                                                    <bitfield caption="Input/Sense Configuration" mask="0x7" name="ISC" rw="RW" values="PORT_ISC"/>
                                                    <bitfield caption="Pullup enable" mask="0x8" name="PULLUPEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Pin 2 Control" initval="0x00" name="PIN2CTRL" offset="0x12" rw="RW" size="1">
                                                    <bitfield caption="Inverted I/O Enable" mask="0x80" name="INVEN" rw="RW"/>
                                                    <bitfield caption="Input/Sense Configuration" mask="0x7" name="ISC" rw="RW" values="PORT_ISC"/>
                                                    <bitfield caption="Pullup enable" mask="0x8" name="PULLUPEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Pin 3 Control" initval="0x00" name="PIN3CTRL" offset="0x13" rw="RW" size="1">
                                                    <bitfield caption="Inverted I/O Enable" mask="0x80" name="INVEN" rw="RW"/>
                                                    <bitfield caption="Input/Sense Configuration" mask="0x7" name="ISC" rw="RW" values="PORT_ISC"/>
                                                    <bitfield caption="Pullup enable" mask="0x8" name="PULLUPEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Pin 4 Control" initval="0x00" name="PIN4CTRL" offset="0x14" rw="RW" size="1">
                                                    <bitfield caption="Inverted I/O Enable" mask="0x80" name="INVEN" rw="RW"/>
                                                    <bitfield caption="Input/Sense Configuration" mask="0x7" name="ISC" rw="RW" values="PORT_ISC"/>
                                                    <bitfield caption="Pullup enable" mask="0x8" name="PULLUPEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Pin 5 Control" initval="0x00" name="PIN5CTRL" offset="0x15" rw="RW" size="1">
                                                    <bitfield caption="Inverted I/O Enable" mask="0x80" name="INVEN" rw="RW"/>
                                                    <bitfield caption="Input/Sense Configuration" mask="0x7" name="ISC" rw="RW" values="PORT_ISC"/>
                                                    <bitfield caption="Pullup enable" mask="0x8" name="PULLUPEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Pin 6 Control" initval="0x00" name="PIN6CTRL" offset="0x16" rw="RW" size="1">
                                                    <bitfield caption="Inverted I/O Enable" mask="0x80" name="INVEN" rw="RW"/>
                                                    <bitfield caption="Input/Sense Configuration" mask="0x7" name="ISC" rw="RW" values="PORT_ISC"/>
                                                    <bitfield caption="Pullup enable" mask="0x8" name="PULLUPEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Pin 7 Control" initval="0x00" name="PIN7CTRL" offset="0x17" rw="RW" size="1">
                                                    <bitfield caption="Inverted I/O Enable" mask="0x80" name="INVEN" rw="RW"/>
                                                    <bitfield caption="Input/Sense Configuration" mask="0x7" name="ISC" rw="RW" values="PORT_ISC"/>
                                                    <bitfield caption="Pullup enable" mask="0x8" name="PULLUPEN" rw="RW"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Input/Sense Configuration select" name="PORT_ISC">
                                                  <value caption="Interrupt disabled but input buffer enabled" name="INTDISABLE" value="0x00"/>
                                                  <value caption="Sense Both Edges" name="BOTHEDGES" value="0x01"/>
                                                  <value caption="Sense Rising Edge" name="RISING" value="0x02"/>
                                                  <value caption="Sense Falling Edge" name="FALLING" value="0x03"/>
                                                  <value caption="Digital Input Buffer disabled" name="INPUT_DISABLE" value="0x04"/>
                                                  <value caption="Sense low Level" name="LEVEL" value="0x05"/>
                                                </value-group>
                                              </module>
                                              <module caption="Port Multiplexer" id="I2600" name="PORTMUX">
                                                <register-group caption="Port Multiplexer" name="PORTMUX" size="0x10">
                                                  <register caption="Port Multiplexer Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Event Output 0" mask="0x1" name="EVOUT0" rw="RW"/>
                                                    <bitfield caption="Event Output 1" mask="0x2" name="EVOUT1" rw="RW"/>
                                                    <bitfield caption="Event Output 2" mask="0x4" name="EVOUT2" rw="RW"/>
                                                    <bitfield caption="Configurable Custom Logic LUT0" mask="0x10" name="LUT0" rw="RW" values="PORTMUX_LUT0"/>
                                                    <bitfield caption="Configurable Custom Logic LUT1" mask="0x20" name="LUT1" rw="RW" values="PORTMUX_LUT1"/>
                                                  </register>
                                                  <register caption="Port Multiplexer Control B" initval="0x00" name="CTRLB" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="Port Multiplexer SPI0" mask="0x4" name="SPI0" rw="RW" values="PORTMUX_SPI0"/>
                                                    <bitfield caption="Port Multiplexer TWI0" mask="0x10" name="TWI0" rw="RW" values="PORTMUX_TWI0"/>
                                                    <bitfield caption="Port Multiplexer USART0" mask="0x1" name="USART0" rw="RW" values="PORTMUX_USART0"/>
                                                  </register>
                                                  <register caption="Port Multiplexer Control C" initval="0x00" name="CTRLC" offset="0x2" rw="RW" size="1">
                                                    <bitfield caption="Port Multiplexer TCA0 Output 0" mask="0x1" name="TCA00" rw="RW" values="PORTMUX_TCA00"/>
                                                    <bitfield caption="Port Multiplexer TCA0 Output 1" mask="0x2" name="TCA01" rw="RW" values="PORTMUX_TCA01"/>
                                                    <bitfield caption="Port Multiplexer TCA0 Output 2" mask="0x4" name="TCA02" rw="RW" values="PORTMUX_TCA02"/>
                                                    <bitfield caption="Port Multiplexer TCA0 Output 3" mask="0x8" name="TCA03" rw="RW" values="PORTMUX_TCA03"/>
                                                    <bitfield caption="Port Multiplexer TCA0 Output 4" mask="0x10" name="TCA04" rw="RW" values="PORTMUX_TCA04"/>
                                                    <bitfield caption="Port Multiplexer TCA0 Output 5" mask="0x20" name="TCA05" rw="RW" values="PORTMUX_TCA05"/>
                                                  </register>
                                                  <register caption="Port Multiplexer Control D" initval="0x00" name="CTRLD" offset="0x3" rw="RW" size="1">
                                                    <bitfield caption="Port Multiplexer TCB" mask="0x1" name="TCB0" rw="RW" values="PORTMUX_TCB0"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Configurable Custom Logic LUT0 select" name="PORTMUX_LUT0">
                                                  <value caption="Default pin" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pin" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Configurable Custom Logic LUT1 select" name="PORTMUX_LUT1">
                                                  <value caption="Default pin" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pin" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer SPI0 select" name="PORTMUX_SPI0">
                                                  <value caption="Default pins" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pins" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer TWI0 select" name="PORTMUX_TWI0">
                                                  <value caption="Default pins" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pins" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer USART0 select" name="PORTMUX_USART0">
                                                  <value caption="Default pins" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pins" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer TCA0 Output 0 select" name="PORTMUX_TCA00">
                                                  <value caption="Default pin" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pin" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer TCA0 Output 1 select" name="PORTMUX_TCA01">
                                                  <value caption="Default pin" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pin" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer TCA0 Output 2 select" name="PORTMUX_TCA02">
                                                  <value caption="Default pin" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pin" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer TCA0 Output 3 select" name="PORTMUX_TCA03">
                                                  <value caption="Default pin" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pin" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer TCA0 Output 4 select" name="PORTMUX_TCA04">
                                                  <value caption="Default pin" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pin" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer TCA0 Output 5 select" name="PORTMUX_TCA05">
                                                  <value caption="Default pin" name="DEFAULT" value="0"/>
                                                  <value caption="Alternate pin" name="ALTERNATE" value="1"/>
                                                </value-group>
                                                <value-group caption="Port Multiplexer TCB select" name="PORTMUX_TCB0">
                                                  <value caption="Default pin" name="DEFAULT" value="0x0"/>
                                                  <value caption="Alternate pin" name="ALTERNATE" value="0x1"/>
                                                </value-group>
                                              </module>
                                              <module caption="Peripherial Touch Controller" id="I2120" name="PTC"/>
                                              <module caption="Reset controller" id="I2111" name="RSTCTRL">
                                                <register-group caption="Reset controller" name="RSTCTRL" size="0x4">
                                                  <register caption="Reset Flags" initval="0x00" name="RSTFR" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Brown out detector Reset flag" mask="0x2" name="BORF" rw="RW"/>
                                                    <bitfield caption="External Reset flag" mask="0x4" name="EXTRF" rw="RW"/>
                                                    <bitfield caption="Power on Reset flag" mask="0x1" name="PORF" rw="RW"/>
                                                    <bitfield caption="Software Reset flag" mask="0x10" name="SWRF" rw="RW"/>
                                                    <bitfield caption="UPDI Reset flag" mask="0x20" name="UPDIRF" rw="RW"/>
                                                    <bitfield caption="Watch dog Reset flag" mask="0x8" name="WDRF" rw="RW"/>
                                                  </register>
                                                  <register caption="Software Reset" initval="0x00" name="SWRR" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="Software reset enable" mask="0x1" name="SWRE" rw="RW"/>
                                                  </register>
                                                </register-group>
                                              </module>
                                              <module caption="Real-Time Counter" id="I2116" name="RTC">
                                                <register-group caption="Real-Time Counter" name="RTC" size="0x20">
                                                  <register caption="Clock Select" initval="0x00" name="CLKSEL" offset="0x07" rw="RW" size="1">
                                                    <bitfield caption="Clock Select" mask="0x3" name="CLKSEL" rw="RW" values="RTC_CLKSEL"/>
                                                  </register>
                                                  <register caption="Compare" name="CMP" offset="0x0C" rw="RW" size="2"/>
                                                  <register caption="Counter" name="CNT" offset="0x08" rw="RW" size="2"/>
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x00" rw="RW" size="1">
                                                    <bitfield caption="Prescaling Factor" mask="0x78" name="PRESCALER" rw="RW" values="RTC_PRESCALER"/>
                                                    <bitfield caption="Enable" mask="0x1" name="RTCEN" rw="RW"/>
                                                    <bitfield caption="Run In Standby" mask="0x80" name="RUNSTDBY" rw="RW"/>
                                                  </register>
                                                  <register caption="Debug control" initval="0x00" name="DBGCTRL" offset="0x05" rw="RW" size="1">
                                                    <bitfield caption="Run in debug" mask="0x1" name="DBGRUN" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Control" initval="0x00" name="INTCTRL" offset="0x02" rw="RW" size="1">
                                                    <bitfield caption="Compare Match Interrupt enable" mask="0x2" name="CMP" rw="RW"/>
                                                    <bitfield caption="Overflow Interrupt enable" mask="0x1" name="OVF" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x03" rw="RW" size="1">
                                                    <bitfield caption="Compare Match Interrupt" mask="0x2" name="CMP" rw="RW"/>
                                                    <bitfield caption="Overflow Interrupt Flag" mask="0x1" name="OVF" rw="RW"/>
                                                  </register>
                                                  <register caption="Period" name="PER" offset="0x0A" rw="RW" size="2"/>
                                                  <register caption="PIT Control A" initval="0x00" name="PITCTRLA" offset="0x10" rw="RW" size="1">
                                                    <bitfield caption="Period" mask="0x78" name="PERIOD" rw="RW" values="RTC_PERIOD"/>
                                                    <bitfield caption="Enable" mask="0x1" name="PITEN" rw="RW"/>
                                                  </register>
                                                  <register caption="PIT Debug control" initval="0x00" name="PITDBGCTRL" offset="0x15" rw="RW" size="1">
                                                    <bitfield caption="Run in debug" mask="0x1" name="DBGRUN" rw="RW"/>
                                                  </register>
                                                  <register caption="PIT Interrupt Control" initval="0x00" name="PITINTCTRL" offset="0x12" rw="RW" size="1">
                                                    <bitfield caption="Periodic Interrupt" mask="0x1" name="PI" rw="RW"/>
                                                  </register>
                                                  <register caption="PIT Interrupt Flags" initval="0x00" name="PITINTFLAGS" offset="0x13" rw="RW" size="1">
                                                    <bitfield caption="Periodic Interrupt" mask="0x1" name="PI" rw="RW"/>
                                                  </register>
                                                  <register caption="PIT Status" initval="0x00" name="PITSTATUS" offset="0x11" rw="R" size="1">
                                                    <bitfield caption="CTRLA Synchronization Busy Flag" mask="0x1" name="CTRLBUSY" rw="R"/>
                                                  </register>
                                                  <register caption="Status" initval="0x00" name="STATUS" offset="0x01" rw="R" size="1">
                                                    <bitfield caption="Comparator Synchronization Busy Flag" mask="0x8" name="CMPBUSY" rw="R"/>
                                                    <bitfield caption="Count Synchronization Busy Flag" mask="0x2" name="CNTBUSY" rw="R"/>
                                                    <bitfield caption="CTRLA Synchronization Busy Flag" mask="0x1" name="CTRLABUSY" rw="R"/>
                                                    <bitfield caption="Period Synchronization Busy Flag" mask="0x4" name="PERBUSY" rw="R"/>
                                                  </register>
                                                  <register caption="Temporary" name="TEMP" offset="0x04" rw="RW" size="1"/>
                                                </register-group>
                                                <value-group caption="Clock Select select" name="RTC_CLKSEL">
                                                  <value caption="Internal 32kHz OSC" name="INT32K" value="0x00"/>
                                                  <value caption="Internal 1kHz OSC" name="INT1K" value="0x01"/>
                                                  <value caption="32KHz Crystal OSC" name="TOSC32K" value="0x02"/>
                                                  <value caption="External Clock" name="EXTCLK" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Prescaling Factor select" name="RTC_PRESCALER">
                                                  <value caption="RTC Clock / 1" name="DIV1" value="0x00"/>
                                                  <value caption="RTC Clock / 2" name="DIV2" value="0x01"/>
                                                  <value caption="RTC Clock / 4" name="DIV4" value="0x02"/>
                                                  <value caption="RTC Clock / 8" name="DIV8" value="0x03"/>
                                                  <value caption="RTC Clock / 16" name="DIV16" value="0x04"/>
                                                  <value caption="RTC Clock / 32" name="DIV32" value="0x05"/>
                                                  <value caption="RTC Clock / 64" name="DIV64" value="0x06"/>
                                                  <value caption="RTC Clock / 128" name="DIV128" value="0x07"/>
                                                  <value caption="RTC Clock / 256" name="DIV256" value="0x08"/>
                                                  <value caption="RTC Clock / 512" name="DIV512" value="0x09"/>
                                                  <value caption="RTC Clock / 1024" name="DIV1024" value="0x0A"/>
                                                  <value caption="RTC Clock / 2048" name="DIV2048" value="0x0B"/>
                                                  <value caption="RTC Clock / 4096" name="DIV4096" value="0x0C"/>
                                                  <value caption="RTC Clock / 8192" name="DIV8192" value="0x0D"/>
                                                  <value caption="RTC Clock / 16384" name="DIV16384" value="0x0E"/>
                                                  <value caption="RTC Clock / 32768" name="DIV32768" value="0x0F"/>
                                                </value-group>
                                                <value-group caption="Period select" name="RTC_PERIOD">
                                                  <value caption="Off" name="OFF" value="0x00"/>
                                                  <value caption="RTC Clock Cycles 4" name="CYC4" value="0x01"/>
                                                  <value caption="RTC Clock Cycles 8" name="CYC8" value="0x02"/>
                                                  <value caption="RTC Clock Cycles 16" name="CYC16" value="0x03"/>
                                                  <value caption="RTC Clock Cycles 32" name="CYC32" value="0x04"/>
                                                  <value caption="RTC Clock Cycles 64" name="CYC64" value="0x05"/>
                                                  <value caption="RTC Clock Cycles 128" name="CYC128" value="0x06"/>
                                                  <value caption="RTC Clock Cycles 256" name="CYC256" value="0x07"/>
                                                  <value caption="RTC Clock Cycles 512" name="CYC512" value="0x08"/>
                                                  <value caption="RTC Clock Cycles 1024" name="CYC1024" value="0x09"/>
                                                  <value caption="RTC Clock Cycles 2048" name="CYC2048" value="0x0A"/>
                                                  <value caption="RTC Clock Cycles 4096" name="CYC4096" value="0x0B"/>
                                                  <value caption="RTC Clock Cycles 8192" name="CYC8192" value="0x0C"/>
                                                  <value caption="RTC Clock Cycles 16384" name="CYC16384" value="0x0D"/>
                                                  <value caption="RTC Clock Cycles 32768" name="CYC32768" value="0x0E"/>
                                                </value-group>
                                              </module>
                                              <module caption="Signature row" id="I2600" name="SIGROW">
                                                <register-group caption="Signature row" name="SIGROW" size="0x40">
                                                  <register caption="Device ID Byte 0" name="DEVICEID0" offset="0x00" rw="R" size="1"/>
                                                  <register caption="Device ID Byte 1" name="DEVICEID1" offset="0x01" rw="R" size="1"/>
                                                  <register caption="Device ID Byte 2" name="DEVICEID2" offset="0x02" rw="R" size="1"/>
                                                  <register caption="OSC16 error at 3V" name="OSC16ERR3V" offset="0x22" rw="R" size="1"/>
                                                  <register caption="OSC16 error at 5V" name="OSC16ERR5V" offset="0x23" rw="R" size="1"/>
                                                  <register caption="OSC20 error at 3V" name="OSC20ERR3V" offset="0x24" rw="R" size="1"/>
                                                  <register caption="OSC20 error at 5V" name="OSC20ERR5V" offset="0x25" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 0" name="SERNUM0" offset="0x03" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 1" name="SERNUM1" offset="0x04" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 2" name="SERNUM2" offset="0x05" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 3" name="SERNUM3" offset="0x06" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 4" name="SERNUM4" offset="0x07" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 5" name="SERNUM5" offset="0x08" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 6" name="SERNUM6" offset="0x09" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 7" name="SERNUM7" offset="0x0A" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 8" name="SERNUM8" offset="0x0B" rw="R" size="1"/>
                                                  <register caption="Serial Number Byte 9" name="SERNUM9" offset="0x0C" rw="R" size="1"/>
                                                  <register caption="Temperature Sensor Calibration Byte 0" name="TEMPSENSE0" offset="0x20" rw="R" size="1"/>
                                                  <register caption="Temperature Sensor Calibration Byte 1" name="TEMPSENSE1" offset="0x21" rw="R" size="1"/>
                                                </register-group>
                                              </module>
                                              <module caption="Sleep Controller" id="I2112" name="SLPCTRL">
                                                <register-group caption="Sleep Controller" name="SLPCTRL" size="0x2">
                                                  <register caption="Control" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Sleep enable" mask="0x1" name="SEN" rw="RW"/>
                                                    <bitfield caption="Sleep mode" mask="0x6" name="SMODE" rw="RW" values="SLPCTRL_SMODE"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Sleep mode select" name="SLPCTRL_SMODE">
                                                  <value caption="Idle mode" name="IDLE" value="0x00"/>
                                                  <value caption="Standby Mode" name="STDBY" value="0x01"/>
                                                  <value caption="Power-down Mode" name="PDOWN" value="0x02"/>
                                                </value-group>
                                              </module>
                                              <module caption="Serial Peripheral Interface" id="I2107" name="SPI">
                                                <register-group caption="Serial Peripheral Interface" name="SPI" size="0x8">
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Enable Double Speed" mask="0x10" name="CLK2X" rw="RW"/>
                                                    <bitfield caption="Data Order Setting" mask="0x40" name="DORD" rw="RW"/>
                                                    <bitfield caption="Enable Module" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Master Operation Enable" mask="0x20" name="MASTER" rw="RW"/>
                                                    <bitfield caption="Prescaler" mask="0x6" name="PRESC" rw="RW" values="SPI_PRESC"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="Buffer Mode Enable" mask="0x80" name="BUFEN" rw="RW"/>
                                                    <bitfield caption="Buffer Write Mode" mask="0x40" name="BUFWR" rw="RW"/>
                                                    <bitfield caption="SPI Mode" mask="0x3" name="MODE" rw="RW" values="SPI_MODE"/>
                                                    <bitfield caption="Slave Select Disable" mask="0x4" name="SSD" rw="RW"/>
                                                  </register>
                                                  <register caption="Data" name="DATA" offset="0x4" rw="RW" size="1"/>
                                                  <register caption="Interrupt Control" name="INTCTRL" offset="0x2" rw="RW" size="1">
                                                    <bitfield caption="Data Register Empty Interrupt Enable" mask="0x20" name="DREIE" rw="RW"/>
                                                    <bitfield caption="Interrupt Enable" mask="0x1" name="IE" rw="RW"/>
                                                    <bitfield caption="Receive Complete Interrupt Enable" mask="0x80" name="RXCIE" rw="RW"/>
                                                    <bitfield caption="Slave Select Trigger Interrupt Enable" mask="0x10" name="SSIE" rw="RW"/>
                                                    <bitfield caption="Transfer Complete Interrupt Enable" mask="0x40" name="TXCIE" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x3" rw="RW" size="1">
                                                    <mode name="BUFFERED">
                                                      <bitfield caption="Buffer Overflow" mask="0x1" name="BUFOVF" rw="RW"/>
                                                      <bitfield caption="Data Register Empty Interrupt Flag" mask="0x20" name="DREIF" rw="RW"/>
                                                      <bitfield caption="Receive Complete Interrupt Flag" mask="0x80" name="RXCIF" rw="RW"/>
                                                      <bitfield caption="Slave Select Trigger Interrupt Flag" mask="0x10" name="SSIF" rw="RW"/>
                                                      <bitfield caption="Transfer Complete Interrupt Flag" mask="0x40" name="TXCIF" rw="RW"/>
                                                    </mode>
                                                    <mode name="DEFAULT">
                                                      <bitfield caption="Interrupt Flag" mask="0x80" name="IF" rw="RW"/>
                                                      <bitfield caption="Write Collision" mask="0x40" name="WRCOL" rw="RW"/>
                                                    </mode>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Prescaler select" name="SPI_PRESC">
                                                  <value caption="System Clock / 4" name="DIV4" value="0x00"/>
                                                  <value caption="System Clock / 16" name="DIV16" value="0x01"/>
                                                  <value caption="System Clock / 64" name="DIV64" value="0x02"/>
                                                  <value caption="System Clock / 128" name="DIV128" value="0x03"/>
                                                </value-group>
                                                <value-group caption="SPI Mode select" name="SPI_MODE">
                                                  <value caption="SPI Mode 0" name="0" value="0x00"/>
                                                  <value caption="SPI Mode 1" name="1" value="0x01"/>
                                                  <value caption="SPI Mode 2" name="2" value="0x02"/>
                                                  <value caption="SPI Mode 3" name="3" value="0x03"/>
                                                </value-group>
                                              </module>
                                              <module caption="System Configuration Registers" id="I2600" name="SYSCFG">
                                                <register-group caption="System Configuration Registers" name="SYSCFG" size="0x20">
                                                  <register caption="External Break" initval="0x00" name="EXTBRK" offset="0x02" rw="RW" size="1">
                                                    <bitfield caption="External break enable" mask="0x1" name="ENEXTBRK" rw="RW"/>
                                                  </register>
                                                  <register caption="Revision ID" name="REVID" offset="0x01" rw="RW" size="1"/>
                                                </register-group>
                                              </module>
                                              <module caption="16-bit Timer/Counter Type A" id="I2117" name="TCA">
                                                <register-group caption="16-bit Timer/Counter Type A - Single Mode" name="TCA_SINGLE" size="0x40">
                                                  <register caption="Compare 0" name="CMP0" offset="0x28" rw="RW" size="2"/>
                                                  <register caption="Compare 0 Buffer" name="CMP0BUF" offset="0x38" rw="RW" size="2"/>
                                                  <register caption="Compare 1" name="CMP1" offset="0x2A" rw="RW" size="2"/>
                                                  <register caption="Compare 1 Buffer" name="CMP1BUF" offset="0x3A" rw="RW" size="2"/>
                                                  <register caption="Compare 2" name="CMP2" offset="0x2C" rw="RW" size="2"/>
                                                  <register caption="Compare 2 Buffer" name="CMP2BUF" offset="0x3C" rw="RW" size="2"/>
                                                  <register caption="Count" name="CNT" offset="0x20" rw="RW" size="2"/>
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x00" rw="RW" size="1">
                                                    <bitfield caption="Clock Selection" mask="0xe" name="CLKSEL" rw="RW" values="TCA_SINGLE_CLKSEL"/>
                                                    <bitfield caption="Module Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x01" rw="RW" size="1">
                                                    <bitfield caption="Auto Lock Update" mask="0x8" name="ALUPD" rw="RW"/>
                                                    <bitfield caption="Compare 0 Enable" mask="0x10" name="CMP0EN" rw="RW"/>
                                                    <bitfield caption="Compare 1 Enable" mask="0x20" name="CMP1EN" rw="RW"/>
                                                    <bitfield caption="Compare 2 Enable" mask="0x40" name="CMP2EN" rw="RW"/>
                                                    <bitfield caption="Waveform generation mode" mask="0x7" name="WGMODE" rw="RW" values="TCA_SINGLE_WGMODE"/>
                                                  </register>
                                                  <register caption="Control C" initval="0x00" name="CTRLC" offset="0x02" rw="RW" size="1">
                                                    <bitfield caption="Compare 0 Waveform Output Value" mask="0x1" name="CMP0OV" rw="RW"/>
                                                    <bitfield caption="Compare 1 Waveform Output Value" mask="0x2" name="CMP1OV" rw="RW"/>
                                                    <bitfield caption="Compare 2 Waveform Output Value" mask="0x4" name="CMP2OV" rw="RW"/>
                                                  </register>
                                                  <register caption="Control D" initval="0x00" name="CTRLD" offset="0x03" rw="RW" size="1">
                                                    <bitfield caption="Split Mode Enable" mask="0x1" name="SPLITM" rw="RW"/>
                                                  </register>
                                                  <register caption="Control E Clear" initval="0x00" name="CTRLECLR" offset="0x04" rw="RW" size="1">
                                                    <bitfield caption="Command" mask="0xc" name="CMD" rw="RW" values="TCA_SINGLE_CMD"/>
                                                    <bitfield caption="Direction" mask="0x1" name="DIR" rw="RW"/>
                                                    <bitfield caption="Lock Update" mask="0x2" name="LUPD" rw="RW"/>
                                                  </register>
                                                  <register caption="Control E Set" initval="0x00" name="CTRLESET" offset="0x05" rw="RW" size="1">
                                                    <bitfield caption="Command" mask="0xc" name="CMD" rw="RW" values="TCA_SINGLE_CMD"/>
                                                    <bitfield caption="Direction" mask="0x1" name="DIR" rw="RW" values="TCA_SINGLE_DIR"/>
                                                    <bitfield caption="Lock Update" mask="0x2" name="LUPD" rw="RW"/>
                                                  </register>
                                                  <register caption="Control F Clear" initval="0x00" name="CTRLFCLR" offset="0x06" rw="RW" size="1">
                                                    <bitfield caption="Compare 0 Buffer Valid" mask="0x2" name="CMP0BV" rw="RW"/>
                                                    <bitfield caption="Compare 1 Buffer Valid" mask="0x4" name="CMP1BV" rw="RW"/>
                                                    <bitfield caption="Compare 2 Buffer Valid" mask="0x8" name="CMP2BV" rw="RW"/>
                                                    <bitfield caption="Period Buffer Valid" mask="0x1" name="PERBV" rw="RW"/>
                                                  </register>
                                                  <register caption="Control F Set" initval="0x00" name="CTRLFSET" offset="0x07" rw="RW" size="1">
                                                    <bitfield caption="Compare 0 Buffer Valid" mask="0x2" name="CMP0BV" rw="RW"/>
                                                    <bitfield caption="Compare 1 Buffer Valid" mask="0x4" name="CMP1BV" rw="RW"/>
                                                    <bitfield caption="Compare 2 Buffer Valid" mask="0x8" name="CMP2BV" rw="RW"/>
                                                    <bitfield caption="Period Buffer Valid" mask="0x1" name="PERBV" rw="RW"/>
                                                  </register>
                                                  <register caption="Degbug Control" initval="0x00" name="DBGCTRL" offset="0x0E" rw="RW" size="1">
                                                    <bitfield caption="Debug Run" mask="0x1" name="DBGRUN" rw="RW"/>
                                                  </register>
                                                  <register caption="Event Control" initval="0x00" name="EVCTRL" offset="0x09" rw="RW" size="1">
                                                    <bitfield caption="Count on Event Input" mask="0x1" name="CNTEI" rw="RW"/>
                                                    <bitfield caption="Event Action" mask="0x6" name="EVACT" rw="RW" values="TCA_SINGLE_EVACT"/>
                                                  </register>
                                                  <register caption="Interrupt Control" initval="0x00" name="INTCTRL" offset="0x0A" rw="RW" size="1">
                                                    <bitfield caption="Compare 0 Interrupt" mask="0x10" name="CMP0" rw="RW"/>
                                                    <bitfield caption="Compare 1 Interrupt" mask="0x20" name="CMP1" rw="RW"/>
                                                    <bitfield caption="Compare 2 Interrupt" mask="0x40" name="CMP2" rw="RW"/>
                                                    <bitfield caption="Overflow Interrupt" mask="0x1" name="OVF" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x0B" rw="RW" size="1">
                                                    <bitfield caption="Compare 0 Interrupt" mask="0x10" name="CMP0" rw="RW"/>
                                                    <bitfield caption="Compare 1 Interrupt" mask="0x20" name="CMP1" rw="RW"/>
                                                    <bitfield caption="Compare 2 Interrupt" mask="0x40" name="CMP2" rw="RW"/>
                                                    <bitfield caption="Overflow Interrupt" mask="0x1" name="OVF" rw="RW"/>
                                                  </register>
                                                  <register caption="Period" name="PER" offset="0x26" rw="RW" size="2"/>
                                                  <register caption="Period Buffer" name="PERBUF" offset="0x36" rw="RW" size="2"/>
                                                  <register caption="Temporary data for 16-bit Access" name="TEMP" offset="0x0F" rw="RW" size="1"/>
                                                </register-group>
                                                <register-group caption="16-bit Timer/Counter Type A - Split Mode" name="TCA_SPLIT" size="0x40">
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x00" rw="RW" size="1">
                                                    <bitfield caption="Clock Selection" mask="0xe" name="CLKSEL" rw="RW" values="TCA_SPLIT_CLKSEL"/>
                                                    <bitfield caption="Module Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x01" rw="RW" size="1">
                                                    <bitfield caption="High Compare 0 Enable" mask="0x10" name="HCMP0EN" rw="RW"/>
                                                    <bitfield caption="High Compare 1 Enable" mask="0x20" name="HCMP1EN" rw="RW"/>
                                                    <bitfield caption="High Compare 2 Enable" mask="0x40" name="HCMP2EN" rw="RW"/>
                                                    <bitfield caption="Low Compare 0 Enable" mask="0x1" name="LCMP0EN" rw="RW"/>
                                                    <bitfield caption="Low Compare 1 Enable" mask="0x2" name="LCMP1EN" rw="RW"/>
                                                    <bitfield caption="Low Compare 2 Enable" mask="0x4" name="LCMP2EN" rw="RW"/>
                                                  </register>
                                                  <register caption="Control C" initval="0x00" name="CTRLC" offset="0x02" rw="RW" size="1">
                                                    <bitfield caption="High Compare 0 Output Value" mask="0x10" name="HCMP0OV" rw="RW"/>
                                                    <bitfield caption="High Compare 1 Output Value" mask="0x20" name="HCMP1OV" rw="RW"/>
                                                    <bitfield caption="High Compare 2 Output Value" mask="0x40" name="HCMP2OV" rw="RW"/>
                                                    <bitfield caption="Low Compare 0 Output Value" mask="0x1" name="LCMP0OV" rw="RW"/>
                                                    <bitfield caption="Low Compare 1 Output Value" mask="0x2" name="LCMP1OV" rw="RW"/>
                                                    <bitfield caption="Low Compare 2 Output Value" mask="0x4" name="LCMP2OV" rw="RW"/>
                                                  </register>
                                                  <register caption="Control D" initval="0x00" name="CTRLD" offset="0x03" rw="RW" size="1">
                                                    <bitfield caption="Split Mode Enable" mask="0x1" name="SPLITM" rw="RW"/>
                                                  </register>
                                                  <register caption="Control E Clear" initval="0x00" name="CTRLECLR" offset="0x04" rw="RW" size="1">
                                                    <bitfield caption="Command" mask="0xc" name="CMD" rw="RW" values="TCA_SPLIT_CMD"/>
                                                  </register>
                                                  <register caption="Control E Set" initval="0x00" name="CTRLESET" offset="0x05" rw="RW" size="1">
                                                    <bitfield caption="Command" mask="0xc" name="CMD" rw="RW" values="TCA_SPLIT_CMD"/>
                                                  </register>
                                                  <register caption="Degbug Control" initval="0x00" name="DBGCTRL" offset="0x0E" rw="RW" size="1">
                                                    <bitfield caption="Debug Run" mask="0x1" name="DBGRUN" rw="RW"/>
                                                  </register>
                                                  <register caption="High Compare" name="HCMP0" offset="0x29" rw="RW" size="1"/>
                                                  <register caption="High Compare" name="HCMP1" offset="0x2B" rw="RW" size="1"/>
                                                  <register caption="High Compare" name="HCMP2" offset="0x2D" rw="RW" size="1"/>
                                                  <register caption="High Count" name="HCNT" offset="0x21" rw="RW" size="1"/>
                                                  <register caption="High Period" name="HPER" offset="0x27" rw="RW" size="1"/>
                                                  <register caption="Interrupt Control" initval="0x00" name="INTCTRL" offset="0x0A" rw="RW" size="1">
                                                    <bitfield caption="High Underflow Interrupt Enable" mask="0x2" name="HUNF" rw="RW"/>
                                                    <bitfield caption="Low Compare 0 Interrupt Enable" mask="0x10" name="LCMP0" rw="RW"/>
                                                    <bitfield caption="Low Compare 1 Interrupt Enable" mask="0x20" name="LCMP1" rw="RW"/>
                                                    <bitfield caption="Low Compare 2 Interrupt Enable" mask="0x40" name="LCMP2" rw="RW"/>
                                                    <bitfield caption="Low Underflow Interrupt Enable" mask="0x1" name="LUNF" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x0B" rw="RW" size="1">
                                                    <bitfield caption="High Underflow Interrupt Flag" mask="0x2" name="HUNF" rw="RW"/>
                                                    <bitfield caption="Low Compare 2 Interrupt Flag" mask="0x10" name="LCMP0" rw="RW"/>
                                                    <bitfield caption="Low Compare 1 Interrupt Flag" mask="0x20" name="LCMP1" rw="RW"/>
                                                    <bitfield caption="Low Compare 0 Interrupt Flag" mask="0x40" name="LCMP2" rw="RW"/>
                                                    <bitfield caption="Low Underflow Interrupt Flag" mask="0x1" name="LUNF" rw="RW"/>
                                                  </register>
                                                  <register caption="Low Compare" name="LCMP0" offset="0x28" rw="RW" size="1"/>
                                                  <register caption="Low Compare" name="LCMP1" offset="0x2A" rw="RW" size="1"/>
                                                  <register caption="Low Compare" name="LCMP2" offset="0x2C" rw="RW" size="1"/>
                                                  <register caption="Low Count" name="LCNT" offset="0x20" rw="RW" size="1"/>
                                                  <register caption="Low Period" name="LPER" offset="0x26" rw="RW" size="1"/>
                                                </register-group>
                                                <register-group caption="16-bit Timer/Counter Type A" class="union" name="TCA" size="0x40" union-tag="TCA.SINGLE.CTRLD.SPLITM">
                                                  <register-group name="SINGLE" name-in-module="TCA_SINGLE" offset="0" union-tag-value="0"/>
                                                  <register-group name="SPLIT" name-in-module="TCA_SPLIT" offset="0" union-tag-value="1"/>
                                                </register-group>
                                                <value-group caption="Clock Selection select" name="TCA_SINGLE_CLKSEL">
                                                  <value caption="System Clock" name="DIV1" value="0x00"/>
                                                  <value caption="System Clock / 2" name="DIV2" value="0x01"/>
                                                  <value caption="System Clock / 4" name="DIV4" value="0x02"/>
                                                  <value caption="System Clock / 8" name="DIV8" value="0x03"/>
                                                  <value caption="System Clock / 16" name="DIV16" value="0x04"/>
                                                  <value caption="System Clock / 64" name="DIV64" value="0x05"/>
                                                  <value caption="System Clock / 256" name="DIV256" value="0x06"/>
                                                  <value caption="System Clock / 1024" name="DIV1024" value="0x07"/>
                                                </value-group>
                                                <value-group caption="Waveform generation mode select" name="TCA_SINGLE_WGMODE">
                                                  <value caption="Normal Mode" name="NORMAL" value="0x00"/>
                                                  <value caption="Frequency Generation Mode" name="FRQ" value="0x01"/>
                                                  <value caption="Single Slope PWM" name="SINGLESLOPE" value="0x03"/>
                                                  <value caption="Dual Slope PWM, overflow on TOP" name="DSTOP" value="0x05"/>
                                                  <value caption="Dual Slope PWM, overflow on TOP and BOTTOM" name="DSBOTH" value="0x06"/>
                                                  <value caption="Dual Slope PWM, overflow on BOTTOM" name="DSBOTTOM" value="0x07"/>
                                                </value-group>
                                                <value-group caption="Command select" name="TCA_SINGLE_CMD">
                                                  <value caption="No Command" name="NONE" value="0x00"/>
                                                  <value caption="Force Update" name="UPDATE" value="0x01"/>
                                                  <value caption="Force Restart" name="RESTART" value="0x02"/>
                                                  <value caption="Force Hard Reset" name="RESET" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Direction select" name="TCA_SINGLE_DIR">
                                                  <value caption="Count up" name="UP" value="0x0"/>
                                                  <value caption="Count down" name="DOWN" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Event Action select" name="TCA_SINGLE_EVACT">
                                                  <value caption="Count on positive edge event" name="POSEDGE" value="0x00"/>
                                                  <value caption="Count on any edge event" name="ANYEDGE" value="0x01"/>
                                                  <value caption="Count on prescaled clock while event line is 1." name="HIGHLVL" value="0x02"/>
                                                  <value caption="Count on prescaled clock. Event controls count direction. Up-count when event line is 0, down-count when event line is 1." name="UPDOWN" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Clock Selection select" name="TCA_SPLIT_CLKSEL">
                                                  <value caption="System Clock" name="DIV1" value="0x00"/>
                                                  <value caption="System Clock / 2" name="DIV2" value="0x01"/>
                                                  <value caption="System Clock / 4" name="DIV4" value="0x02"/>
                                                  <value caption="System Clock / 8" name="DIV8" value="0x03"/>
                                                  <value caption="System Clock / 16" name="DIV16" value="0x04"/>
                                                  <value caption="System Clock / 64" name="DIV64" value="0x05"/>
                                                  <value caption="System Clock / 256" name="DIV256" value="0x06"/>
                                                  <value caption="System Clock / 1024" name="DIV1024" value="0x07"/>
                                                </value-group>
                                                <value-group caption="Command select" name="TCA_SPLIT_CMD">
                                                  <value caption="No Command" name="NONE" value="0x00"/>
                                                  <value caption="Force Update" name="UPDATE" value="0x01"/>
                                                  <value caption="Force Restart" name="RESTART" value="0x02"/>
                                                  <value caption="Force Hard Reset" name="RESET" value="0x03"/>
                                                </value-group>
                                              </module>
                                              <module caption="16-bit Timer Type B" id="I2119" name="TCB">
                                                <register-group caption="16-bit Timer Type B" name="TCB" size="0x10">
                                                  <register caption="Compare or Capture" name="CCMP" offset="0xC" rw="RW" size="2"/>
                                                  <register caption="Count" initval="0x0000" name="CNT" offset="0xA" rw="RW" size="2"/>
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Clock Select" mask="0x6" name="CLKSEL" rw="RW" values="TCB_CLKSEL"/>
                                                    <bitfield caption="Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Run Standby" mask="0x40" name="RUNSTDBY" rw="RW"/>
                                                    <bitfield caption="Synchronize Update" mask="0x10" name="SYNCUPD" rw="RW"/>
                                                  </register>
                                                  <register caption="Control Register B" initval="0x00" name="CTRLB" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="Asynchronous Enable" mask="0x40" name="ASYNC" rw="RW"/>
                                                    <bitfield caption="Pin Output Enable" mask="0x10" name="CCMPEN" rw="RW"/>
                                                    <bitfield caption="Pin Initial State" mask="0x20" name="CCMPINIT" rw="RW"/>
                                                    <bitfield caption="Timer Mode" mask="0x7" name="CNTMODE" rw="RW" values="TCB_CNTMODE"/>
                                                  </register>
                                                  <register caption="Debug Control" initval="0x00" name="DBGCTRL" offset="0x8" rw="RW" size="1">
                                                    <bitfield caption="Debug Run" mask="0x1" name="DBGRUN" rw="RW"/>
                                                  </register>
                                                  <register caption="Event Control" initval="0x00" name="EVCTRL" offset="0x4" rw="RW" size="1">
                                                    <bitfield caption="Event Input Enable" mask="0x1" name="CAPTEI" rw="RW"/>
                                                    <bitfield caption="Event Edge" mask="0x10" name="EDGE" rw="RW"/>
                                                    <bitfield caption="Input Capture Noise Cancellation Filter" mask="0x40" name="FILTER" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Control" initval="0x00" name="INTCTRL" offset="0x5" rw="RW" size="1">
                                                    <bitfield caption="Capture or Timeout" mask="0x1" name="CAPT" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x6" rw="RW" size="1">
                                                    <bitfield caption="Capture or Timeout" mask="0x1" name="CAPT" rw="RW"/>
                                                  </register>
                                                  <register caption="Status" initval="0x00" name="STATUS" offset="0x7" rw="R" size="1">
                                                    <bitfield caption="Run" mask="0x1" name="RUN" rw="R"/>
                                                  </register>
                                                  <register caption="Temporary Value" name="TEMP" offset="0x9" rw="RW" size="1"/>
                                                </register-group>
                                                <value-group caption="Clock Select select" name="TCB_CLKSEL">
                                                  <value caption="CLK_PER (No Prescaling)" name="CLKDIV1" value="0x00"/>
                                                  <value caption="CLK_PER/2 (From Prescaler)" name="CLKDIV2" value="0x01"/>
                                                  <value caption="Use Clock from TCA" name="CLKTCA" value="0x02"/>
                                                </value-group>
                                                <value-group caption="Timer Mode select" name="TCB_CNTMODE">
                                                  <value caption="Periodic Interrupt" name="INT" value="0x00"/>
                                                  <value caption="Periodic Timeout" name="TIMEOUT" value="0x01"/>
                                                  <value caption="Input Capture Event" name="CAPT" value="0x02"/>
                                                  <value caption="Input Capture Frequency measurement" name="FRQ" value="0x03"/>
                                                  <value caption="Input Capture Pulse-Width measurement" name="PW" value="0x04"/>
                                                  <value caption="Input Capture Frequency and Pulse-Width measurement" name="FRQPW" value="0x05"/>
                                                  <value caption="Single Shot" name="SINGLE" value="0x06"/>
                                                  <value caption="8-bit PWM" name="PWM8" value="0x07"/>
                                                </value-group>
                                              </module>
                                              <module caption="Timer Counter D" id="I2129" name="TCD">
                                                <register-group caption="Timer Counter D" name="TCD" size="0x40">
                                                  <register caption="Capture A" name="CAPTUREA" offset="0x22" rw="R" size="2"/>
                                                  <register caption="Capture B" name="CAPTUREB" offset="0x24" rw="R" size="2"/>
                                                  <register caption="Compare A Clear" name="CMPACLR" offset="0x2A" rw="RW" size="2"/>
                                                  <register caption="Compare A Set" name="CMPASET" offset="0x28" rw="RW" size="2"/>
                                                  <register caption="Compare B Clear" name="CMPBCLR" offset="0x2E" rw="RW" size="2"/>
                                                  <register caption="Compare B Set" name="CMPBSET" offset="0x2C" rw="RW" size="2"/>
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x00" rw="RW" size="1">
                                                    <bitfield caption="clock select" mask="0x60" name="CLKSEL" rw="RW" values="TCD_CLKSEL"/>
                                                    <bitfield caption="counter prescaler" mask="0x18" name="CNTPRES" rw="RW" values="TCD_CNTPRES"/>
                                                    <bitfield caption="Enable" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Syncronization prescaler" mask="0x6" name="SYNCPRES" rw="RW" values="TCD_SYNCPRES"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x01" rw="RW" size="1">
                                                    <bitfield caption="Waveform generation mode" mask="0x3" name="WGMODE" rw="RW" values="TCD_WGMODE"/>
                                                  </register>
                                                  <register caption="Control C" initval="0x00" name="CTRLC" offset="0x02" rw="RW" size="1">
                                                    <bitfield caption="Auto update" mask="0x2" name="AUPDATE" rw="RW"/>
                                                    <bitfield caption="Compare C output select" mask="0x40" name="CMPCSEL" rw="RW" values="TCD_CMPCSEL"/>
                                                    <bitfield caption="Compare D output select" mask="0x80" name="CMPDSEL" rw="RW" values="TCD_CMPDSEL"/>
                                                    <bitfield caption="Compare output value override" mask="0x1" name="CMPOVR" rw="RW"/>
                                                    <bitfield caption="Fifty percent waveform" mask="0x8" name="FIFTY" rw="RW"/>
                                                  </register>
                                                  <register caption="Control D" initval="0x00" name="CTRLD" offset="0x03" rw="RW" size="1">
                                                    <bitfield caption="Compare A value" mask="0xf" name="CMPAVAL" rw="RW"/>
                                                    <bitfield caption="Compare B value" mask="0xf0" name="CMPBVAL" rw="RW"/>
                                                  </register>
                                                  <register caption="Control E" initval="0x00" name="CTRLE" offset="0x04" rw="RW" size="1">
                                                    <bitfield caption="Disable at end of cycle" mask="0x80" name="DISEOC" rw="RW"/>
                                                    <bitfield caption="Restart strobe" mask="0x4" name="RESTART" rw="RW"/>
                                                    <bitfield caption="Software Capture A Strobe" mask="0x8" name="SCAPTUREA" rw="RW"/>
                                                    <bitfield caption="Software Capture B Strobe" mask="0x10" name="SCAPTUREB" rw="RW"/>
                                                    <bitfield caption="synchronize strobe" mask="0x2" name="SYNC" rw="RW"/>
                                                    <bitfield caption="synchronize end of cycle strobe" mask="0x1" name="SYNCEOC" rw="RW"/>
                                                  </register>
                                                  <register caption="Debug Control" initval="0x00" name="DBGCTRL" offset="0x1E" rw="RW" size="1">
                                                    <bitfield caption="Debug run" mask="0x1" name="DBGRUN" rw="RW"/>
                                                    <bitfield caption="Fault detection" mask="0x4" name="FAULTDET" rw="RW"/>
                                                  </register>
                                                  <register caption="Dither Control A" initval="0x00" name="DITCTRL" offset="0x18" rw="RW" size="1">
                                                    <bitfield caption="dither select" mask="0x3" name="DITHERSEL" rw="RW" values="TCD_DITHERSEL"/>
                                                  </register>
                                                  <register caption="Dither value" initval="0x00" name="DITVAL" offset="0x19" rw="RW" size="1">
                                                    <bitfield caption="Dither value" mask="0xf" name="DITHER" rw="RW"/>
                                                  </register>
                                                  <register caption="Delay Control" initval="0x00" name="DLYCTRL" offset="0x14" rw="RW" size="1">
                                                    <bitfield caption="Delay prescaler" mask="0x30" name="DLYPRESC" rw="RW" values="TCD_DLYPRESC"/>
                                                    <bitfield caption="Delay select" mask="0x3" name="DLYSEL" rw="RW" values="TCD_DLYSEL"/>
                                                    <bitfield caption="Delay trigger" mask="0xc" name="DLYTRIG" rw="RW" values="TCD_DLYTRIG"/>
                                                  </register>
                                                  <register caption="Delay value" initval="0x00" name="DLYVAL" offset="0x15" rw="RW" size="1">
                                                    <bitfield caption="Delay value" mask="0xff" name="DLYVAL" rw="RW"/>
                                                  </register>
                                                  <register caption="EVCTRLA" initval="0x00" name="EVCTRLA" offset="0x08" rw="RW" size="1">
                                                    <bitfield caption="event action" mask="0x4" name="ACTION" rw="RW" values="TCD_ACTION"/>
                                                    <bitfield caption="event config" mask="0xc0" name="CFG" rw="RW" values="TCD_CFG"/>
                                                    <bitfield caption="edge select" mask="0x10" name="EDGE" rw="RW" values="TCD_EDGE"/>
                                                    <bitfield caption="Trigger event enable" mask="0x1" name="TRIGEI" rw="RW"/>
                                                  </register>
                                                  <register caption="EVCTRLB" initval="0x00" name="EVCTRLB" offset="0x09" rw="RW" size="1">
                                                    <bitfield caption="event action" mask="0x4" name="ACTION" rw="RW" values="TCD_ACTION"/>
                                                    <bitfield caption="event config" mask="0xc0" name="CFG" rw="RW" values="TCD_CFG"/>
                                                    <bitfield caption="edge select" mask="0x10" name="EDGE" rw="RW" values="TCD_EDGE"/>
                                                    <bitfield caption="Trigger event enable" mask="0x1" name="TRIGEI" rw="RW"/>
                                                  </register>
                                                  <register caption="Fault Control" initval="0x00" name="FAULTCTRL" offset="0x12" rw="RW" size="1">
                                                    <bitfield caption="Compare A value" mask="0x1" name="CMPA" rw="RW"/>
                                                    <bitfield caption="Compare A enable" mask="0x10" name="CMPAEN" rw="RW"/>
                                                    <bitfield caption="Compare B value" mask="0x2" name="CMPB" rw="RW"/>
                                                    <bitfield caption="Compare B enable" mask="0x20" name="CMPBEN" rw="RW"/>
                                                    <bitfield caption="Compare C value" mask="0x4" name="CMPC" rw="RW"/>
                                                    <bitfield caption="Compare C enable" mask="0x40" name="CMPCEN" rw="RW"/>
                                                    <bitfield caption="Compare D vaule" mask="0x8" name="CMPD" rw="RW"/>
                                                    <bitfield caption="Compare D enable" mask="0x80" name="CMPDEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Input Control A" initval="0x00" name="INPUTCTRLA" offset="0x10" rw="RW" size="1">
                                                    <bitfield caption="Input mode" mask="0xf" name="INPUTMODE" rw="RW" values="TCD_INPUTMODE"/>
                                                  </register>
                                                  <register caption="Input Control B" initval="0x00" name="INPUTCTRLB" offset="0x11" rw="RW" size="1">
                                                    <bitfield caption="Input mode" mask="0xf" name="INPUTMODE" rw="RW" values="TCD_INPUTMODE"/>
                                                  </register>
                                                  <register caption="Interrupt Control" initval="0x00" name="INTCTRL" offset="0x0C" rw="RW" size="1">
                                                    <bitfield caption="Overflow interrupt enable" mask="0x1" name="OVF" rw="RW"/>
                                                    <bitfield caption="Trigger A interrupt enable" mask="0x4" name="TRIGA" rw="RW"/>
                                                    <bitfield caption="Trigger B interrupt enable" mask="0x8" name="TRIGB" rw="RW"/>
                                                  </register>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x0D" rw="RW" size="1">
                                                    <bitfield caption="Overflow interrupt enable" mask="0x1" name="OVF" rw="RW"/>
                                                    <bitfield caption="Trigger A interrupt enable" mask="0x4" name="TRIGA" rw="RW"/>
                                                    <bitfield caption="Trigger B interrupt enable" mask="0x8" name="TRIGB" rw="RW"/>
                                                  </register>
                                                  <register caption="Status" initval="0x00" name="STATUS" offset="0x0E" rw="RW" size="1">
                                                    <bitfield caption="Command ready" mask="0x2" name="CMDRDY" rw="R"/>
                                                    <bitfield caption="Enable ready" mask="0x1" name="ENRDY" rw="R"/>
                                                    <bitfield caption="PWM activity on A" mask="0x40" name="PWMACTA" rw="RW"/>
                                                    <bitfield caption="PWM activity on B" mask="0x80" name="PWMACTB" rw="RW"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="clock select select" name="TCD_CLKSEL">
                                                  <value caption="20 MHz oscillator" name="20MHZ" value="0x00"/>
                                                  <value caption="External clock" name="EXTCLK" value="0x02"/>
                                                  <value caption="System clock" name="SYSCLK" value="0x03"/>
                                                </value-group>
                                                <value-group caption="counter prescaler select" name="TCD_CNTPRES">
                                                  <value caption="Sync clock divided by 1" name="DIV1" value="0x00"/>
                                                  <value caption="Sync clock divided by 4" name="DIV4" value="0x01"/>
                                                  <value caption="Sync clock divided by 32" name="DIV32" value="0x02"/>
                                                </value-group>
                                                <value-group caption="Syncronization prescaler select" name="TCD_SYNCPRES">
                                                  <value caption="Selevted clock source divided by 1" name="DIV1" value="0x00"/>
                                                  <value caption="Selevted clock source divided by 2" name="DIV2" value="0x01"/>
                                                  <value caption="Selevted clock source divided by 4" name="DIV4" value="0x02"/>
                                                  <value caption="Selevted clock source divided by 8" name="DIV8" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Waveform generation mode select" name="TCD_WGMODE">
                                                  <value caption="One ramp mode" name="ONERAMP" value="0x0"/>
                                                  <value caption="Two ramp mode" name="TWORAMP" value="0x1"/>
                                                  <value caption="Four ramp mode" name="FOURRAMP" value="0x2"/>
                                                  <value caption="Dual slope mode" name="DS" value="0x3"/>
                                                </value-group>
                                                <value-group caption="Compare C output select select" name="TCD_CMPCSEL">
                                                  <value caption="PWM A output" name="PWMA" value="0x0"/>
                                                  <value caption="PWM B output" name="PWMB" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Compare D output select select" name="TCD_CMPDSEL">
                                                  <value caption="PWM A output" name="PWMA" value="0x0"/>
                                                  <value caption="PWM B output" name="PWMB" value="0x1"/>
                                                </value-group>
                                                <value-group caption="dither select select" name="TCD_DITHERSEL">
                                                  <value caption="On-time ramp B" name="ONTIMEB" value="0x0"/>
                                                  <value caption="On-time ramp A and B" name="ONTIMEAB" value="0x1"/>
                                                  <value caption="Dead-time rampB" name="DEADTIMEB" value="0x2"/>
                                                  <value caption="Dead-time ramp A and B" name="DEADTIMEAB" value="0x3"/>
                                                </value-group>
                                                <value-group caption="Delay prescaler select" name="TCD_DLYPRESC">
                                                  <value caption="No prescaling" name="DIV1" value="0x0"/>
                                                  <value caption="Prescale with 2" name="DIV2" value="0x1"/>
                                                  <value caption="Prescale with 4" name="DIV4" value="0x2"/>
                                                  <value caption="Prescale with 8" name="DIV8" value="0x3"/>
                                                </value-group>
                                                <value-group caption="Delay select select" name="TCD_DLYSEL">
                                                  <value caption="No delay" name="OFF" value="0x0"/>
                                                  <value caption="Input blanking enabled" name="INBLANK" value="0x1"/>
                                                  <value caption="Event delay enabled" name="EVENT" value="0x2"/>
                                                </value-group>
                                                <value-group caption="Delay trigger select" name="TCD_DLYTRIG">
                                                  <value caption="Compare A set" name="CMPASET" value="0x0"/>
                                                  <value caption="Compare A clear" name="CMPACLR" value="0x1"/>
                                                  <value caption="Compare B set" name="CMPBSET" value="0x2"/>
                                                  <value caption="Compare B clear" name="CMPBCLR" value="0x3"/>
                                                </value-group>
                                                <value-group caption="event action select" name="TCD_ACTION">
                                                  <value caption="Event trigger a fault" name="FAULT" value="0x0"/>
                                                  <value caption="Event trigger a fault and capture" name="CAPTURE" value="0x1"/>
                                                </value-group>
                                                <value-group caption="event config select" name="TCD_CFG">
                                                  <value caption="Neither Filter nor Asynchronous Event is enabled" name="NEITHER" value="0x0"/>
                                                  <value caption="Input Capture Noise Cancellation Filter enabled" name="FILTER" value="0x1"/>
                                                  <value caption="Asynchronous Event output qualification enabled" name="ASYNC" value="0x2"/>
                                                </value-group>
                                                <value-group caption="edge select select" name="TCD_EDGE">
                                                  <value caption="The falling edge or low level of event generates retrigger or fault action" name="FALL_LOW" value="0x0"/>
                                                  <value caption="The rising edge or high level of event generates retrigger or fault action" name="RISE_HIGH" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Input mode select" name="TCD_INPUTMODE">
                                                  <value caption="Input has no actions" name="NONE" value="0x0"/>
                                                  <value caption="Stop output, jump to opposite compare cycle and wait" name="JMPWAIT" value="0x1"/>
                                                  <value caption="Stop output, execute opposite compare cycle and wait" name="EXECWAIT" value="0x2"/>
                                                  <value caption="stop output, execute opposite compare cycle while fault active" name="EXECFAULT" value="0x3"/>
                                                  <value caption="Stop all outputs, maintain frequency" name="FREQ" value="0x4"/>
                                                  <value caption="Stop all outputs, execute dead time while fault active" name="EXECDT" value="0x5"/>
                                                  <value caption="Stop all outputs, jump to next compare cycle and wait" name="WAIT" value="0x6"/>
                                                  <value caption="Stop all outputs, wait for software action" name="WAITSW" value="0x7"/>
                                                  <value caption="Stop output on edge, jump to next compare cycle" name="EDGETRIG" value="0x8"/>
                                                  <value caption="Stop output on edge, maintain frequency" name="EDGETRIGFREQ" value="0x9"/>
                                                  <value caption="Stop output at level, maintain frequency" name="LVLTRIGFREQ" value="0xA"/>
                                                </value-group>
                                              </module>
                                              <module caption="Two-Wire Interface" id="I2110" name="TWI">
                                                <register-group caption="Two-Wire Interface" name="TWI" size="0x10">
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="FM Plus Enable" mask="0x2" name="FMPEN" rw="RW"/>
                                                    <bitfield caption="SDA Hold Time" mask="0xc" name="SDAHOLD" rw="RW" values="TWI_SDAHOLD"/>
                                                    <bitfield caption="SDA Setup Time" mask="0x10" name="SDASETUP" rw="RW" values="TWI_SDASETUP"/>
                                                  </register>
                                                  <register caption="Debug Control Register" initval="0x00" name="DBGCTRL" offset="0x2" rw="RW" size="1">
                                                    <bitfield caption="Debug Run" mask="0x1" name="DBGRUN" rw="RW"/>
                                                  </register>
                                                  <register caption="Master Address" name="MADDR" offset="0x7" rw="RW" size="1"/>
                                                  <register caption="Master Baurd Rate Control" name="MBAUD" offset="0x6" rw="RW" size="1"/>
                                                  <register caption="Master Control A" initval="0x00" name="MCTRLA" offset="0x3" rw="RW" size="1">
                                                    <bitfield caption="Enable TWI Master" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Quick Command Enable" mask="0x10" name="QCEN" rw="RW"/>
                                                    <bitfield caption="Read Interrupt Enable" mask="0x80" name="RIEN" rw="RW"/>
                                                    <bitfield caption="Smart Mode Enable" mask="0x2" name="SMEN" rw="RW"/>
                                                    <bitfield caption="Inactive Bus Timeout" mask="0xc" name="TIMEOUT" rw="RW" values="TWI_TIMEOUT"/>
                                                    <bitfield caption="Write Interrupt Enable" mask="0x40" name="WIEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Master Control B" initval="0x00" name="MCTRLB" offset="0x4" rw="RW" size="1">
                                                    <bitfield caption="Acknowledge Action" mask="0x4" name="ACKACT" rw="RW" values="TWI_ACKACT"/>
                                                    <bitfield caption="Flush" mask="0x8" name="FLUSH" rw="RW"/>
                                                    <bitfield caption="Command" mask="0x3" name="MCMD" rw="RW" values="TWI_MCMD"/>
                                                  </register>
                                                  <register caption="Master Data" name="MDATA" offset="0x8" rw="RW" size="1"/>
                                                  <register caption="Master Status" initval="0x00" name="MSTATUS" offset="0x5" rw="RW" size="1">
                                                    <bitfield caption="Arbitration Lost" mask="0x8" name="ARBLOST" rw="RW"/>
                                                    <bitfield caption="Bus Error" mask="0x4" name="BUSERR" rw="RW"/>
                                                    <bitfield caption="Bus State" mask="0x3" name="BUSSTATE" rw="RW" values="TWI_BUSSTATE"/>
                                                    <bitfield caption="Clock Hold" mask="0x20" name="CLKHOLD" rw="RW"/>
                                                    <bitfield caption="Read Interrupt Flag" mask="0x80" name="RIF" rw="RW"/>
                                                    <bitfield caption="Received Acknowledge" mask="0x10" name="RXACK" rw="R"/>
                                                    <bitfield caption="Write Interrupt Flag" mask="0x40" name="WIF" rw="RW"/>
                                                  </register>
                                                  <register caption="Slave Address" name="SADDR" offset="0xC" rw="RW" size="1"/>
                                                  <register caption="Slave Address Mask" name="SADDRMASK" offset="0xE" rw="RW" size="1">
                                                    <bitfield caption="Address Enable" mask="0x1" name="ADDREN" rw="RW"/>
                                                    <bitfield caption="Address Mask" mask="0xfe" name="ADDRMASK" rw="RW"/>
                                                  </register>
                                                  <register caption="Slave Control A" initval="0x00" name="SCTRLA" offset="0x9" rw="RW" size="1">
                                                    <bitfield caption="Address/Stop Interrupt Enable" mask="0x40" name="APIEN" rw="RW"/>
                                                    <bitfield caption="Data Interrupt Enable" mask="0x80" name="DIEN" rw="RW"/>
                                                    <bitfield caption="Enable TWI Slave" mask="0x1" name="ENABLE" rw="RW"/>
                                                    <bitfield caption="Stop Interrupt Enable" mask="0x20" name="PIEN" rw="RW"/>
                                                    <bitfield caption="Promiscuous Mode Enable" mask="0x4" name="PMEN" rw="RW"/>
                                                    <bitfield caption="Smart Mode Enable" mask="0x2" name="SMEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Slave Control B" initval="0x00" name="SCTRLB" offset="0xA" rw="RW" size="1">
                                                    <bitfield caption="Acknowledge Action" mask="0x4" name="ACKACT" rw="RW" values="TWI_ACKACT"/>
                                                    <bitfield caption="Command" mask="0x3" name="SCMD" rw="RW" values="TWI_SCMD"/>
                                                  </register>
                                                  <register caption="Slave Data" name="SDATA" offset="0xD" rw="RW" size="1"/>
                                                  <register caption="Slave Status" initval="0x00" name="SSTATUS" offset="0xB" rw="RW" size="1">
                                                    <bitfield caption="Slave Address or Stop" mask="0x1" name="AP" rw="R" values="TWI_AP"/>
                                                    <bitfield caption="Address/Stop Interrupt Flag" mask="0x40" name="APIF" rw="RW"/>
                                                    <bitfield caption="Bus Error" mask="0x4" name="BUSERR" rw="RW"/>
                                                    <bitfield caption="Clock Hold" mask="0x20" name="CLKHOLD" rw="R"/>
                                                    <bitfield caption="Collision" mask="0x8" name="COLL" rw="RW"/>
                                                    <bitfield caption="Data Interrupt Flag" mask="0x80" name="DIF" rw="RW"/>
                                                    <bitfield caption="Read/Write Direction" mask="0x2" name="DIR" rw="R"/>
                                                    <bitfield caption="Received Acknowledge" mask="0x10" name="RXACK" rw="R"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="SDA Hold Time select" name="TWI_SDAHOLD">
                                                  <value caption="SDA hold time off" name="OFF" value="0x00"/>
                                                  <value caption="Typical 50ns hold time" name="50NS" value="0x01"/>
                                                  <value caption="Typical 300ns hold time" name="300NS" value="0x02"/>
                                                  <value caption="Typical 500ns hold time" name="500NS" value="0x03"/>
                                                </value-group>
                                                <value-group caption="SDA Setup Time select" name="TWI_SDASETUP">
                                                  <value caption="SDA setup time is 4 clock cycles" name="4CYC" value="0x0"/>
                                                  <value caption="SDA setup time is 8 clock cycles" name="8CYC" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Inactive Bus Timeout select" name="TWI_TIMEOUT">
                                                  <value caption="Bus Timeout Disabled" name="DISABLED" value="0x00"/>
                                                  <value caption="50 Microseconds" name="50US" value="0x01"/>
                                                  <value caption="100 Microseconds" name="100US" value="0x02"/>
                                                  <value caption="200 Microseconds" name="200US" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Acknowledge Action select" name="TWI_ACKACT">
                                                  <value caption="Send ACK" name="ACK" value="0x0"/>
                                                  <value caption="Send NACK" name="NACK" value="0x1"/>
                                                </value-group>
                                                <value-group caption="Command select" name="TWI_MCMD">
                                                  <value caption="No Action" name="NOACT" value="0x00"/>
                                                  <value caption="Issue Repeated Start Condition" name="REPSTART" value="0x01"/>
                                                  <value caption="Receive or Transmit Data, depending on DIR" name="RECVTRANS" value="0x02"/>
                                                  <value caption="Issue Stop Condition" name="STOP" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Bus State select" name="TWI_BUSSTATE">
                                                  <value caption="Unknown Bus State" name="UNKNOWN" value="0x00"/>
                                                  <value caption="Bus is Idle" name="IDLE" value="0x01"/>
                                                  <value caption="This Module Controls The Bus" name="OWNER" value="0x02"/>
                                                  <value caption="The Bus is Busy" name="BUSY" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Command select" name="TWI_SCMD">
                                                  <value caption="No Action" name="NOACT" value="0x00"/>
                                                  <value caption="Used To Complete a Transaction" name="COMPTRANS" value="0x02"/>
                                                  <value caption="Used in Response to Address/Data Interrupt" name="RESPONSE" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Slave Address or Stop select" name="TWI_AP">
                                                  <value caption="Stop condition generated APIF" name="STOP" value="0x0"/>
                                                  <value caption="Address detection generated APIF" name="ADR" value="0x1"/>
                                                </value-group>
                                              </module>
                                              <module caption="Universal Synchronous and Asynchronous Receiver and Transmitter" id="I2108" name="USART">
                                                <register-group caption="Universal Synchronous and Asynchronous Receiver and Transmitter" name="USART" size="0x10">
                                                  <register caption="Baud Rate" initval="0x0000" name="BAUD" offset="0x8" rw="RW" size="2"/>
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x5" rw="RW" size="1">
                                                    <bitfield caption="Auto-baud Error Interrupt Enable" mask="0x4" name="ABEIE" rw="RW"/>
                                                    <bitfield caption="Data Register Empty Interrupt Enable" mask="0x20" name="DREIE" rw="RW"/>
                                                    <bitfield caption="Loop-back Mode Enable" mask="0x8" name="LBME" rw="RW"/>
                                                    <bitfield caption="RS485 Mode internal transmitter" mask="0x3" name="RS485" rw="RW" values="USART_RS485"/>
                                                    <bitfield caption="Receive Complete Interrupt Enable" mask="0x80" name="RXCIE" rw="RW"/>
                                                    <bitfield caption="Receiver Start Frame Interrupt Enable" mask="0x10" name="RXSIE" rw="RW"/>
                                                    <bitfield caption="Transmit Complete Interrupt Enable" mask="0x40" name="TXCIE" rw="RW"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x6" rw="RW" size="1">
                                                    <bitfield caption="Multi-processor Communication Mode" mask="0x1" name="MPCM" rw="RW"/>
                                                    <bitfield caption="Open Drain Mode Enable" mask="0x8" name="ODME" rw="RW"/>
                                                    <bitfield caption="Reciever enable" mask="0x80" name="RXEN" rw="RW"/>
                                                    <bitfield caption="Receiver Mode" mask="0x6" name="RXMODE" rw="RW" values="USART_RXMODE"/>
                                                    <bitfield caption="Start Frame Detection Enable" mask="0x10" name="SFDEN" rw="RW"/>
                                                    <bitfield caption="Transmitter Enable" mask="0x40" name="TXEN" rw="RW"/>
                                                  </register>
                                                  <register caption="Control C" initval="0x03" name="CTRLC" offset="0x7" rw="RW" size="1">
                                                    <mode name="MSPI">
                                                      <bitfield caption="Communication Mode" mask="0xc0" name="CMODE" rw="RW" values="USART_MSPI_CMODE"/>
                                                      <bitfield caption="SPI Master Mode, Clock Phase" mask="0x2" name="UCPHA" rw="RW"/>
                                                      <bitfield caption="SPI Master Mode, Data Order" mask="0x4" name="UDORD" rw="RW"/>
                                                    </mode>
                                                    <mode name="NORMAL">
                                                      <bitfield caption="Character Size" mask="0x7" name="CHSIZE" rw="RW" values="USART_NORMAL_CHSIZE"/>
                                                      <bitfield caption="Communication Mode" mask="0xc0" name="CMODE" rw="RW" values="USART_NORMAL_CMODE"/>
                                                      <bitfield caption="Parity Mode" mask="0x30" name="PMODE" rw="RW" values="USART_NORMAL_PMODE"/>
                                                      <bitfield caption="Stop Bit Mode" mask="0x8" name="SBMODE" rw="RW" values="USART_NORMAL_SBMODE"/>
                                                    </mode>
                                                  </register>
                                                  <register caption="Debug Control" initval="0x00" name="DBGCTRL" offset="0xB" rw="RW" size="1">
                                                    <bitfield caption="Debug Run" mask="0x1" name="DBGRUN" rw="RW"/>
                                                  </register>
                                                  <register caption="Event Control" initval="0x00" name="EVCTRL" offset="0xC" rw="RW" size="1">
                                                    <bitfield caption="IrDA Event Input Enable" mask="0x1" name="IREI" rw="RW"/>
                                                  </register>
                                                  <register caption="Receive Data High Byte" initval="0x00" name="RXDATAH" offset="0x1" rw="R" size="1">
                                                    <bitfield caption="Buffer Overflow" mask="0x40" name="BUFOVF" rw="R"/>
                                                    <bitfield caption="Receiver Data Register" mask="0x1" name="DATA8" rw="R"/>
                                                    <bitfield caption="Frame Error" mask="0x4" name="FERR" rw="R"/>
                                                    <bitfield caption="Parity Error" mask="0x2" name="PERR" rw="R"/>
                                                    <bitfield caption="Receive Complete Interrupt Flag" mask="0x80" name="RXCIF" rw="R"/>
                                                  </register>
                                                  <register caption="Receive Data Low Byte" initval="0x00" name="RXDATAL" offset="0x0" rw="R" size="1">
                                                    <bitfield caption="RX Data" mask="0xff" name="DATA" rw="R"/>
                                                  </register>
                                                  <register caption="IRCOM Receiver Pulse Length Control" initval="0x00" name="RXPLCTRL" offset="0xE" rw="RW" size="1">
                                                    <bitfield caption="Receiver Pulse Lenght" mask="0x7f" name="RXPL" rw="RW"/>
                                                  </register>
                                                  <register caption="Status" initval="0x00" name="STATUS" offset="0x4" rw="RW" size="1">
                                                    <bitfield caption="Break Detected Flag" mask="0x2" name="BDF" rw="RW"/>
                                                    <bitfield caption="Data Register Empty Flag" mask="0x20" name="DREIF" rw="R"/>
                                                    <bitfield caption="Inconsistent Sync Field Interrupt Flag" mask="0x8" name="ISFIF" rw="RW"/>
                                                    <bitfield caption="Receive Complete Interrupt Flag" mask="0x80" name="RXCIF" rw="R"/>
                                                    <bitfield caption="Receive Start Interrupt" mask="0x10" name="RXSIF" rw="R"/>
                                                    <bitfield caption="Transmit Interrupt Flag" mask="0x40" name="TXCIF" rw="RW"/>
                                                    <bitfield caption="Wait For Break" mask="0x1" name="WFB" rw="RW"/>
                                                  </register>
                                                  <register caption="Transmit Data High Byte" initval="0x00" name="TXDATAH" offset="0x3" rw="RW" size="1">
                                                    <bitfield caption="Transmit Data Register (CHSIZE=9bit)" mask="0x1" name="DATA8" rw="RW"/>
                                                  </register>
                                                  <register caption="Transmit Data Low Byte" initval="0x00" name="TXDATAL" offset="0x2" rw="RW" size="1">
                                                    <bitfield caption="Transmit Data Register" mask="0xff" name="DATA" rw="RW"/>
                                                  </register>
                                                  <register caption="IRCOM Transmitter Pulse Length Control" initval="0x00" name="TXPLCTRL" offset="0xD" rw="RW" size="1">
                                                    <bitfield caption="Transmit pulse length" mask="0xff" name="TXPL" rw="RW"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="RS485 Mode internal transmitter select" name="USART_RS485">
                                                  <value caption="RS485 Mode disabled" name="OFF" value="0x00"/>
                                                  <value caption="RS485 Mode External drive" name="EXT" value="0x01"/>
                                                  <value caption="RS485 Mode Internal drive" name="INT" value="0x02"/>
                                                </value-group>
                                                <value-group caption="Receiver Mode select" name="USART_RXMODE">
                                                  <value caption="Normal mode" name="NORMAL" value="0x0"/>
                                                  <value caption="CLK2x mode" name="CLK2X" value="0x1"/>
                                                  <value caption="Generic autobaud mode" name="GENAUTO" value="0x2"/>
                                                  <value caption="LIN constrained autobaud mode" name="LINAUTO" value="0x3"/>
                                                </value-group>
                                                <value-group caption="Communication Mode select" name="USART_MSPI_CMODE">
                                                  <value caption="Asynchronous Mode" name="ASYNCHRONOUS" value="0x00"/>
                                                  <value caption="Synchronous Mode" name="SYNCHRONOUS" value="0x01"/>
                                                  <value caption="Infrared Communication" name="IRCOM" value="0x02"/>
                                                  <value caption="Master SPI Mode" name="MSPI" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Character Size select" name="USART_NORMAL_CHSIZE">
                                                  <value caption="Character size: 5 bit" name="5BIT" value="0x00"/>
                                                  <value caption="Character size: 6 bit" name="6BIT" value="0x01"/>
                                                  <value caption="Character size: 7 bit" name="7BIT" value="0x02"/>
                                                  <value caption="Character size: 8 bit" name="8BIT" value="0x03"/>
                                                  <value caption="Character size: 9 bit read low byte first" name="9BITL" value="0x06"/>
                                                  <value caption="Character size: 9 bit read high byte first" name="9BITH" value="0x07"/>
                                                </value-group>
                                                <value-group caption="Communication Mode select" name="USART_NORMAL_CMODE">
                                                  <value caption="Asynchronous Mode" name="ASYNCHRONOUS" value="0x00"/>
                                                  <value caption="Synchronous Mode" name="SYNCHRONOUS" value="0x01"/>
                                                  <value caption="Infrared Communication" name="IRCOM" value="0x02"/>
                                                  <value caption="Master SPI Mode" name="MSPI" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Parity Mode select" name="USART_NORMAL_PMODE">
                                                  <value caption="No Parity" name="DISABLED" value="0x00"/>
                                                  <value caption="Even Parity" name="EVEN" value="0x02"/>
                                                  <value caption="Odd Parity" name="ODD" value="0x03"/>
                                                </value-group>
                                                <value-group caption="Stop Bit Mode select" name="USART_NORMAL_SBMODE">
                                                  <value caption="1 stop bit" name="1BIT" value="0x0"/>
                                                  <value caption="2 stop bits" name="2BIT" value="0x1"/>
                                                </value-group>
                                              </module>
                                              <module caption="User Row" id="I2600" name="USERROW">
                                                <register-group caption="User Row" name="USERROW" size="0x20">
                                                  <register caption="User Row Byte 0" name="USERROW0" offset="0x00" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 1" name="USERROW1" offset="0x01" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 2" name="USERROW2" offset="0x02" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 3" name="USERROW3" offset="0x03" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 4" name="USERROW4" offset="0x04" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 5" name="USERROW5" offset="0x05" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 6" name="USERROW6" offset="0x06" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 7" name="USERROW7" offset="0x07" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 8" name="USERROW8" offset="0x08" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 9" name="USERROW9" offset="0x09" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 10" name="USERROW10" offset="0x0A" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 11" name="USERROW11" offset="0x0B" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 12" name="USERROW12" offset="0x0C" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 13" name="USERROW13" offset="0x0D" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 14" name="USERROW14" offset="0x0E" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 15" name="USERROW15" offset="0x0F" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 16" name="USERROW16" offset="0x10" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 17" name="USERROW17" offset="0x11" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 18" name="USERROW18" offset="0x12" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 19" name="USERROW19" offset="0x13" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 20" name="USERROW20" offset="0x14" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 21" name="USERROW21" offset="0x15" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 22" name="USERROW22" offset="0x16" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 23" name="USERROW23" offset="0x17" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 24" name="USERROW24" offset="0x18" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 25" name="USERROW25" offset="0x19" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 26" name="USERROW26" offset="0x1A" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 27" name="USERROW27" offset="0x1B" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 28" name="USERROW28" offset="0x1C" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 29" name="USERROW29" offset="0x1D" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 30" name="USERROW30" offset="0x1E" rw="RW" size="1"/>
                                                  <register caption="User Row Byte 31" name="USERROW31" offset="0x1F" rw="RW" size="1"/>
                                                </register-group>
                                              </module>
                                              <module caption="Virtual Ports" id="I2103" name="VPORT">
                                                <register-group caption="Virtual Ports" name="VPORT" size="0x4">
                                                  <register caption="Data Direction" name="DIR" offset="0x0" rw="RW" size="1"/>
                                                  <register caption="Input Value" name="IN" offset="0x2" rw="RW" size="1"/>
                                                  <register caption="Interrupt Flags" initval="0x00" name="INTFLAGS" offset="0x3" rw="RW" size="1">
                                                    <bitfield caption="Pin Interrupt" mask="0xff" name="INT" rw="RW"/>
                                                  </register>
                                                  <register caption="Output Value" name="OUT" offset="0x1" rw="RW" size="1"/>
                                                </register-group>
                                              </module>
                                              <module caption="Voltage reference" id="I2600" name="VREF">
                                                <register-group caption="Voltage reference" name="VREF" size="0x2">
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="ADC0 reference select" mask="0x70" name="ADC0REFSEL" rw="RW" values="VREF_ADC0REFSEL"/>
                                                    <bitfield caption="DAC0/AC0 reference select" mask="0x7" name="DAC0REFSEL" rw="RW" values="VREF_DAC0REFSEL"/>
                                                  </register>
                                                  <register caption="Control B" initval="0x00" name="CTRLB" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="ADC0 reference enable" mask="0x2" name="ADC0REFEN" rw="RW"/>
                                                    <bitfield caption="DAC0/AC0 reference enable" mask="0x1" name="DAC0REFEN" rw="RW"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="ADC0 reference select select" name="VREF_ADC0REFSEL">
                                                  <value caption="Voltage reference at 0.55V" name="0V55" value="0x00"/>
                                                  <value caption="Voltage reference at 1.1V" name="1V1" value="0x01"/>
                                                  <value caption="Voltage reference at 2.5V" name="2V5" value="0x02"/>
                                                  <value caption="Voltage reference at 4.34V" name="4V34" value="0x03"/>
                                                  <value caption="Voltage reference at 1.5V" name="1V5" value="0x04"/>
                                                </value-group>
                                                <value-group caption="DAC0/AC0 reference select select" name="VREF_DAC0REFSEL">
                                                  <value caption="Voltage reference at 0.55V" name="0V55" value="0x00"/>
                                                  <value caption="Voltage reference at 1.1V" name="1V1" value="0x01"/>
                                                  <value caption="Voltage reference at 2.5V" name="2V5" value="0x02"/>
                                                  <value caption="Voltage reference at 4.34V" name="4V34" value="0x03"/>
                                                  <value caption="Voltage reference at 1.5V" name="1V5" value="0x04"/>
                                                </value-group>
                                              </module>
                                              <module caption="Watch-Dog Timer" id="I2127" name="WDT">
                                                <register-group caption="Watch-Dog Timer" name="WDT" size="0x2">
                                                  <register caption="Control A" initval="0x00" name="CTRLA" offset="0x0" rw="RW" size="1">
                                                    <bitfield caption="Period" mask="0xf" name="PERIOD" rw="RW" values="WDT_PERIOD"/>
                                                    <bitfield caption="Window" mask="0xf0" name="WINDOW" rw="RW" values="WDT_WINDOW"/>
                                                  </register>
                                                  <register caption="Status" initval="0x00" name="STATUS" offset="0x1" rw="RW" size="1">
                                                    <bitfield caption="Lock enable" mask="0x80" name="LOCK" rw="RW"/>
                                                    <bitfield caption="Syncronization busy" mask="0x1" name="SYNCBUSY" rw="R"/>
                                                  </register>
                                                </register-group>
                                                <value-group caption="Period select" name="WDT_PERIOD">
                                                  <value caption="Watch-Dog timer Off" name="OFF" value="0x00"/>
                                                  <value caption="8 cycles (8ms)" name="8CLK" value="0x01"/>
                                                  <value caption="16 cycles (16ms)" name="16CLK" value="0x02"/>
                                                  <value caption="32 cycles (32ms)" name="32CLK" value="0x03"/>
                                                  <value caption="64 cycles (64ms)" name="64CLK" value="0x04"/>
                                                  <value caption="128 cycles (0.128s)" name="128CLK" value="0x05"/>
                                                  <value caption="256 cycles (0.256s)" name="256CLK" value="0x06"/>
                                                  <value caption="512 cycles (0.512s)" name="512CLK" value="0x07"/>
                                                  <value caption="1K cycles (1.0s)" name="1KCLK" value="0x08"/>
                                                  <value caption="2K cycles (2.0s)" name="2KCLK" value="0x09"/>
                                                  <value caption="4K cycles (4.1s)" name="4KCLK" value="0x0A"/>
                                                  <value caption="8K cycles (8.2s)" name="8KCLK" value="0x0B"/>
                                                </value-group>
                                                <value-group caption="Window select" name="WDT_WINDOW">
                                                  <value caption="Window mode off" name="OFF" value="0x00"/>
                                                  <value caption="8 cycles (8ms)" name="8CLK" value="0x01"/>
                                                  <value caption="16 cycles (16ms)" name="16CLK" value="0x02"/>
                                                  <value caption="32 cycles (32ms)" name="32CLK" value="0x03"/>
                                                  <value caption="64 cycles (64ms)" name="64CLK" value="0x04"/>
                                                  <value caption="128 cycles (0.128s)" name="128CLK" value="0x05"/>
                                                  <value caption="256 cycles (0.256s)" name="256CLK" value="0x06"/>
                                                  <value caption="512 cycles (0.512s)" name="512CLK" value="0x07"/>
                                                  <value caption="1K cycles (1.0s)" name="1KCLK" value="0x08"/>
                                                  <value caption="2K cycles (2.0s)" name="2KCLK" value="0x09"/>
                                                  <value caption="4K cycles (4.1s)" name="4KCLK" value="0x0A"/>
                                                  <value caption="8K cycles (8.2s)" name="8KCLK" value="0x0B"/>
                                                </value-group>
                                              </module>
                                            </modules>
                                            <pinouts>
                                              <pinout name="SOIC14">
                                                <pin pad="VDD" position="1"/>
                                                <pin pad="PA4" position="2"/>
                                                <pin pad="PA5" position="3"/>
                                                <pin pad="PA6" position="4"/>
                                                <pin pad="PA7" position="5"/>
                                                <pin pad="PB3" position="6"/>
                                                <pin pad="PB2" position="7"/>
                                                <pin pad="PB1" position="8"/>
                                                <pin pad="PB0" position="9"/>
                                                <pin pad="PA0" position="10"/>
                                                <pin pad="PA1" position="11"/>
                                                <pin pad="PA2" position="12"/>
                                                <pin pad="PA3" position="13"/>
                                                <pin pad="GND" position="14"/>
                                              </pinout>
                                            </pinouts>
                                          </avr-tools-device-file>"""))
