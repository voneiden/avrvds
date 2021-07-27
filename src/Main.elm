port module Main exposing (..)

import Array exposing (Array, get)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import CustomMarkdown exposing (defaultHtmlRenderer)
import Data.Chip exposing (Bitfield, ChipDefinition, ChipDevice, ChipDeviceModule, ChipModule, ChipPin, ChipPinout, ChipVariant, Register, RegisterGroup, Signal, chipDefinitionDecoder)
import Data.ChipCdef exposing (ChipCdef, chipCDEFDecoder)
import Data.ChipTypes exposing (DeviceModuleCategory(..), Module, Pad(..), PinoutType(..))
import Data.Tome as Tome exposing (Chapter, Section, SubSection, Tome, parseTome)
import Data.Util.DeviceModuleCategory as DeviceModuleCategory
import Data.Util.Module as Module
import Data.Util.Pad as Pad
import Dict exposing (Dict, keys)
import Dict.Extra exposing (groupBy)
import Html exposing (Attribute, Html, a, button, div, h2, h3, h4, input, label, text)
import Html.Attributes exposing (checked, class, href, id, style, type_)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, stopPropagationOn)
import Html.Lazy exposing (lazy2)
import Http exposing (Error(..))
import Json.Decode as Decoder
import Json.Encode
import List exposing (concat, drop, filter, filterMap, length, map, member, reverse, sortBy, take)
import Markdown.Parser
import Markdown.Renderer exposing (Renderer)
import Maybe exposing (withDefault)
import Parser exposing (DeadEnd)
import String exposing (fromInt, join, replace)
import Url
import Util.BitMask exposing (BitMask(..), bitLength)
import Util.ParserUtil exposing (parserDeadEndsToString)



-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


type alias Session =
    { key : Nav.Key
    , flags: Flags
    }



-- MODEL


type alias DefinitionState =
    { chipDefinition : ChipDefinition
    , variant : ChipVariant
    , device: ChipDevice
    , pinout : ChipPinout
    , visibleModules : List DeviceModuleCategory
    , highlightModule : Maybe Module
    , selectedSignal : Maybe Signal
    , highlightRelatedCategories : List DeviceModuleCategory
    } -- TODO well CDEF can't go here can it now?


type Model
    = RequestFailed Session Http.Error
    | ParsingTomeFailed Session (List DeadEnd)
    | InsufficientData Session String
    | Loading Session (Maybe DefinitionState) (Maybe ChipCdef) (Maybe Tome)
    | Success Session DefinitionState ChipCdef Tome


modelSession : Model -> Session
modelSession model =
    case model of
        RequestFailed session _ ->
            session
        ParsingTomeFailed session _ ->
            session
        InsufficientData session _ ->
            session
        Loading session _ _ _ ->
            session
        Success session _ _ _ ->
            session


type alias Flags =
    { root: String
    , visibleModules: Maybe (List String)
    }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ key =
    --(Loading, getDefinition)
    ( Loading (Session key flags) Nothing Nothing Nothing, Cmd.batch [getDefinition flags.root "ATtiny814", getCDEF flags.root "ATtiny814", getTome flags.root] )


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    UrlRequested urlRequest


onUrlChange : Url.Url -> Msg
onUrlChange url =
    UrlChanged url

type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url.Url
    | LoadDevice String
    | ReceiveDefinition (Result Http.Error ChipDefinition)
    | ReceiveCDEF (Result Http.Error ChipCdef)
    | ReceiveTome (Result Http.Error String)
    | ToggleVisibleCategory DeviceModuleCategory
    | HighlightModule Module
    | SelectSignal (Maybe Signal)
    | ClearHighlight


-- PORTS
port storeVisibleModules : Json.Encode.Value -> Cmd msg

toggleVisible : List DeviceModuleCategory -> DeviceModuleCategory -> List DeviceModuleCategory
toggleVisible categories category =
    case member category categories of
        True ->
            filter (\c -> c /= category) categories

        False ->
            categories ++ [ category ]

modelCDEF : Model -> Maybe ChipCdef
modelCDEF model =
    case model of
        Loading _ _ cdef _ ->
            cdef
        Success _ _ cdef _ ->
            Just cdef
        _ ->
            Nothing

modelTome : Model -> Maybe Tome
modelTome model =
    case model of
        Loading _ _ _ tome ->
            tome
        Success _ _ _ tome ->
            Just tome
        _ ->
            Nothing

-- For Msg's that are not model specific
updateGeneric : Msg -> Model -> ( Model, Cmd Msg)
updateGeneric msg model =
    let
        session = modelSession model
    in
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    --( Test2 session url.fragment, Nav.pushUrl session.key (Url.toString url) )
                    (model, Nav.pushUrl session.key (Url.toString url) )

                --(Test2 session url.fragment, Cmd.none)
                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            case model of
                --Test2 _ _ ->
                --    ( Test2 session (map2 (++) (Just "NAV!") url.fragment), Cmd.none )
                _ ->
                    ( model, Cmd.none )

        LoadDevice definitionId ->
            let
                cdef = modelCDEF model
                tome = modelTome model
            in
            ( Loading session Nothing cdef tome, Cmd.batch [getDefinition session.flags.root definitionId, getCDEF session.flags.root definitionId] )
        _ -> (model, Cmd.none)


encodeVisibleModules : List DeviceModuleCategory -> Json.Encode.Value
encodeVisibleModules visibleModules =
    Json.Encode.list (\x -> Json.Encode.string <| DeviceModuleCategory.toString x ) visibleModules


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading session maybeDefinition maybeCdef maybeTome->
            case msg of
                ReceiveDefinition result ->
                    case result of
                        Ok chipDefinition ->
                            let
                                maybeVariant = get 0 chipDefinition.variants
                                maybePinout = get 0 chipDefinition.pinouts
                                maybeDevice = get 0 chipDefinition.devices
                            in
                                case maybeVariant of
                                    Nothing -> ( InsufficientData session "Definition variants list is empty", Cmd.none )
                                    Just variant ->
                                        case maybePinout of
                                            Nothing -> (InsufficientData session "Definition pinouts list is empty", Cmd.none)
                                            Just pinout ->
                                                case maybeDevice of
                                                    Nothing -> (InsufficientData session "Definition devices list is empty", Cmd.none)
                                                    Just device ->
                                                        let
                                                            visibleModules = case session.flags.visibleModules of
                                                                Just list ->
                                                                    List.filterMap DeviceModuleCategory.fromString list
                                                                Nothing ->
                                                                    [ IO, ANALOG, INTERFACE, TIMER, OTHER ]
                                                            definition = DefinitionState chipDefinition variant device pinout visibleModules Nothing Nothing []
                                                        in
                                                        case (maybeCdef, maybeTome) of
                                                            (Just cdef, Just tome) ->
                                                                ( Success session definition cdef tome, Cmd.none )
                                                            _ ->
                                                                ( Loading session (Just definition) maybeCdef maybeTome, Cmd.none )
                        Err error ->
                            ( RequestFailed session error, Cmd.none )

                ReceiveCDEF result ->
                    case result of
                        Ok cdef ->
                            case (maybeDefinition, maybeTome) of
                                (Just definition, Just tome) ->
                                    ( Success session definition cdef tome, Cmd.none )
                                _ ->
                                    ( Loading session maybeDefinition (Just cdef) maybeTome, Cmd.none )
                        Err err ->
                            ( RequestFailed session err, Cmd.none)
                ReceiveTome result ->
                    case result of
                        Ok tomeString ->
                            let
                                tomeResult = parseTome tomeString
                            in
                            case tomeResult of
                                Ok tome ->
                                    case (maybeDefinition, maybeCdef) of
                                        (Just definition, Just cdef) ->
                                            ( Success session definition cdef tome, Cmd.none)
                                        _ ->
                                            ( Loading session maybeDefinition maybeCdef (Just tome), Cmd.none)
                                Err err ->
                                    ( ParsingTomeFailed session err, Cmd.none )
                        Err err ->
                            ( RequestFailed session err, Cmd.none)
                _ ->
                    updateGeneric msg model
        Success session definition cdef tome ->
            case msg of
                ToggleVisibleCategory category ->
                    let
                        newVisibleModules = toggleVisible definition.visibleModules category
                    in
                    ( Success session { definition | visibleModules = newVisibleModules } cdef tome, storeVisibleModules (encodeVisibleModules newVisibleModules) )

                HighlightModule category ->
                    ( Success session { definition | highlightModule = Just category } cdef tome, Cmd.none )

                ClearHighlight ->
                    ( Success session { definition | highlightModule = Nothing } cdef tome, Cmd.none )

                SelectSignal pin ->
                    ( Success session { definition | selectedSignal = pin } cdef tome, Cmd.none)

                _ ->
                    updateGeneric msg model
        _ ->
            updateGeneric msg model

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "AVR Visual Datasheet"
    , body =
        [ div []
            [ viewModel model
            ]
        ]
    }


soicRightPins : List ChipPin -> List ChipPin
soicRightPins pins =
    let
        count =
            length pins // 2
    in
    drop count pins |> take count |> reverse


soicLeftPins : List ChipPin -> List ChipPin
soicLeftPins pins =
    take (length pins // 2) pins


getSignalsFromDevice : DefinitionState -> ChipDevice -> List Signal
getSignalsFromDevice state device =
    map .instances (filter (\m -> member m.group state.visibleModules) device.modules)
        |> concat
        |> map .signals
        |> filterMap identity
        |> concat


padSignals : Pad -> List Signal -> List Signal
padSignals pad signals =
    filter (\p -> p.pad == pad) signals


type alias SignalFit =
    { pads : List Pad
    , signals : List Signal
    }


signalGroupFitsPads : List Signal -> List Pad -> List Pad -> Maybe (List Pad)
signalGroupFitsPads signals availablePads usedPads =
    case signals of
        [] ->
            Just usedPads

        x :: xs ->
            if member x.pad availablePads then
                signalGroupFitsPads xs (filter ((/=) x.pad) availablePads) (usedPads ++ [ x.pad ])

            else
                Nothing


signalGroupsFitPads : List (List Signal) -> List Pad -> Maybe ( List Pad, List Signal )
signalGroupsFitPads signalGroups availablePads =
    case signalGroups of
        [] ->
            Nothing

        x :: xs ->
            case signalGroupFitsPads x availablePads [] of
                Nothing ->
                    signalGroupsFitPads xs availablePads

                Just usedPads ->
                    --let
                        --tmp =
                        --     Debug.log ("Fitted" ++ toString x) (length x)
                    --in
                    Just ( filter (\p -> not (member p usedPads)) availablePads, x )


fitSignalGroups : List (List Signal) -> List Pad -> List Pad -> List Signal
fitSignalGroups signalGroups initPads pads =
    case signalGroups of
        [] ->
            []

        signalGroups_ ->
            case signalGroupsFitPads signalGroups_ pads of
                Nothing ->
                    if initPads == pads then
                        --let
                        --    tmp =
                        --        Debug.log ("FORCE FITTING REMAINING" ++ toString signalGroups_) (length signalGroups_)
                        --in
                        concat signalGroups_

                    else
                        fitSignalGroups signalGroups_ initPads initPads

                Just ( remainingPads, fittedSignals ) ->
                    fittedSignals ++ fitSignalGroups (filter ((/=) fittedSignals) signalGroups) initPads remainingPads

isPort : Pad -> Bool
isPort pad =
    case pad of
        VDD ->
            False

        GND ->
            False

        _ ->
            True

filterPortPads : List Pad -> List Pad
filterPortPads pads =
    filter isPort pads


getDefault : comparable -> Dict comparable v -> v -> v
getDefault targetKey dict default =
    withDefault default <| Dict.get targetKey dict


sortSignals : List Pad -> List Signal -> List Signal
sortSignals pads signals =
    let
        signalsGroupedByFunction =
            groupBy .function signals

        functions =
            keys signalsGroupedByFunction

        sortedFunctions =
            reverse <| sortBy (\f -> length <| getDefault f signalsGroupedByFunction []) functions

        portPads =
            filterPortPads pads
    in
    fitSignalGroups (map (\f -> getDefault f signalsGroupedByFunction []) sortedFunctions) portPads portPads


signalToString : Signal -> String
signalToString signal =
    let
        prefix =
            case signal.group of
                "OUT" ->
                    signal.function ++ "-" ++ signal.group

                _ ->
                    signal.group

        result =
            case signal.index of
                Nothing ->
                    prefix

                Just index ->
                    prefix ++ fromInt index
    in
    case signal.function of
        "IOPORT" ->
            Pad.toString signal.pad
        _ ->
            replace "_" "-" result


highlightSignal : DefinitionState -> Signal -> Bool
highlightSignal state signal =
    case state.highlightModule of
        Just c ->
            signal.deviceModule == c

        Nothing ->
            False

highlightSignalClass : Bool -> Maybe String
highlightSignalClass highlight =
    case highlight of
        True -> Just "highlight"
        False -> Nothing

selectSignalClass : DefinitionState -> Signal -> Maybe String
selectSignalClass state signal =
    case state.selectedSignal of
        Just selectedSignal ->
            if signal.deviceModule == selectedSignal.deviceModule then
                if signal.function == selectedSignal.function then
                    Just "selected"
                else
                    Just "selected-related"
            else
                Just "selected-unrelated"
        Nothing -> Nothing


viewPinSignal : DefinitionState -> Signal -> Html Msg
viewPinSignal state signal =
    div
        [ class <| cls
            [ Just "pin-signal"
            , Just signal.function
            , highlightSignalClass <| highlightSignal state signal
            , selectSignalClass state signal
            ]
        , onMouseEnter <| HighlightModule signal.deviceModule
        , onMouseLeave ClearHighlight
        --, onClick <| SelectSignal (Just signal)
        , stopPropagationOn "click" (Decoder.succeed (SelectSignal (Just signal), True))
        ]
        [
            div [ class "pin-label-wrapper"] [
                div [ class "pin-label" ] [ text <| signalToString signal]
            ]
        ]


viewPin : DefinitionState -> List Signal -> ChipPin -> Html Msg
viewPin state signals pin =
    let
        nonport =
            case isPort pin.pad of
                False -> [div [ class (cls [Just "pin-signal", Just (Pad.toString pin.pad)]) ] [ div [ class "pin-label" ] [ text <| Pad.toString pin.pad ]]]
                True -> []
    in
        div [ class "pin" ] <|

            [ div [ class "pin-leg" ] [ div [ class "pin-label" ] [ text <| fromInt pin.position ] ] ]
            ++
            nonport
            ++
            map (viewPinSignal state) (padSignals pin.pad signals)


cls : List (Maybe String) -> String
cls xs =
    filterMap identity xs
        |> join " "

viewModuleSelectCheckbox : DefinitionState -> DeviceModuleCategory -> Html Msg
viewModuleSelectCheckbox state category =
    div []
        [ input
            [ type_ "checkbox"
            , checked <| member category state.visibleModules
            , onClick <| ToggleVisibleCategory category
            ]
            []
        , label [] [ text <| DeviceModuleCategory.toString category ]
        ]


viewModuleSelect : DefinitionState -> Html Msg
viewModuleSelect state =
    div [ id "module-select" ] <| map (viewModuleSelectCheckbox state) DeviceModuleCategory.list


viewChip : DefinitionState -> ChipCdef -> List Signal -> Html Msg
viewChip state cdef signals =
    let
        leftPins =
            soicLeftPins state.pinout.pins

        rightPins =
            soicRightPins state.pinout.pins

        leftPads =
            map .pad leftPins

        rightPads =
            map .pad rightPins

        leftSignals =
            sortSignals leftPads <| filter (\s -> member s.pad leftPads) signals

        rightSignals =
            sortSignals rightPads <| filter (\s -> member s.pad rightPads) signals
    in
    case state.pinout.pinoutType of
        SOIC ->
            div [ id "chip-container", onClick (SelectSignal Nothing)]
                [ viewModuleSelect state
                , div [ id "chip-view", class "soic dark" ]
                    [ div [ class "soic-left" ] <| map (viewPin state leftSignals) (soicLeftPins state.pinout.pins)
                    , div [ class "soic-middle" ]
                        [ div [ class "pin1-marker" ] []
                        , div [ class "module-name" ] [ text state.device.name ]
                        ]
                    , div [ class "soic-right" ] <| map (viewPin state rightSignals) (soicRightPins state.pinout.pins)
                    ]
                ]

--maskToRange : String -> String

viewBitfield : Bitfield -> Html Msg
viewBitfield bitfield =

    let
        range =
            case bitfield.mask of
                BitIndex bit ->
                    "[Bit " ++ String.fromInt bit ++ "]"
                BitRange high low ->
                    "[" ++ String.fromInt low ++ ":" ++ String.fromInt high ++ "]"
    in
    div [ class "bitfield" ] <| [h4 [] [text <| bitfield.name ++ " " ++ range ++ " " ++ bitfield.caption]]
    ++
    render (defaultHtmlRenderer Nothing Nothing) (Maybe.withDefault "" bitfield.description)

bitfieldSorter : Bitfield -> Int
bitfieldSorter field =
    case field.mask of
        BitIndex i ->
            i
        BitRange _ j ->
            j


viewRegister : Register -> Html Msg
viewRegister register =
    div [ class "register" ] <|
        [ h3 [] [text register.name]]
        ++ render (defaultHtmlRenderer Nothing Nothing) (Maybe.withDefault "" register.description)
        ++ case register.bitfields of
            Nothing ->
                [text "No bitfields"]
            Just bitfields ->
                map viewBitfield (List.sortBy bitfieldSorter bitfields)



viewRegisterGroup : Bool -> RegisterGroup -> Html Msg
viewRegisterGroup caption registerGroup =
    let
        registers = map viewRegister registerGroup.registers
    in
    case caption of
        True ->
            div [ class "register-group"] <| [ div [] [ text registerGroup.caption]] ++ registers
        False ->
            div [ class "register-group"] <| registers

-- note on signal logic
-- group by "function"
-- name by "group" + index (if contains more than 1!)
viewModule : DefinitionState -> ChipCdef -> Html Msg
viewModule state cdef =
    div [id "module-info"] <|
        case state.selectedSignal of
            Nothing ->
                [viewModulesOverview state]
            Just signal ->
                let
                    chipModules = filter (\m -> m.name == signal.deviceModule) state.chipDefinition.modules
                in
                case chipModules of
                    chipModule::_ ->
                         [h2 [] [ text <| Module.toString chipModule.name ++ " - " ++ chipModule.caption]]
                         ++
                         case chipModule.registerGroups of
                             Just registerGroups ->
                                 let
                                     captionGroup =
                                         case length registerGroups of
                                             1 -> False

                                             _ -> True
                                 in
                                 map (lazy2 viewRegisterGroup captionGroup) registerGroups
                             Nothing ->
                                 [ text "No registers"]

                    [] ->
                        [text <| "No matching ChipModule found for DeviceModule " ++ Module.toString signal.deviceModule]

flexBasis : Int -> List (Attribute Msg)
flexBasis basis =
    let
        basis_ = fromInt basis
    in
    [style "flex-basis" (basis_ ++ "px"), style "flex-grow" basis_]

viewBitfieldOverview : BitfieldOverview -> Html Msg
viewBitfieldOverview bitfieldOverview =
    case bitfieldOverview of
        JustBitfield bitfield ->
            div ([ class "bitfield-overview"] ++ flexBasis (bitLength bitfield.mask)) <|
                case bitfield.mask of
                    BitIndex index ->
                        [text bitfield.name]
                    BitRange high low ->
                        [text <| bitfield.name ++ "[" ++ fromInt high ++ ":" ++ fromInt low ++ "]"]
        BlankBitfield length ->
            div ([ class "bitfield-overview blank-bitfield"] ++ flexBasis (length)) [text ""]


type BitfieldOverview =
    JustBitfield Bitfield | BlankBitfield Int


fillBitfieldGapsHelper : Int -> List Bitfield -> List (BitfieldOverview)
fillBitfieldGapsHelper lastIndex bitfields =
    let
        createBlank : Int -> Int -> List BitfieldOverview
        createBlank high low =
            let
                blankLength = high - low - 1
            in
            if (blankLength > 0) then
                [BlankBitfield blankLength]
            else
                []
    in
    case bitfields of
        field :: otherFields ->
            case field.mask of
                BitIndex index ->
                    createBlank lastIndex index ++ JustBitfield field :: fillBitfieldGapsHelper index otherFields
                BitRange high low ->
                    createBlank lastIndex high ++ JustBitfield field :: fillBitfieldGapsHelper low otherFields
        [] ->
            []
-- datasheet is high byte to low byte, so use the same
fillBitfieldGaps : List Bitfield -> List (BitfieldOverview)
fillBitfieldGaps bitfields =
    fillBitfieldGapsHelper 8 (List.reverse <| List.sortBy bitfieldSorter bitfields)

viewRegisterOverivew : Register -> Html Msg
viewRegisterOverivew register =
    div [ class "register-overview" ] <|
        case register.bitfields of
            Just bitfields ->
                 map viewBitfieldOverview (fillBitfieldGaps bitfields)
            Nothing ->
                [text "No bitfields"]

viewRegisterGroupOverivew : RegisterGroup -> List (Html Msg)
viewRegisterGroupOverivew registerGroup =
    map viewRegisterOverivew registerGroup.registers

viewModuleOverview : ChipModule -> Maybe (Html Msg)
viewModuleOverview chipModule =
    case chipModule.registerGroups of
        Just registerGroups ->
            Just <| div [ class "module-overview" ] <|
                [ div [] [ text <| Module.toString chipModule.name ++ " - " ++ chipModule.caption]]
                ++
                concat (map viewRegisterGroupOverivew registerGroups)
        Nothing ->
            Nothing

viewModulesOverview : DefinitionState -> Html Msg
viewModulesOverview state =
    div [ class "modules-overview" ] <| [] ++ filterMap viewModuleOverview state.chipDefinition.modules

viewChipSelect : DefinitionState -> Html Msg
viewChipSelect state =
    div []
        [ a [href "#", onClick <| LoadDevice "ATtiny202"] [ text "ATtiny202" ]
        , text " | "
        , a [href "#", onClick <| LoadDevice "ATtiny814"] [ text "ATtiny814" ]
        ]

debugR : Result String (List (Html Msg)) -> List (Html Msg)
debugR r =
    case r of
        Ok x -> x
        Err err -> [div [] [text err]]

render : Renderer (Html Msg) -> String -> List (Html Msg)
render renderer markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)
        |> debugR
        --|> Result.withDefault [text "Markdown render failed"]


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"

-- TODO CHECK OUT https://package.elm-lang.org/packages/lazamar/dict-parser/latest/Parser-Dict for faster code parsing

viewSubSection : List Tome.Document -> ChipCdef -> SubSection -> Html Msg
viewSubSection documents cdef subSection =
    div [ class "subsection"] <|
        [ h3 [] [text subSection.title]]
        ++
        render (defaultHtmlRenderer (Just cdef) (Just documents)) subSection.body

viewSection : List Tome.Document -> ChipCdef -> Section -> Html Msg
viewSection documents cdef section =
    div [ class "section"] <|
        [ h2 [] [text section.title]]
        ++
        render (defaultHtmlRenderer (Just cdef) (Just documents)) section.body
        ++
        List.map (viewSubSection documents cdef) section.subSections

viewChapter : ChipCdef -> Chapter -> Html Msg
viewChapter cdef chapter =
    -- TODO some kind of logic to determine what topic we want to show
    div [] <| List.map (viewSection chapter.documents cdef) chapter.sections

viewTome : DefinitionState -> ChipCdef -> Tome -> Html Msg
viewTome state cdef tome =
    let
        maybeAlias = Dict.get state.device.name tome.aliases
    in
    case maybeAlias of
        Just alias ->
            let
                chapter = List.filter (\c -> c.title == alias) tome.chapters
            in
            div [id "tome"] <|
                case chapter of
                    [c] ->
                        [lazy2 viewChapter cdef c]
                    _ ->
                        [text "Selected device has invalid documentation alias"]
        Nothing ->
            div [] [text "Selected device has no extra documentation."]


viewModel : Model -> Html Msg
viewModel model =
    case model of
        RequestFailed _ error ->
            div []
                [ text "Unable to load device definition: ",
                case error of
                    BadBody message ->
                        text <| "Decoding error:" ++ message

                    BadUrl message ->
                        text <| "Bad url" ++ message

                    Timeout ->
                        text "Request timed out"

                    NetworkError ->
                        text "Network error"

                    BadStatus statusCode ->
                        text <| "Bad status response:" ++ fromInt statusCode
                , button [ onClick <| LoadDevice "ATtiny202" ] [ text "Try Again!" ]
                ]
        ParsingTomeFailed _ error ->
            div [] <| [ text ("Parsing Tome failed: " ++ (parserDeadEndsToString error))]
        InsufficientData _ reason ->
            div [] [ text "Received device definition is invalid: "
                   , text reason
                   ]
        Loading _ _ _ _ ->
            text "Loading..."

        Success _ state cdef tome ->
            div []
                [ div []
                    [ viewChipSelect state
                    , viewChip state cdef  (sortSignals (map .pad state.pinout.pins) (getSignalsFromDevice state state.device))
                    , div [id "info-row"]
                         [ viewModule state cdef
                         , viewTome state cdef tome
                         ]
                    ]
                ]




getDefinition : String -> String -> Cmd Msg
getDefinition root definitionId =
    Http.get
        { url = root ++ "data/" ++ definitionId ++ ".json"
        , expect = Http.expectJson ReceiveDefinition chipDefinitionDecoder
        }


getCDEF : String -> String -> Cmd Msg
getCDEF root definitionId =
    Http.get
        { url = root ++ "data/" ++ definitionId ++ "_cdef.json"
        , expect = Http.expectJson ReceiveCDEF chipCDEFDecoder
        }

getTome : String  -> Cmd Msg
getTome root =
    Http.get
        { url = root ++ "data/tome.md"
        , expect = Http.expectString ReceiveTome
        }
