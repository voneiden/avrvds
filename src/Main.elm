module Main exposing (..)

--import Html.Attributes exposing (..)

import Array exposing (Array, get)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import CustomMarkdown exposing (defaultHtmlRenderer)
import Data.Chip exposing (Bitfield, ChipDefinition, ChipDevice, ChipDeviceModule, ChipPin, ChipPinout, ChipVariant, Register, RegisterGroup, Signal, chipDefinitionDecoder)
import Data.ChipTypes exposing (DeviceModuleCategory(..), Module, Pad(..), PinoutType(..))
import Data.Util.DeviceModuleCategory as DeviceModuleCategory
import Data.Util.Module as Module
import Data.Util.Pad as Pad
--import Debug exposing (toString)
import Dict exposing (Dict, keys)
import Dict.Extra exposing (groupBy)
import Html exposing (Html, a, button, div, h2, h3, h4, input, label, text)
import Html.Attributes exposing (checked, class, href, id, type_)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, stopPropagationOn)
import Http exposing (Error(..))
import Json.Decode as Decoder
import List exposing (concat, drop, filter, filterMap, length, map, member, reverse, sortBy, take)
import Markdown.Parser
import Markdown.Renderer exposing (Renderer)
import Maybe exposing (map2, withDefault)
import String exposing (endsWith, fromInt, join, replace)
import Tuple exposing (first, second)
import Url



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
    , root: String
    }



-- MODEL


type alias State =
    { chipDefinition : ChipDefinition
    , variant : ChipVariant
    , device: ChipDevice
    , pinout : ChipPinout
    , view : Int
    , pin : Int
    , visibleModules : List DeviceModuleCategory
    , highlightModule : Maybe Module
    , selectedSignal : Maybe Signal
    , hilightRelatedCategories : List DeviceModuleCategory
    }


type Model
    = Failed Session Http.Error
    | InvalidState Session
    | Loading Session
    | Success Session State
    | Test1 Session
    | Test2 Session (Maybe String)


modelSession : Model -> Session
modelSession model =
    case model of
        Failed session _ ->
            session

        Loading session ->
            session

        Test1 session ->
            session

        Test2 session _ ->
            session

        Success session _ ->
            session

        InvalidState session ->
            session


type alias Flags =
    { root: String
    }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    --(Loading, getDefinition)
    ( Loading <| Session key flags.root, getDefinition flags.root "ATtiny814" )


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    UrlRequested urlRequest


onUrlChange : Url.Url -> Msg
onUrlChange url =
    UrlChanged url


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url.Url
    | RequestDefinition String
    | ReceiveDefinition (Result Http.Error ChipDefinition)
    | ToggleVisibleCategory DeviceModuleCategory
    | HighlightModule Module
    | SelectSignal (Maybe Signal)
    | ClearHighlight


toggleVisible : List DeviceModuleCategory -> DeviceModuleCategory -> List DeviceModuleCategory
toggleVisible categories category =
    case member category categories of
        True ->
            filter (\c -> c /= category) categories

        False ->
            categories ++ [ category ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            modelSession model
    in
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( Test2 session url.fragment, Nav.pushUrl session.key (Url.toString url) )

                --(Test2 session url.fragment, Cmd.none)
                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            case model of
                Test2 _ _ ->
                    ( Test2 session (map2 (++) (Just "NAV!") url.fragment), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RequestDefinition definitionId ->
            ( Loading session, getDefinition session.root definitionId )

        ReceiveDefinition result ->
            case result of
                Ok chipDefinition ->
                    let
                        maybeVariant = get 0 chipDefinition.variants
                        maybePinout = get 0 chipDefinition.pinouts
                        maybeDevice = get 0 chipDefinition.devices
                    in
                        case maybeVariant of
                            Nothing -> ( InvalidState session, Cmd.none )
                            Just variant ->
                                case maybePinout of
                                    Nothing -> (InvalidState session, Cmd.none)
                                    Just pinout ->
                                        case maybeDevice of
                                            Nothing -> (InvalidState session, Cmd.none)
                                            Just device ->
                                                ( Success session (State chipDefinition variant device pinout 0 0 [ IO, ANALOG, INTERFACE, TIMER, OTHER ] Nothing Nothing []), Cmd.none )

                Err error ->
                    ( Failed session error, Cmd.none )

        ToggleVisibleCategory category ->
            case model of
                Success _ state ->
                    ( Success session { state | visibleModules = toggleVisible state.visibleModules category }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        HighlightModule category ->
            case model of
                Success _ state ->
                    ( Success session { state | highlightModule = Just category }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClearHighlight ->
            case model of
                Success _ state ->
                    ( Success session { state | highlightModule = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectSignal pin ->
            case model of
                Success _ state ->
                    ( Success session { state | selectedSignal = pin } , Cmd.none)
                _ ->
                    ( model, Cmd.none)

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
            [ viewGif model
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


getSignalsFromDevice : State -> ChipDevice -> List Signal
getSignalsFromDevice state device =
    map .instances (filter (\m -> member m.group state.visibleModules) device.modules)
        |> concat
        |> map .signals
        |> filterMap identity
        |> concat


padSignals : Pad -> List Signal -> List Signal
padSignals pad signals =
    filter (\p -> p.pad == pad) signals


isAlternativeSignal : Signal -> Bool
isAlternativeSignal signal =
    endsWith "_ALT" signal.function


signalModule : ChipDevice -> Signal -> Maybe Module
signalModule device signal =
    case filter (\d -> member signal (concat <| map (\i -> withDefault [] i.signals) d.instances)) device.modules of
        [ m ] ->
            Just m.name

        _ ->
            Nothing



-- |> filter (\i -> member signal i.signals)
--let
--    modules = filter (\d -> d.instances) device.modules


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


highlightSignal : State -> Signal -> Bool
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

selectSignalClass : State -> Signal -> Maybe String
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


viewPinSignal : State -> Signal -> Html Msg
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


viewPin : State -> List Signal -> ChipPin -> Html Msg
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


cls2 : List ( Bool, String ) -> String
cls2 pairs =
    filter first pairs
        |> map second
        |> join " "

cls : List (Maybe String) -> String
cls xs =
    filterMap identity xs
        |> join " "

viewModuleSelectCheckbox : State -> DeviceModuleCategory -> Html Msg
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


viewModuleSelect : State -> Html Msg
viewModuleSelect state =
    div [ class "module-select" ] <| map (viewModuleSelectCheckbox state) DeviceModuleCategory.list


viewChip : State -> List Signal -> Html Msg
viewChip state signals =
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

viewBitfield : Bitfield -> Html Msg
viewBitfield bitfield =
    div [] <| [h4 [] [text bitfield.caption]]
    ++
    render defaultHtmlRenderer (Maybe.withDefault "" bitfield.description)

viewRegister : Register -> Html Msg
viewRegister register =
    div [] <|
        [ h3 [] [text register.name]]
        ++ render defaultHtmlRenderer (Maybe.withDefault "" register.description)
        ++ case register.bitfields of
            Nothing ->
                [text "No bitfields"]
            Just bitfields ->
                map viewBitfield bitfields



viewRegisterGroup : Bool -> RegisterGroup -> Html Msg
viewRegisterGroup caption registerGroup =
    let
        registers = map viewRegister registerGroup.registers
    in
    case caption of
        True ->
            div [] <| [ div [] [ text registerGroup.caption]] ++ registers
        False ->
            div [] <| registers

-- note on signal logic
-- group by "function"
-- name by "group" + index (if contains more than 1!)
viewModule : State -> Html Msg
viewModule state =
    case state.selectedSignal of
        Nothing ->
            text ""
        Just signal ->
            let
                chipModules = filter (\m -> m.name == signal.deviceModule) state.chipDefinition.modules
            in
            case chipModules of
                chipModule::_ ->
                    div [] <|
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
                                 map (viewRegisterGroup captionGroup) registerGroups
                             Nothing ->
                                 [ text "No registers"]

                [] ->
                    div [] [text <| "No matching ChipModule found for DeviceModule " ++ Module.toString signal.deviceModule]

viewChipSelect : State -> Html Msg
viewChipSelect state =
    div []
        [ a [href "#", onClick <| RequestDefinition "ATtiny202"] [ text "ATtiny202" ]
        , text " | "
        , a [href "#", onClick <| RequestDefinition "ATtiny814"] [ text "ATtiny814" ]
        ]

test =
    let
        foo = defaultHtmlRenderer
    in
    0

render : Renderer (Html Msg) -> String -> List (Html Msg)
render renderer markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)
        |> Result.withDefault [text "Markdown render failed"]

deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"


viewGif : Model -> Html Msg
viewGif model =
    case model of
        Test1 _ ->
            div []
                [ text "Initial state"
                , a [ href "#fok" ] [ text "link" ]
                ]

        Test2 _ maybeHash ->
            case maybeHash of
                Just hash ->
                    div [] [ text "Navigated to", text hash ]

                Nothing ->
                    div [] [ text "Navigated to no hash" ]

        Failed _ error ->
            div []
                [ text "I could not load a random cat for some reason:",
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
                , button [ onClick <| RequestDefinition "ATtiny202" ] [ text "Try Again!" ]
                ]

        Loading _ ->
            text "Loading..."

        Success _ state ->
            div []
                [ div []
                    [ viewChipSelect state
                    , viewChip state  (sortSignals (map .pad state.pinout.pins) (getSignalsFromDevice state state.device))
                    , viewModule state
                    -- viewChip pinout (getSignalsFromDevice device))
                    ]
                ]
        InvalidState _ ->
            div [] [text "Woopsie doopsie, invalid state"]


getDefinition : String -> String -> Cmd Msg
getDefinition root definitionId =
    Http.get
        { url = root ++ "data/" ++ definitionId ++ ".json"
        , expect = Http.expectJson ReceiveDefinition chipDefinitionDecoder
        }
