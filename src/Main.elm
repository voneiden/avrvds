module Main exposing (..)

--import Html.Attributes exposing (..)

import Array exposing (Array, get)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Data.Chip exposing (ChipDefinition, ChipDevice, ChipDeviceModule, ChipPin, ChipPinout, ChipVariant, Signal, chipDefinitionDecoder)
import Data.ChipTypes exposing (DeviceModuleCategory(..), Module, Pad(..), PinoutType(..))
import Data.Util.DeviceModuleCategory as DeviceModuleCategory
import Data.Util.Pad as Pad
import Debug exposing (toString)
import Dict exposing (Dict, keys)
import Dict.Extra exposing (groupBy)
import Html exposing (Html, a, button, div, input, label, text)
import Html.Attributes exposing (checked, class, href, id, type_)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import List exposing (append, concat, drop, filter, filterMap, length, map, member, reverse, sortBy, take)
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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    --(Loading, getDefinition)
    ( Loading <| Session key, getDefinition "ATtiny814" )


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
            ( Loading session, getDefinition definitionId )

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
                                                ( Success session (State chipDefinition variant device pinout 0 0 [ IO, ANALOG, INTERFACE, TIMER, OTHER ] Nothing []), Cmd.none )

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
        |> filter (\s -> s.group /= "PIN")


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
                    let
                        tmp =
                            Debug.log ("Fitted" ++ toString x) (length x)
                    in
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
                        let
                            tmp =
                                Debug.log ("FORCE FITTING REMAINING" ++ toString signalGroups_) (length signalGroups_)
                        in
                        concat signalGroups_

                    else
                        fitSignalGroups signalGroups_ initPads initPads

                Just ( remainingPads, fittedSignals ) ->
                    fittedSignals ++ fitSignalGroups (filter ((/=) fittedSignals) signalGroups) initPads remainingPads


filterPortPads : List Pad -> List Pad
filterPortPads pads =
    let
        isPort : Pad -> Bool
        isPort pad =
            case pad of
                VDD ->
                    False

                GND ->
                    False

                _ ->
                    True
    in
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
    replace "_" "-" result


highlightSignal : State -> Signal -> Bool
highlightSignal state signal =
    case state.highlightModule of
        Just c ->
            signal.deviceModule == c

        Nothing ->
            False


viewPinSignal : State -> Signal -> Html Msg
viewPinSignal state signal =
    div
        [ class <| cls [ ( True, "pin-signal" ), ( True, signal.function ), ( highlightSignal state signal, "highlight" ) ]
        , onMouseEnter <| HighlightModule signal.deviceModule
        , onMouseLeave ClearHighlight
        ]
        [ div [ class "pin-label" ]
            [ text <| signalToString signal
            ]
        ]


viewPin : State -> List Signal -> ChipPin -> Html Msg
viewPin state signals pin =
    div [ class "pin" ] <|
        append
            [ div [ class "pin-leg" ] [ div [ class "pin-label" ] [ text <| fromInt pin.position ] ]
            , div [ class "pin-pad" ] [ div [ class "pin-label" ] [ text <| Pad.toString pin.pad ] ]
            ]
        <|
            map (viewPinSignal state) (padSignals pin.pad signals)


cls : List ( Bool, String ) -> String
cls pairs =
    filter first pairs
        |> map second
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
            div [ id "chip-container" ]
                [ viewModuleSelect state
                , div [ id "chip-view", class "soic" ]
                    [ div [ class "soic-left" ] <| map (viewPin state leftSignals) (soicLeftPins state.pinout.pins)
                    , div [ class "soic-middle" ]
                        [ div [ class "pin1-marker" ] []
                        , div [ class "module-name" ] [ text state.device.name ]
                        ]
                    , div [ class "soic-right" ] <| map (viewPin state rightSignals) (soicRightPins state.pinout.pins)
                    ]
                ]



-- note on signal logic
-- group by "function"
-- name by "group" + index (if contains more than 1!)


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
                [ text "I could not load a random cat for some reason:"
                , text (toString error)
                , button [ onClick <| RequestDefinition "ATtiny814" ] [ text "Try Again!" ]
                ]

        Loading _ ->
            text "Loading..."

        Success _ state ->
            div []
                [ div []
                    [ viewChip state  (sortSignals (map .pad state.pinout.pins) (getSignalsFromDevice state state.device))

                    -- viewChip pinout (getSignalsFromDevice device))
                    ]
                ]
        InvalidState _ ->
            div [] [text "Woopsie doopsie, invalid state"]


getDefinition : String -> Cmd Msg
getDefinition definitionId =
    Http.get
        { url = "/data/" ++ definitionId ++ ".json"
        , expect = Http.expectJson ReceiveDefinition chipDefinitionDecoder
        }
