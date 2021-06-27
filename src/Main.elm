module Main exposing (..)

--import Html.Attributes exposing (..)
import Array exposing (Array, get)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Debug exposing (toString)
import Decoders.ChipDefinitionDecoder exposing (ChipDefinition, ChipDevice, ChipDeviceModule, ChipPin, ChipPinout, DeviceModuleCategory(..), Pad(..), PinoutType(..), Signal, chipDefinitionDecoder, padToString)
import Dict exposing (Dict, keys)
import Dict.Extra exposing (groupBy)
import Html exposing (Html, a, button, div, h2, text)
import Html.Attributes exposing (class, href, id, style)
import Html.Events exposing (onClick)
import Http
import List exposing (append, concat, drop, filter, filterMap, length, map, member, reverse, sort, sortBy, take)
import Maybe exposing (map2, withDefault)
import String exposing (endsWith, fromInt, replace)
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
    { key: Nav.Key
    }

-- MODEL
type alias State =
    { chipDefinition : ChipDefinition
    , pinout: Int
    , view : Int
    , pin : Int
    , variant : Int
    }

type Model
  = Failed Session Http.Error
  | Loading Session
  | Success Session State
  | Test1 Session
  | Test2 Session (Maybe String)



modelSession : Model -> Session
modelSession model =
    case model of
        Failed session _ -> session
        Loading session -> session
        Test1 session -> session
        Test2 session _ -> session
        Success session _ -> session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
      --(Loading, getDefinition)
      (Loading <| Session key, getDefinition "ATtiny814")

onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest = UrlRequested urlRequest

onUrlChange : Url.Url -> Msg
onUrlChange url = UrlChanged url

type Msg
  = UrlRequested UrlRequest
  | UrlChanged Url.Url
  | RequestDefinition String
  | ReceiveDefinition (Result Http.Error ChipDefinition)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    session = modelSession model
  in
  case msg of
    UrlRequested urlRequest ->
        case urlRequest of
            Browser.Internal url ->
                (Test2 session url.fragment, Nav.pushUrl session.key (Url.toString url) )
                --(Test2 session url.fragment, Cmd.none)
            Browser.External url ->
                (model, Nav.load(url))

    UrlChanged url->
        case model of
            Test2 _ _ ->
                (Test2 session (map2 (++) (Just "NAV!") url.fragment), Cmd.none )
            _ ->
                (model, Cmd.none)

    RequestDefinition definitionId ->
      (Loading session, getDefinition definitionId)

    ReceiveDefinition result ->
      case result of
        Ok chipDefinition ->
          (Success session (State chipDefinition 0 0 0 0), Cmd.none)

        Err error ->
          (Failed session error, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Document Msg
view model =
    { title = "AVR Visual Datasheet"
    , body = [
            div []
                [ viewGif model
                ]
        ]
    }


soicRightPins : List ChipPin -> List ChipPin
soicRightPins pins =
    let count = length pins // 2 in
        drop count pins |> take count |> reverse

soicLeftPins : List ChipPin -> List ChipPin
soicLeftPins pins =
    take ((length pins) // 2) pins


getSignalsFromDefinition : ChipDefinition -> Maybe (List Signal)
getSignalsFromDefinition definition =
    let
        moduleFilter : ChipDeviceModule -> Bool
        moduleFilter m =
            case m.group of
                PORT -> True
                ANALOG -> True
                INTERFACE -> True
                TIMER -> True
                OTHER -> True
                _ -> False
    in
        case get 0 definition.devices of
            Nothing -> Nothing
            Just device ->
                map .instances (filter moduleFilter device.modules)
                |> concat
                |> filter (\i -> i.name /= "EVSYS")
                |> map .signals
                |> filterMap identity
                |> concat
                |> filter (\s -> s.group /= "PIN")
                |> Just

padSignals : Pad -> List Signal -> List Signal
padSignals pad signals =
    filter (\p -> p.pad == pad) signals

isAlternativeSignal : Signal -> Bool
isAlternativeSignal signal =
    endsWith "_ALT" signal.function


type alias SignalFit =
    { pads : List Pad
    , signals : List Signal
    }

signalGroupFitsPads : List Signal -> List Pad -> List Pad -> Maybe (List Pad)
signalGroupFitsPads signals availablePads usedPads =
    case signals of
        [] -> Just usedPads
        x::xs ->
            if member x.pad availablePads then
                signalGroupFitsPads xs (filter ((/=) x.pad) availablePads) (usedPads ++ [x.pad])
            else
                Nothing

signalGroupsFitPads : List (List Signal) -> List Pad -> Maybe (List Pad, List Signal)
signalGroupsFitPads signalGroups availablePads =
    case signalGroups of
        [] -> Nothing
        x::xs ->
            case signalGroupFitsPads x availablePads [] of
                Nothing -> signalGroupsFitPads xs availablePads
                Just usedPads ->
                    let
                        tmp = Debug.log ("Fitted" ++ toString x) (length x)
                    in
                        Just (filter (\p -> not (member p usedPads)) availablePads, x)

fitSignalGroups : List (List Signal) -> List Pad -> List Pad -> List Signal
fitSignalGroups signalGroups initPads pads =
    case signalGroups of
        [] -> []
        signalGroups_ ->
            case signalGroupsFitPads signalGroups_ pads of
                Nothing ->
                    if initPads == pads then
                        let
                            tmp = Debug.log ("FORCE FITTING REMAINING" ++ toString signalGroups_) (length signalGroups_)
                        in
                            concat signalGroups_
                    else
                        fitSignalGroups signalGroups_ initPads initPads
                Just (remainingPads, fittedSignals) ->
                    fittedSignals ++ fitSignalGroups (filter ((/=) fittedSignals) signalGroups) initPads remainingPads

filterPortPads : List Pad -> List Pad
filterPortPads pads =
    let
        isPort : Pad -> Bool
        isPort pad =
            case pad of
                VDD -> False
                GND -> False
                _ ->  True
    in
        filter isPort pads

getDefault : comparable -> Dict comparable v -> v -> v
getDefault targetKey dict default =
    withDefault default <| Dict.get targetKey dict

sortSignals : List Pad -> List Signal -> List Signal
sortSignals pads signals =
    let

        signalsGroupedByFunction = groupBy .function signals
        functions = keys signalsGroupedByFunction
        sortedFunctions = reverse <| sortBy (\f -> length <| getDefault f signalsGroupedByFunction []) functions
        portPads = filterPortPads pads
    in
        fitSignalGroups (map (\f -> getDefault f signalsGroupedByFunction []) sortedFunctions) portPads portPads

signalToString : Signal -> String
signalToString signal =
    let
        prefix =
            case signal.group of
                "OUT" -> signal.function ++ "-" ++ signal.group
                _ -> signal.group
        result =
            case signal.index of
                    Nothing -> prefix
                    Just index -> prefix ++ fromInt index
    in
        replace "_" "-" result


viewPinSignal : Signal -> Html Msg
viewPinSignal signal =
    div [ class "pin-signal"] [
        div [ class "pin-label" ] [
            text <| signalToString signal
        ]
    ]

viewPin : List Signal -> ChipPin -> Html Msg
viewPin signals pin =
    div [ class "pin"]
        <| append
            [
             div [ class "pin-leg"] [ div [class "pin-label"] [text <| fromInt pin.position ]],
             div [ class "pin-pad"] [ div [class "pin-label"] [text <| padToString pin.pad ]]
            ]
            <| map viewPinSignal (padSignals pin.pad signals)


viewChip : ChipPinout -> List Signal -> Html Msg
viewChip pinout signals =
    let
        leftPins = soicLeftPins pinout.pins
        rightPins = soicRightPins pinout.pins

        leftPads = map .pad leftPins
        rightPads = map .pad rightPins

        leftSignals = sortSignals leftPads <| filter (\s -> member s.pad leftPads ) signals
        rightSignals = sortSignals rightPads <| filter (\s -> member s.pad rightPads ) signals
    in
        case pinout.pinoutType of
            SOIC ->
                div [ id "chip-view", class "soic"] [
                    div [ class "soic-left"] <| map (viewPin leftSignals) (soicLeftPins pinout.pins),
                    div [ class "soic-middle"] [
                        div [ class "pin1-marker"] []
                    ],
                    div [ class "soic-right"] <| map (viewPin rightSignals) (soicRightPins pinout.pins)
                ]

-- note on signal logic
-- group by "function"
-- name by "group" + index (if contains more than 1!)


viewGif : Model -> Html Msg
viewGif model =
  case model of
    Test1 _ ->
        div [] [
            text "Initial state",
            a [href "#fok"] [ text "link" ]
        ]
    Test2 _ maybeHash ->
        case maybeHash of
            Just hash ->
                div [] [ text "Navigated to", text hash]
            Nothing ->
                div [] [ text "Navigated to no hash"]

    Failed _ error ->
      div []
        [ text "I could not load a random cat for some reason:"
        , text (toString error)
        , button [ onClick <| RequestDefinition "ATtiny814" ] [ text "Try Again!" ]
        ]

    Loading _ ->
      text "Loading..."

    Success _ appState ->
      div [] [
        div [] [
            case get appState.pinout appState.chipDefinition.pinouts of
                Nothing -> text "Nada"
                Just pinout ->
                    case getSignalsFromDefinition appState.chipDefinition of
                        Nothing -> text "Nada device"
                        Just signals -> viewChip pinout (sortSignals (map .pad pinout.pins) signals)
                        --Just signals -> viewChip pinout signals
            ]
        ]


getDefinition : String -> Cmd Msg
getDefinition definitionId =
  Http.get
    { url = "/data/" ++ definitionId ++ ".json"
    , expect = Http.expectJson ReceiveDefinition chipDefinitionDecoder
    }

