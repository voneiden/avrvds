module Main exposing (..)

--import Html.Attributes exposing (..)
import Array exposing (Array, get)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Debug exposing (toString)
import Decoders.ChipDefinitionDecoder exposing (ChipDefinition, ChipPin, ChipPinout, PinoutType(..), chipDefinitionDecoder)
import Html exposing (Html, a, button, div, h2, text)
import Html.Attributes exposing (class, href, id, style)
import Html.Events exposing (onClick)
import Http
import List exposing (concat, drop, length, map, take)
import Maybe exposing (map2)
import String exposing (fromInt)
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
                [ h2 [] [ text "Random Cats" ]
                , viewGif model
                ]
        ]
    }


soicTopPins : List ChipPin -> List ChipPin
soicTopPins pins =
    let count = length pins // 2 in
        drop count pins |> take count

soicBottomPins : List ChipPin -> List ChipPin
soicBottomPins pins =
    take ((length pins) // 2) pins

viewPin : ChipPin -> Html Msg
viewPin pin =
    div [ class "soic-pin"] [
     div [ class "soic-pin-leg"] [ div [class "soic-pin-label"] [text <| fromInt pin.position ]],
     div [ class "soic-pin-pad"] [ div [class "soic-pin-label"] [text pin.pad ]]
     ]

viewChip : ChipPinout -> Html Msg
viewChip pinout =
    case pinout.pinoutType of
        SOIC ->
            div [ id "chip-view", class "soic"] [
                div [ class "soic-top"] <| map viewPin (soicTopPins pinout.pins),
                div [ class "soic-middle"] [],
                div [ class "soic-bottom"] <| map viewPin (soicBottomPins pinout.pins)
            ]

-- note on signal logic
-- group by "function"
-- name by "group" + index (if contains more than 1!)

-- todo some kind of placement prioritizer function for signals. should be fun

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
      div []
        [ button [ onClick <| RequestDefinition "ATtiny814", style "display" "block" ] [ text "More Please!" ]
        , div [] [
            case get appState.pinout appState.chipDefinition.pinouts of
                Nothing -> text "Nada"
                Just pinout -> viewChip pinout
            ]
        ]


getDefinition : String -> Cmd Msg
getDefinition definitionId =
  Http.get
    { url = "/data/" ++ definitionId ++ ".json"
    , expect = Http.expectJson ReceiveDefinition chipDefinitionDecoder
    }

