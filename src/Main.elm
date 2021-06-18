module Main exposing (..)

--import Html.Attributes exposing (..)
import Array exposing (Array, get)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Decoders.ChipDefinitionDecoder exposing (ChipDefinition, chipDefinitionDecoder)
import Html exposing (Html, a, button, div, h2, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Http
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
type alias AppState =
    { chipDefinition : ChipDefinition
    , pinout : Int
    }

type Model
  = Failure Session
  | Loading Session
  | Test1 Session
  | Test2 Session (Maybe String)
  | Success Session AppState


modelSession : Model -> Session
modelSession model =
    case model of
        Failure session -> session
        Loading session -> session
        Test1 session -> session
        Test2 session _ -> session
        Success session _ -> session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
      --(Loading, getDefinition)
      (Test1 (Session key), Cmd.none)

onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest = UrlRequested urlRequest

onUrlChange : Url.Url -> Msg
onUrlChange url = UrlChanged url

type Msg
  = UrlRequested UrlRequest
  | UrlChanged Url.Url
  | RequestDefinition
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
        (model, Cmd.none)

    RequestDefinition ->
      (Loading session, getDefinition)

    ReceiveDefinition result ->
      case result of
        Ok chipDefinition ->
          (Success session (AppState chipDefinition  0), Cmd.none)

        Err _ ->
          (Failure session, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Document Msg
view model =
    { title = "Hello world"
    , body = [
            div []
                [ h2 [] [ text "Random Cats" ]
                , viewGif model
                ]
        ]
    }



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

    Failure _ ->
      div []
        [ text "I could not load a random cat for some reason. "
        , button [ onClick RequestDefinition ] [ text "Try Again!" ]
        ]

    Loading _ ->
      text "Loading..."

    Success _ appState ->
      div []
        [ button [ onClick RequestDefinition, style "display" "block" ] [ text "More Please!" ]
        , div [] [
            case get appState.pinout appState.chipDefinition.pinouts of
                Nothing -> text "Nada"
                Just pinout -> text pinout.name
            ]
        ]


getDefinition : Cmd Msg
getDefinition =
  Http.get
    { url = "/data/ATtiny814.json"
    , expect = Http.expectJson ReceiveDefinition chipDefinitionDecoder
    }

