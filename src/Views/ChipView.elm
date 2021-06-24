module Views.ChipView exposing (..)
-- MODEL
import Decoders.ChipDefinitionDecoder exposing (ChipDefinition)
import Main exposing (Session)


type alias State =
    { chipDefinition : ChipDefinition
    , pinout : Int
    }

type Model
  = Failure Session
  | Loading Session
  | Success Session State


view : Model -> Html Msg
view model =
       case model of
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
