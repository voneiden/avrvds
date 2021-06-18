module Views.HomeView exposing (..)
-- MODEL
import ChipDefinitionDecoder exposing (ChipDefinition)
type alias AppState =
    { chipDefinition : ChipDefinition
    , pinout : Int
    }

type Model
  = Failure
  | Loading
  | Success AppState
