module Views.ChipView exposing (..)
-- MODEL
import Decoders.ChipDefinitionDecoder exposing (ChipDefinition)
type alias AppState =
    { chipDefinition : ChipDefinition
    , pinout : Int
    }

type Model
  = Failure
  | Loading
  | Success AppState
