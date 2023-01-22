import Data.Chip exposing (Signal)
type FootprintState = FootprintNothing
                    | FootprintSignal Signal

type SelectedSignal = SelectedSignal Signal
type HoveredSignal = HoveredSignal Signal
