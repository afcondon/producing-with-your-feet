module Component.Boards.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, MidiConnections)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Input =
  { engine :: EngineState
  , connections :: MidiConnections
  }

data Output = NoOutput

type State = Input

data Action = Receive Input

type Slot = H.Slot (Const Void) Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div [ HP.class_ (H.ClassName "boards-view") ]
    [ HH.div [ HP.class_ (H.ClassName "loopy-panel") ]
        [ HH.h3_ [ HH.text "LoopyPro" ]
        , HH.text "Loop control panel (coming soon)"
        ]
    , HH.div [ HP.class_ (H.ClassName "board-builder") ]
        [ HH.h3_ [ HH.text "Board Presets" ]
        , HH.text "Board builder (coming soon)"
        ]
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.put input
