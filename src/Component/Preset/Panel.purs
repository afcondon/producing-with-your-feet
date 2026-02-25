module Component.Preset.Panel
  ( component
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Data.Pedal (PedalId)
import Engine (EngineState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Input =
  { pedalId :: PedalId
  , engine :: EngineState
  }

data Output = PresetRecalled PedalId

type State = Input

data Action = Receive Input

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
  HH.div [ HP.class_ (H.ClassName "preset-panel") ]
    [ HH.h4_ [ HH.text "Presets" ]
    , HH.text "Preset management (coming soon)"
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.put input
