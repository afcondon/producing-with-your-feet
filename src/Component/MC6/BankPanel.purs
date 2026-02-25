module Component.MC6.BankPanel
  ( component
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

type State = Input

data Action = Receive Input

component :: forall q o m. MonadAff m => H.Component q Input o m
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
  HH.div [ HP.class_ (H.ClassName "mc6-bank-panel") ]
    [ HH.h4_ [ HH.text "MC6 Bank Export" ]
    , HH.text "MC6 bank export (coming soon)"
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Receive input -> H.put input
