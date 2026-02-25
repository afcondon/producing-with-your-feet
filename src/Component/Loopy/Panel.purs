module Component.Loopy.Panel
  ( component
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Loopy as Loopy
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Engine (MidiConnections)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input =
  { connections :: MidiConnections
  }

data Output
  = LoopSelected Int
  | ActionFired Loopy.LoopyAction

type State =
  { input :: Input
  , selectedLoop :: Int
  }

data Action
  = Receive Input
  | ClickLoop Int
  | ClickAction Loopy.LoopyAction

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i -> { input: i, selectedLoop: 0 }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "loopy-panel") ]
    [ HH.div [ HP.class_ (H.ClassName "loopy-header") ]
        [ HH.span [ HP.class_ (H.ClassName "loopy-title") ] [ HH.text "LoopyPro" ] ]
    , HH.div [ HP.class_ (H.ClassName "loopy-grid") ]
        (map renderGroup Loopy.groups)
    , HH.div [ HP.class_ (H.ClassName "loopy-actions") ]
        (map renderAction Loopy.actions)
    ]
  where
  renderGroup group =
    HH.div [ HP.class_ (H.ClassName "loopy-group") ]
      [ loopButton group.loopA group.color "A"
      , loopButton group.loopB group.color "B"
      , HH.span [ HP.class_ (H.ClassName "loopy-group-label") ]
          [ HH.text group.color.label ]
      ]

  loopButton (Loopy.LoopIndex idx) _color label =
    HH.button
      [ HP.class_ (H.ClassName (if state.selectedLoop == idx then "loopy-loop selected" else "loopy-loop"))
      , HE.onClick \_ -> ClickLoop idx
      ]
      [ HH.text label ]

  renderAction def =
    HH.button
      [ HP.class_ (H.ClassName "loopy-action-btn")
      , HE.onClick \_ -> ClickAction def.action
      ]
      [ HH.text def.label ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  ClickLoop idx -> do
    H.modify_ _ { selectedLoop = idx }
    H.raise (LoopSelected idx)
  ClickAction action -> H.raise (ActionFired action)
