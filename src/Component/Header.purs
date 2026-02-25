module Component.Header
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))

import Data.Pedal (PedalId(..))
import Effect.Aff.Class (class MonadAff)
import Engine (MidiConnections, View(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input =
  { view :: View
  , connections :: MidiConnections
  }

data Output
  = ViewChanged View
  | PedalOutputChanged String
  | TwisterInputChanged String

type State = Input

data Action
  = Receive Input
  | ClickView View
  | ChangePedalOutput String
  | ChangeTwisterInput String

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
render state =
  HH.header
    [ HP.class_ (H.ClassName "app-header") ]
    [ HH.div [ HP.class_ (H.ClassName "view-toggle") ]
        [ viewButton "Grid" GridView
        , viewButton "Detail" (DetailView (PedalId "onward"))
        , viewButton "Boards" BoardsView
        ]
    , HH.div [ HP.class_ (H.ClassName "midi-pickers") ]
        [ midiPicker "Pedal MIDI" state.connections.pedalOutputId state.connections.availableOutputs ChangePedalOutput
        , midiPicker "Twister" state.connections.twisterInputId state.connections.availableInputs ChangeTwisterInput
        ]
    ]
  where
  viewButton label view =
    HH.button
      [ HP.class_ (H.ClassName (if isActive view then "active" else ""))
      , HE.onClick \_ -> ClickView view
      ]
      [ HH.text label ]

  isActive = case _, state.view of
    GridView, GridView -> true
    BoardsView, BoardsView -> true
    DetailView _, DetailView _ -> true
    _, _ -> false

  midiPicker label selectedId ports onChange =
    HH.div [ HP.class_ (H.ClassName "midi-picker") ]
      [ HH.label_ [ HH.text label ]
      , HH.select
          [ HE.onValueChange onChange ]
          ( [ HH.option [ HP.value "" ] [ HH.text "No output" ] ]
            <> map portOption ports
          )
      ]
    where
    portOption port =
      HH.option
        [ HP.value port.id
        , HP.selected (selectedId == Just port.id)
        ]
        [ HH.text port.name ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.put input
  ClickView view -> H.raise (ViewChanged view)
  ChangePedalOutput portId -> H.raise (PedalOutputChanged portId)
  ChangeTwisterInput portId -> H.raise (TwisterInputChanged portId)
