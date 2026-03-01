module Component.Header
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Color (toHexString)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))

import Data.Pedal (PedalId)
import Effect.Aff.Class (class MonadAff)
import Engine (View(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry

type Input =
  { view :: View
  , cardOrder :: Array PedalId
  , hiddenPedals :: Array PedalId
  , boardsActivePedal :: Maybe PedalId
  , registry :: PedalRegistry
  }

data Output
  = ViewChanged View
  | PedalPillClicked PedalId

type State = Input

data Action
  = Receive Input
  | ClickView View
  | ClickPedal PedalId

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
        , viewButton "Boards" BoardsView
        , viewButton "Docs" DocsView
        , viewButton "MIDI" ConnectView
        , viewButton "\x21C5" FilesView
        ]
    , HH.div [ HP.class_ (H.ClassName "pedal-pills") ]
        (Array.mapMaybe renderPill state.cardOrder)
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
    FilesView, FilesView -> true
    DocsView, DocsView -> true
    ConnectView, ConnectView -> true
    DetailView _, DetailView _ -> true
    _, _ -> false

  isActivePedal pid = case state.view of
    DetailView activePid -> activePid == pid
    BoardsView -> state.boardsActivePedal == Just pid
    _ -> false

  renderPill pid = do
    def <- CRegistry.findPedal state.registry pid
    let colorStyle = case def.meta.color of
          Just c -> "background: " <> toHexString c
          Nothing -> ""
        isHidden = Array.elem pid state.hiddenPedals
        cls = "pedal-pill"
              <> (if isActivePedal pid then " active" else "")
              <> (if isHidden then " hidden" else "")
    pure $ HH.button
      [ HP.class_ (H.ClassName cls)
      , HP.attr (HH.AttrName "style") colorStyle
      , HE.onClick \_ -> ClickPedal pid
      ]
      [ HH.text def.meta.shortName ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.put input
  ClickView view -> H.raise (ViewChanged view)
  ClickPedal pid -> H.raise (PedalPillClicked pid)
