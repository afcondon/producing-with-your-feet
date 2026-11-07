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
import Data.Twister.Holding (TwisterHolding(..), holdingSentence, holdingWord)

type Input =
  { view :: View
  , cardOrder :: Array PedalId
  , hiddenPedals :: Array PedalId
  , boardsActivePedal :: Maybe PedalId
  , overviewActivePedal :: Maybe PedalId
  , registry :: PedalRegistry
  -- | The two marks at the right of the nav: the MC6, connected or not, and
  -- | who has the Twister. Both derived by the app from what it already keeps.
  , mc6Connected :: Boolean
  , twister :: TwisterHolding
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
        [ viewButton "Overview" OverviewView
        , viewButton "Presets" BoardsView
        , viewButton "MC6" ControlsView
        , viewButton "MIDI" ConnectView
        , viewButton "\x21C5" FilesView
        ]
    , HH.div [ HP.class_ (H.ClassName "pedal-pills") ]
        (Array.mapMaybe renderPill state.cardOrder)
    , HH.div [ HP.class_ (H.ClassName "status-marks") ]
        [ renderMC6Mark state.mc6Connected
        , renderTwisterMark state.twister
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
    BoardsView, BoardsView -> true
    ControlsView, ControlsView -> true
    FilesView, FilesView -> true
    ConnectView, ConnectView -> true
    OverviewView, OverviewView -> true
    DetailView _, DetailView _ -> true
    PedalView _, PedalView _ -> true
    _, _ -> false

  isActivePedal pid = case state.view of
    DetailView activePid -> activePid == pid
    PedalView activePid -> activePid == pid
    BoardsView -> state.boardsActivePedal == Just pid
    OverviewView -> state.overviewActivePedal == Just pid
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

-- | A word and a sign, nothing to press: the MC6 decides who has the Twister,
-- | so a mark that could be clicked would promise something it cannot do.
renderMC6Mark :: forall w i. Boolean -> HH.HTML w i
renderMC6Mark connected =
  statusMark
    { modifier: if connected then "ok" else "absent"
    , sign: if connected then "\x2713" else "\x2717"
    , word: "MC6"
    , title: if connected then "MC6: connected" else "MC6: not connected"
    }

renderTwisterMark :: forall w i. TwisterHolding -> HH.HTML w i
renderTwisterMark holding =
  statusMark
    { modifier: case holding of
        TwisterNotConnected -> "absent"
        TwisterWithPedals -> "ok"
        TwisterWithLooper -> "looper"
    , sign: case holding of
        TwisterNotConnected -> "\x2717"
        TwisterWithPedals -> "\x2713"
        TwisterWithLooper -> "\x25CF"
    , word: "Twister" <> case holding of
        TwisterWithLooper -> ": " <> holdingWord holding
        _ -> ""
    , title: holdingSentence holding
    }

statusMark
  :: forall w i
   . { modifier :: String, sign :: String, word :: String, title :: String }
  -> HH.HTML w i
statusMark mark =
  HH.span
    [ HP.class_ (H.ClassName ("status-mark " <> mark.modifier))
    , HP.title mark.title
    ]
    [ HH.span [ HP.class_ (H.ClassName "status-mark-sign") ] [ HH.text mark.sign ]
    , HH.text mark.word
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.put input
  ClickView view -> H.raise (ViewChanged view)
  ClickPedal pid -> H.raise (PedalPillClicked pid)
