module Component.App (component) where

import Prelude

import Component.Boards.View as BoardsView
import Component.Detail.View as DetailView
import Component.Grid.View as GridView
import Component.Header as Header
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, makeChannel, unCC, unMidiValue)
import Effect.Console as Console
import Data.Pedal (PedalId)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Engine (AppState, View(..), initAppState)
import Engine.Storage (loadEngineState, loadCardOrderParsed)
import Foreign.WebMIDI as MIDI
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (body) as HTMLDocument
import Web.DOM.Element as Element
import Web.HTML.HTMLElement as HTMLElement

data Action
  = Initialize
  | SetView View
  | SetValue PedalId CC MidiValue
  | SendMomentary PedalId CC MidiValue
  | SetInfo PedalId String Int
  | SelectPedalOutput String
  | SelectTwisterInput String
  | HandleHeader Header.Output
  | HandleDetail DetailView.Output
  | HandleGrid GridView.Output
  | HandleBoards BoardsView.Output

type Slots =
  ( header :: Header.Slot Unit
  , grid :: GridView.Slot Unit
  , detail :: DetailView.Slot Unit
  , boards :: BoardsView.Slot Unit
  )

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const initAppState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. MonadAff m => AppState -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ (H.ClassName "app") ]
    [ HH.slot (Proxy :: _ "header") unit Header.component
        { view: state.view
        , connections: state.connections
        }
        HandleHeader
    , case state.view of
        GridView ->
          HH.slot (Proxy :: _ "grid") unit GridView.component
            { engine: state.engine
            , cardOrder: state.cardOrder
            }
            HandleGrid
        DetailView pid ->
          HH.slot (Proxy :: _ "detail") unit DetailView.component
            { engine: state.engine
            , pedalId: pid
            , cardOrder: state.cardOrder
            }
            HandleDetail
        BoardsView ->
          HH.slot (Proxy :: _ "boards") unit BoardsView.component
            { engine: state.engine
            , connections: state.connections
            }
            HandleBoards
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM AppState Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    mEngine <- liftEffect loadEngineState
    case mEngine of
      Just eng -> H.modify_ _ { engine = eng }
      Nothing -> pure unit
    cardOrder <- liftEffect loadCardOrderParsed
    H.modify_ _ { cardOrder = cardOrder }
    st <- H.get
    liftEffect do
      w <- window
      d <- document w
      mb <- HTMLDocument.body d
      for_ mb \b -> case st.view of
        GridView -> Element.setClassName "grid-mode" (HTMLElement.toElement b)
        _ -> pure unit

  SetView view -> do
    H.modify_ _ { view = view }
    liftEffect do
      w <- window
      d <- document w
      mb <- HTMLDocument.body d
      for_ mb \b -> case view of
        GridView -> Element.setClassName "grid-mode" (HTMLElement.toElement b)
        _ -> Element.setClassName "" (HTMLElement.toElement b)

  SetValue pid ccNum val -> do
    liftEffect $ Console.log $ "MIDI CC: pedal=" <> show pid <> " cc=" <> show (unCC ccNum) <> " val=" <> show (unMidiValue val)
    H.modify_ \st -> st
      { engine = Map.update
          (\ps -> Just ps { values = Map.insert ccNum val ps.values })
          pid
          st.engine
      }
    st <- H.get
    case st.connections.pedalOutput of
      Nothing -> pure unit
      Just output -> do
        let mCh = do
              ps <- Map.lookup pid st.engine
              makeChannel ps.channel
        case mCh of
          Nothing -> pure unit
          Just ch -> liftEffect $ MIDI.sendCC output ch ccNum val

  SendMomentary pid ccNum val ->
    handleAction (SetValue pid ccNum val)

  SetInfo pid key val ->
    H.modify_ \st -> st
      { engine = Map.update
          (\ps -> Just ps { info = Map.insert key val ps.info })
          pid
          st.engine
      }

  SelectPedalOutput _portId -> pure unit

  SelectTwisterInput _portId -> pure unit

  HandleHeader output -> case output of
    Header.ViewChanged view -> handleAction (SetView view)
    Header.PedalOutputChanged portId -> handleAction (SelectPedalOutput portId)
    Header.TwisterInputChanged portId -> handleAction (SelectTwisterInput portId)

  HandleDetail output -> case output of
    DetailView.ValueChanged pid ccNum val -> handleAction (SetValue pid ccNum val)
    DetailView.MomentarySent pid ccNum val -> handleAction (SendMomentary pid ccNum val)
    DetailView.PedalSelected pid -> handleAction (SetView (DetailView pid))
    DetailView.InfoChanged pid key val -> handleAction (SetInfo pid key val)

  HandleGrid output -> case output of
    GridView.PedalClicked pid -> handleAction (SetView (DetailView pid))
    GridView.OrderChanged order -> H.modify_ _ { cardOrder = order }
    GridView.ValueChanged pid cc val -> handleAction (SetValue pid cc val)
    GridView.MomentarySent pid cc val -> handleAction (SendMomentary pid cc val)
    GridView.InfoChanged pid key val -> handleAction (SetInfo pid key val)

  HandleBoards _output -> pure unit
