module Component.App (component) where

import Prelude

import Component.Boards.View as BoardsView
import Component.Detail.View as DetailView
import Component.Grid.View as GridView
import Component.Header as Header
import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, makeChannel, unCC, unMidiValue)
import Data.Pedal (PedalId)
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Data.Twister (SideBtn(..), TwisterEncoder(..), TwisterMsg(..), parseTwisterMsg)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Engine (AppState, View(..), initAppState)
import Engine.Storage (loadEngineState, loadCardOrderParsed)
import Engine.Twister as Twister
import Foreign.WebMIDI as MIDI
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Pedals.Registry as Registry
import Type.Proxy (Proxy(..))
import Web.DOM.Element as Element
import Web.HTML (window)
import Web.HTML.HTMLDocument (body) as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

data Action
  = Initialize
  | SetView View
  | SetValue PedalId CC MidiValue
  | SendMomentary PedalId CC MidiValue
  | SetInfo PedalId String Int
  | SelectPedalOutput String
  | SelectTwisterInput String
  | TwisterMidiReceived (Array Int)
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
    -- MIDI initialization
    mAccess <- H.liftAff MIDI.requestMIDIAccess
    outputs <- liftEffect $ MIDI.getOutputs mAccess
    inputs <- liftEffect $ MIDI.getInputs mAccess
    H.modify_ _ { connections { access = Just mAccess
                               , availableOutputs = outputs
                               , availableInputs = inputs } }
    -- Auto-select Twister input
    let mTwisterIn = Array.find (\p -> contains (Pattern "Midi Fighter Twister") p.name) inputs
    for_ mTwisterIn \port ->
      handleAction (SelectTwisterInput port.id)
    -- Auto-select Twister output
    let mTwisterOut = Array.find (\p -> contains (Pattern "Midi Fighter Twister") p.name) outputs
    for_ mTwisterOut \port -> do
      mOut <- liftEffect $ MIDI.openOutput mAccess port.id
      H.modify_ _ { connections { twisterOutput = mOut, twisterOutputId = Just port.id } }

  SetView view -> do
    H.modify_ _ { view = view }
    case view of
      DetailView pid -> do
        H.modify_ _ { focusPedalId = Just pid }
        sendAllLEDs pid
      _ -> pure unit
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
    -- LED feedback for UI-originated changes
    unless st.suppressTwister do
      for_ st.focusPedalId \focusPid ->
        when (focusPid == pid) do
          case Registry.findPedal pid of
            Nothing -> pure unit
            Just def -> case def.twister of
              Nothing -> pure unit
              Just tw -> do
                let mIdx = Array.findIndex (case _ of
                      Just (TwisterCC { cc: ecc }) -> ecc == ccNum
                      _ -> false) tw.encoders
                for_ mIdx \idx ->
                  sendRingPosition idx
                    (Twister.ringValueForEncoder (Array.index tw.encoders idx >>= identity) (unMidiValue val))

  SendMomentary pid ccNum val ->
    handleAction (SetValue pid ccNum val)

  SetInfo pid key val ->
    H.modify_ \st -> st
      { engine = Map.update
          (\ps -> Just ps { info = Map.insert key val ps.info })
          pid
          st.engine
      }

  SelectPedalOutput portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mOut <- liftEffect $ MIDI.openOutput access portId
        H.modify_ _ { connections { pedalOutput = mOut, pedalOutputId = Just portId } }

  SelectTwisterInput portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mInput <- liftEffect $ MIDI.openInput access portId
        case mInput of
          Nothing -> pure unit
          Just input -> do
            H.modify_ _ { connections { twisterInput = Just input, twisterInputId = Just portId } }
            void $ H.subscribe $ HS.makeEmitter \emit ->
              MIDI.onMessage input \bytes ->
                emit (TwisterMidiReceived bytes)

  TwisterMidiReceived bytes ->
    case parseTwisterMsg bytes of
      Nothing -> pure unit
      Just msg -> case msg of
        EncoderTurn idx val -> handleEncoderTurn idx val
        EncoderPress idx -> handleEncoderPress idx
        EncoderRelease _ -> pure unit
        SideButton btn -> handleTwisterSideButton btn

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
    GridView.PedalFocused pid -> do
      H.modify_ _ { focusPedalId = Just pid }
      sendAllLEDs pid
    GridView.OrderChanged order -> H.modify_ _ { cardOrder = order }
    GridView.ValueChanged pid cc val -> handleAction (SetValue pid cc val)
    GridView.MomentarySent pid cc val -> handleAction (SendMomentary pid cc val)
    GridView.InfoChanged pid key val -> handleAction (SetInfo pid key val)

  HandleBoards _output -> pure unit

-- Twister message handlers

handleEncoderTurn :: forall o m. MonadAff m => Int -> Int -> H.HalogenM AppState Action Slots o m Unit
handleEncoderTurn idx val = do
  st <- H.get
  case st.focusPedalId of
    Nothing -> pure unit
    Just pid -> case Registry.findPedal pid of
      Nothing -> pure unit
      Just def -> case Map.lookup pid st.engine of
        Nothing -> pure unit
        Just ps -> case Twister.handleEncoder idx val def ps of
          Nothing -> pure unit
          Just result -> do
            H.modify_ _ { suppressTwister = true }
            handleAction (SetValue pid result.cc result.value)
            H.modify_ _ { suppressTwister = false }
            for_ result.ringSnap \snap ->
              sendRingPosition idx snap

handleEncoderPress :: forall o m. MonadAff m => Int -> H.HalogenM AppState Action Slots o m Unit
handleEncoderPress idx = do
  st <- H.get
  case st.focusPedalId of
    Nothing -> pure unit
    Just pid -> case Registry.findPedal pid of
      Nothing -> pure unit
      Just def -> case Map.lookup pid st.engine of
        Nothing -> pure unit
        Just ps -> case Twister.handleButton idx def ps of
          Nothing -> pure unit
          Just changes -> do
            H.modify_ _ { suppressTwister = true }
            for_ changes \change ->
              handleAction (SetValue pid change.cc change.value)
            H.modify_ _ { suppressTwister = false }
            sendAllLEDs pid

handleTwisterSideButton :: forall o m. MonadAff m => SideBtn -> H.HalogenM AppState Action Slots o m Unit
handleTwisterSideButton btn = do
  st <- H.get
  case btn of
    RefreshLEDs ->
      for_ st.focusPedalId sendAllLEDs
    PrevPedal -> do
      let newFocus = Twister.handleSideButtonPrev st.focusPedalId st.cardOrder
      H.modify_ _ { focusPedalId = newFocus }
      case newFocus of
        Nothing -> dimAllLEDs
        Just pid -> sendAllLEDs pid
    NextPedal -> do
      let newFocus = Twister.handleSideButton st.focusPedalId st.cardOrder
      H.modify_ _ { focusPedalId = newFocus }
      case newFocus of
        Nothing -> dimAllLEDs
        Just pid -> sendAllLEDs pid

-- LED feedback helpers

sendRingPosition :: forall o m. MonadAff m => Int -> Int -> H.HalogenM AppState Action Slots o m Unit
sendRingPosition idx val = do
  st <- H.get
  for_ st.connections.twisterOutput \out ->
    liftEffect $ MIDI.send out [ 0xB0, idx, val ]

sendRGBColor :: forall o m. MonadAff m => Int -> Int -> H.HalogenM AppState Action Slots o m Unit
sendRGBColor idx hue = do
  st <- H.get
  for_ st.connections.twisterOutput \out ->
    liftEffect $ MIDI.send out [ 0xB1, idx, hue ]

sendAllLEDs :: forall o m. MonadAff m => PedalId -> H.HalogenM AppState Action Slots o m Unit
sendAllLEDs pid = do
  st <- H.get
  case Registry.findPedal pid of
    Nothing -> pure unit
    Just def -> case Map.lookup pid st.engine of
      Nothing -> pure unit
      Just ps -> do
        let leds = Twister.computeAllLEDs def ps
        for_ leds \led -> do
          sendRGBColor led.index led.hue
          sendRingPosition led.index led.ring

dimAllLEDs :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
dimAllLEDs = for_ (Array.range 0 15) \i -> do
  sendRGBColor i 0
  sendRingPosition i 0
