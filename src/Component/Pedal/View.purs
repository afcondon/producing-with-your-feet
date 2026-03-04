module Component.Pedal.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Component.Pedal.DonutTree as DonutTree
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Data.Const (Const)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeMidiValue)
import Data.Pedal (PedalId)
import Data.Pedal.Layout (PedalLayout)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, PedalState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME

type Input =
  { engine :: EngineState
  , pedalId :: PedalId
  , registry :: PedalRegistry
  }

data Output
  = BackToGrid
  | ValueChanged PedalId CC MidiValue

type DragContext = { cc :: CC, startY :: Int, startVal :: Int }

type State =
  { input :: Input
  , dragging :: Maybe DragContext
  , dragSub :: Maybe H.SubscriptionId
  , hatsListener :: Maybe (HS.Listener Action)
  , hatsSub :: Maybe H.SubscriptionId
  }

data Action
  = Initialize
  | Receive Input
  | ClickBack
  | HatsKnobDragStart CC Int  -- CC, current value (from pointerdown)
  | HatsSegmentClick CC Int
  | HatsToggleClick CC Int
  | DragMove Int               -- current clientY
  | DragEnd

type Slot = H.Slot (Const Void) Output

hatsContainerId :: String
hatsContainerId = "#pedal-donut-hats"

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i ->
        { input: i
        , dragging: Nothing
        , dragSub: Nothing
        , hatsListener: Nothing
        , hatsSub: Nothing
        }
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    pid = state.input.pedalId
    mDef = CRegistry.findPedal state.input.registry pid
    mPs = Map.lookup pid state.input.engine
  in
    HH.div [ HP.class_ (H.ClassName "pedal-view-container") ]
      [ case mDef, mPs of
          Just def, Just ps -> case def.layout of
            Just _ ->
              HH.div [ HP.class_ (H.ClassName "pedal-view") ]
                [ renderHeader def.meta.name def.meta.brand ps
                -- Empty container — HATS renders into this
                , HH.div [ HP.id "pedal-donut-hats" ] []
                , renderBackButton
                ]
            Nothing ->
              HH.div [ HP.class_ (H.ClassName "pedal-view") ]
                [ renderHeader def.meta.name def.meta.brand ps
                , HH.div [ HP.class_ (H.ClassName "pedal-view-fallback") ]
                    [ HH.text "Donut view not yet available for this pedal." ]
                , renderBackButton
                ]
          _, _ ->
            HH.div [ HP.class_ (H.ClassName "pedal-view") ]
              [ HH.text "Pedal not found"
              , renderBackButton
              ]
      ]

renderHeader :: forall w i. String -> String -> PedalState -> HH.HTML w i
renderHeader name brand ps =
  HH.div [ HP.class_ (H.ClassName "pedal-view-header") ]
    [ HH.h2_ [ HH.text name ]
    , HH.span [ HP.class_ (H.ClassName "brand") ] [ HH.text brand ]
    , HH.span [ HP.class_ (H.ClassName "channel") ] [ HH.text ("Ch " <> show ps.channel) ]
    ]

renderBackButton :: forall m. H.ComponentHTML Action () m
renderBackButton =
  HH.button
    [ HP.class_ (H.ClassName "pedal-view-back")
    , HE.onClick \_ -> ClickBack
    ]
    [ HH.text "Back to Grid" ]

-- | Build HATS callbacks that notify the Halogen listener
makeCallbacks :: HS.Listener Action -> DonutTree.PedalCallbacks
makeCallbacks listener =
  { onKnobDragStart: \cc val -> HS.notify listener (HatsKnobDragStart cc val)
  , onSegmentClick: \cc val -> HS.notify listener (HatsSegmentClick cc val)
  , onToggleClick: \cc val -> HS.notify listener (HatsToggleClick cc val)
  }

-- | Render the HATS tree into the container
renderHats :: PedalLayout -> PedalState -> DonutTree.PedalCallbacks -> Effect Unit
renderHats layout ps callbacks = do
  clearContainer hatsContainerId
  let tree = DonutTree.pedalTree layout ps callbacks
  _ <- rerender hatsContainerId tree
  pure unit

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    -- Set up the HATS event bridge
    { listener, emitter } <- H.liftEffect HS.create
    sid <- H.subscribe emitter
    H.modify_ _ { hatsListener = Just listener, hatsSub = Just sid }
    -- Initial render
    rerenderIfReady

  Receive input -> do
    H.modify_ _ { input = input }
    rerenderIfReady

  ClickBack -> H.raise BackToGrid

  HatsKnobDragStart cc val -> do
    -- Subscribe to document-level mouse events for drag tracking
    sid <- H.subscribe $ HS.makeEmitter \emit -> do
      moveFn <- eventListener \e ->
        case ME.fromEvent e of
          Just mouseEvt -> emit (DragMove (ME.clientY mouseEvt))
          Nothing -> pure unit
      upFn <- eventListener \_ -> emit DragEnd
      target <- Window.toEventTarget <$> window
      addEventListener (EventType "mousemove") moveFn false target
      addEventListener (EventType "mouseup") upFn false target
      pure do
        removeEventListener (EventType "mousemove") moveFn false target
        removeEventListener (EventType "mouseup") upFn false target
    -- We don't have clientY from the pointerdown event (HATS doesn't pass it),
    -- so use 0 as startY — the first DragMove will set the real baseline
    H.modify_ _ { dragging = Just { cc, startY: 0, startVal: val }, dragSub = Just sid }

  HatsSegmentClick cc val -> do
    st <- H.get
    H.raise (ValueChanged st.input.pedalId cc (unsafeMidiValue val))

  HatsToggleClick cc val -> do
    st <- H.get
    H.raise (ValueChanged st.input.pedalId cc (unsafeMidiValue (if val > 63 then 0 else 127)))

  DragMove clientY -> do
    st <- H.get
    case st.dragging of
      Just drag -> do
        if drag.startY == 0
          -- First move: set baseline, don't emit value change yet
          then H.modify_ _ { dragging = Just drag { startY = clientY } }
          else do
            let
              delta = drag.startY - clientY
              newVal = clamp 0 127 (drag.startVal + delta)
            H.raise (ValueChanged st.input.pedalId drag.cc (unsafeMidiValue newVal))
      Nothing -> pure unit

  DragEnd -> do
    st <- H.get
    case st.dragSub of
      Just sid -> do
        H.unsubscribe sid
        H.modify_ _ { dragging = Nothing, dragSub = Nothing }
      Nothing ->
        H.modify_ _ { dragging = Nothing }

-- | Re-render HATS tree if we have layout + state + listener
rerenderIfReady :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
rerenderIfReady = do
  st <- H.get
  let pid = st.input.pedalId
      mDef = CRegistry.findPedal st.input.registry pid
      mPs = Map.lookup pid st.input.engine
  case mDef, mPs, st.hatsListener of
    Just def, Just ps, Just listener -> case def.layout of
      Just layout -> H.liftEffect $ renderHats layout ps (makeCallbacks listener)
      Nothing -> pure unit
    _, _, _ -> pure unit
