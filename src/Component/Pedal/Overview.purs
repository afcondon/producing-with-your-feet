module Component.Pedal.Overview
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Color (toHexString)
import Component.Pedal.DonutTree as DonutTree
import Component.Pedal.HedraTree as HedraTree
import Component.Pedal.OverviewLayout as Layout
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Data.Array (length) as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeMidiValue)
import Data.Pedal (PedalDef, PedalId(..))
import Data.Pedal.Layout (PedalLayout)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, PedalState)
import Halogen as H
import Halogen.HTML as HH
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
  , registry :: PedalRegistry
  , cardOrder :: Array PedalId
  , activePedal :: Maybe PedalId
  }

data Output
  = BackToGrid
  | ValueChanged PedalId CC MidiValue

type DragContext =
  { cc :: CC, startY :: Int, startVal :: Int, pedalId :: PedalId
  , ccX :: Maybe CC, startX :: Int, startValX :: Int
  }

type State =
  { input :: Input
  , dragging :: Maybe DragContext
  , dragSub :: Maybe H.SubscriptionId
  , hatsListener :: Maybe (HS.Listener Action)
  , hatsSub :: Maybe H.SubscriptionId
  , containerW :: Number
  , containerH :: Number
  , resizeSub :: Maybe H.SubscriptionId
  }

data Action
  = Initialize
  | Receive Input
  | WindowResize
  | HatsKnobDragStart PedalId CC Int
  | HatsKnobDragStart2D PedalId CC Int CC Int
  | HatsSegmentClick PedalId CC Int
  | HatsToggleClick PedalId CC Int
  | DragMove Int Int
  | DragEnd

type Slot = H.Slot (Const Void) Output

thumbContainerId :: PedalId -> String
thumbContainerId (PedalId pid) = "#pedal-thumb-" <> pid

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i ->
        { input: i
        , dragging: Nothing
        , dragSub: Nothing
        , hatsListener: Nothing
        , hatsSub: Nothing
        , containerW: 1200.0
        , containerH: 700.0
        , resizeSub: Nothing
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
    n = Array.length state.input.cardOrder
    gridCss = case state.input.activePedal of
      Nothing -> Layout.gridStyle state.containerW state.containerH n
      Just _  -> Layout.activeGridStyle state.containerW state.containerH n
  in
    HH.div
      [ HP.class_ (H.ClassName "overview-treemap")
      , HP.attr (HH.AttrName "style") gridCss
      ]
      (map (renderCell state) state.input.cardOrder)

renderCell :: forall m. State -> PedalId -> H.ComponentHTML Action () m
renderCell state pid@(PedalId pidStr) =
  let
    mDef = CRegistry.findPedal state.input.registry pid
    isActive = state.input.activePedal == Just pid
    cls = "overview-cell" <> if isActive then " active" else ""
    colorBg = case mDef of
      Just def -> case def.meta.color of
        Just c -> "background:" <> toHexString c <> "10;"
        Nothing -> ""
      Nothing -> ""
    cellStyle = colorBg <> if isActive then Layout.activeCellStyle else ""
    cellProps =
      [ HP.class_ (H.ClassName cls)
      ] <> if cellStyle /= ""
             then [ HP.attr (HH.AttrName "style") cellStyle ]
             else []
  in
    HH.div cellProps
      [ case mDef of
          Just def ->
            if hasHatsView def then
              let labelStyle = case def.meta.color of
                    Just c -> "background:" <> toHexString c
                    Nothing -> "background:var(--fg-muted)"
              in HH.div [ HP.class_ (H.ClassName "overview-cell-inner") ]
                [ HH.div [ HP.id ("pedal-thumb-" <> pidStr) ] []
                , HH.div
                    [ HP.class_ (H.ClassName "overview-cell-label")
                    , HP.attr (HH.AttrName "style") labelStyle
                    ]
                    [ HH.text def.meta.name ]
                ]
            else
              HH.div [ HP.class_ (H.ClassName "overview-cell-placeholder") ]
                [ HH.div [ HP.class_ (H.ClassName "overview-cell-name") ] [ HH.text def.meta.name ]
                , HH.div [ HP.class_ (H.ClassName "overview-cell-brand") ] [ HH.text def.meta.brand ]
                ]
          Nothing ->
            HH.div [ HP.class_ (H.ClassName "overview-cell-placeholder") ]
              [ HH.text pidStr ]
      ]

-- | No-op callbacks for inactive pedal donuts (visual only)
noopCallbacks :: DonutTree.PedalCallbacks
noopCallbacks =
  { onKnobDragStart: \_ _ -> pure unit
  , onKnobDragStart2D: \_ _ _ _ -> pure unit
  , onSegmentClick: \_ _ -> pure unit
  , onToggleClick: \_ _ -> pure unit
  }

-- | Build real callbacks for the active pedal
makeCallbacks :: PedalId -> HS.Listener Action -> DonutTree.PedalCallbacks
makeCallbacks pid listener =
  { onKnobDragStart: \cc val -> HS.notify listener (HatsKnobDragStart pid cc val)
  , onKnobDragStart2D: \ccY valY ccXArg valX -> HS.notify listener (HatsKnobDragStart2D pid ccY valY ccXArg valX)
  , onSegmentClick: \cc val -> HS.notify listener (HatsSegmentClick pid cc val)
  , onToggleClick: \cc val -> HS.notify listener (HatsToggleClick pid cc val)
  }

-- | Render a HATS tree into a container
renderHatsInto :: String -> PedalLayout -> PedalState -> DonutTree.PedalCallbacks -> Effect Unit
renderHatsInto containerId layout ps callbacks = do
  clearContainer containerId
  let tree = DonutTree.pedalTree layout ps callbacks
  _ <- rerender containerId tree
  pure unit

-- | Check if a pedal has a HATS view (layout or custom like Hedra)
hasHatsView :: PedalDef -> Boolean
hasHatsView def = def.meta.id == PedalId "hedra" || isJust def.layout
  where
  isJust (Just _) = true
  isJust Nothing = false

-- | Render the Hedra piano keyboard into a container
renderHedraInto :: String -> PedalState -> DonutTree.PedalCallbacks -> Effect Unit
renderHedraInto containerId ps callbacks = do
  clearContainer containerId
  let tree = HedraTree.hedraTree ps callbacks
  _ <- rerender containerId tree
  pure unit

-- | Subscribe to document-level mouse events for drag tracking
setupDragSubscription :: forall m. MonadAff m => H.HalogenM State Action () Output m H.SubscriptionId
setupDragSubscription =
  H.subscribe $ HS.makeEmitter \emit -> do
    moveFn <- eventListener \e ->
      case ME.fromEvent e of
        Just mouseEvt -> emit (DragMove (ME.clientY mouseEvt) (ME.clientX mouseEvt))
        Nothing -> pure unit
    upFn <- eventListener \_ -> emit DragEnd
    target <- Window.toEventTarget <$> window
    addEventListener (EventType "mousemove") moveFn false target
    addEventListener (EventType "mouseup") upFn false target
    pure do
      removeEventListener (EventType "mousemove") moveFn false target
      removeEventListener (EventType "mouseup") upFn false target

-- | Re-render all pedal donuts. Active gets real callbacks, others get noop.
rerenderAll :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
rerenderAll = do
  st <- H.get
  for_ st.input.cardOrder \pid -> do
    let mDef = CRegistry.findPedal st.input.registry pid
        mPs = Map.lookup pid st.input.engine
        isActive = st.input.activePedal == Just pid
    case mDef, mPs of
      Just def, Just ps ->
        if def.meta.id == PedalId "hedra" then do
          let callbacks = case isActive, st.hatsListener of
                true, Just listener -> makeCallbacks pid listener
                _, _ -> noopCallbacks
          H.liftEffect $ renderHedraInto (thumbContainerId pid) ps callbacks
        else case def.layout of
          Just layout -> do
            let callbacks = case isActive, st.hatsListener of
                  true, Just listener -> makeCallbacks pid listener
                  _, _ -> noopCallbacks
            H.liftEffect $ renderHatsInto (thumbContainerId pid) layout ps callbacks
          Nothing -> pure unit
      _, _ -> pure unit

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    { listener, emitter } <- H.liftEffect HS.create
    sid <- H.subscribe emitter
    -- Read initial window dimensions
    w <- H.liftEffect $ Window.innerWidth =<< window
    h <- H.liftEffect $ Window.innerHeight =<< window
    let cw = toNumber w - 32.0  -- 16px padding each side
        ch = toNumber h - 82.0  -- header (~50px) + padding (32px)
    -- Subscribe to resize events
    resizeId <- H.subscribe $ HS.makeEmitter \emit -> do
      fn <- eventListener \_ -> emit WindowResize
      target <- Window.toEventTarget <$> window
      addEventListener (EventType "resize") fn false target
      pure $ removeEventListener (EventType "resize") fn false target
    H.modify_ _
      { hatsListener = Just listener
      , hatsSub = Just sid
      , containerW = cw
      , containerH = ch
      , resizeSub = Just resizeId
      }
    rerenderAll

  Receive input -> do
    H.modify_ _ { input = input }
    rerenderAll

  WindowResize -> do
    w <- H.liftEffect $ Window.innerWidth =<< window
    h <- H.liftEffect $ Window.innerHeight =<< window
    let cw = toNumber w - 32.0
        ch = toNumber h - 82.0
    H.modify_ _ { containerW = cw, containerH = ch }
    rerenderAll

  HatsKnobDragStart pid cc val -> do
    sid <- setupDragSubscription
    H.modify_ _ { dragging = Just { cc, startY: 0, startVal: val, pedalId: pid
                                   , ccX: Nothing, startX: 0, startValX: 0 }
                 , dragSub = Just sid }

  HatsKnobDragStart2D pid ccY valY ccXArg valX -> do
    sid <- setupDragSubscription
    H.modify_ _ { dragging = Just { cc: ccY, startY: 0, startVal: valY, pedalId: pid
                                   , ccX: Just ccXArg, startX: 0, startValX: valX }
                 , dragSub = Just sid }

  HatsSegmentClick pid cc val ->
    H.raise (ValueChanged pid cc (unsafeMidiValue val))

  HatsToggleClick pid cc val ->
    H.raise (ValueChanged pid cc (unsafeMidiValue (if val > 63 then 0 else 127)))

  DragMove clientY clientX -> do
    st <- H.get
    case st.dragging of
      Just drag -> do
        if drag.startY == 0
          then H.modify_ _ { dragging = Just drag { startY = clientY, startX = clientX } }
          else do
            -- Y axis (always active)
            let deltaY = drag.startY - clientY
                newValY = clamp 0 127 (drag.startVal + deltaY)
            H.raise (ValueChanged drag.pedalId drag.cc (unsafeMidiValue newValY))
            -- X axis (only for 2D drags)
            case drag.ccX of
              Just xCC -> do
                let deltaX = clientX - drag.startX
                    newValX = clamp 0 127 (drag.startValX + deltaX)
                H.raise (ValueChanged drag.pedalId xCC (unsafeMidiValue newValX))
              Nothing -> pure unit
      Nothing -> pure unit

  DragEnd -> do
    st <- H.get
    case st.dragSub of
      Just sid -> do
        H.unsubscribe sid
        H.modify_ _ { dragging = Nothing, dragSub = Nothing }
      Nothing ->
        H.modify_ _ { dragging = Nothing }
