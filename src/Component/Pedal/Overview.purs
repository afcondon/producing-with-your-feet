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
  , registry :: PedalRegistry
  , cardOrder :: Array PedalId
  , activePedal :: Maybe PedalId
  }

data Output
  = BackToGrid
  | ValueChanged PedalId CC MidiValue
  -- | A card was clicked. Raised rather than handled here because which pedal is
  -- | active belongs to the app — the header pills set the same field, and two
  -- | places deciding it separately is how they drift apart.
  | SelectPedal PedalId

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
  | CellClicked PedalId

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
    hasActive = state.input.activePedal /= Nothing
    detailW = if hasActive then min (state.containerH * Layout.svgAspect) (state.containerW * 0.42) else 0.0
    gridW = if hasActive then state.containerW - detailW - 8.0 else state.containerW
    gridCss = Layout.gridStyle gridW state.containerH n
  in
    HH.div [ HP.class_ (H.ClassName "overview-container") ]
      [ if hasActive
          then HH.div
            [ HP.class_ (H.ClassName "overview-detail")
            , HP.id "overview-detail"
            ] []
          else HH.text ""
      , HH.div
          [ HP.class_ (H.ClassName "overview-treemap")
          , HP.attr (HH.AttrName "style") gridCss
          ]
          (map (renderCell state) state.input.cardOrder)
      ]

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
    cellStyle = colorBg
    cellProps =
      [ HP.class_ (H.ClassName cls)
      , HE.onClick \_ -> CellClicked pid
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

-- | Subscribe to window-level pointer events for drag tracking.
-- | See the note on the equivalent in `Component.Pedal.View` — pointer rather
-- | than mouse events so touch drags work, `MouseEvent.fromEvent` because
-- | PointerEvent extends MouseEvent, and `pointercancel` so an interrupted
-- | gesture cannot leave the drag stuck.
setupDragSubscription :: forall m. MonadAff m => H.HalogenM State Action () Output m H.SubscriptionId
setupDragSubscription =
  H.subscribe $ HS.makeEmitter \emit -> do
    moveFn <- eventListener \e ->
      case ME.fromEvent e of
        Just pointerEvt -> emit (DragMove (ME.clientY pointerEvt) (ME.clientX pointerEvt))
        Nothing -> pure unit
    upFn <- eventListener \_ -> emit DragEnd
    target <- Window.toEventTarget <$> window
    addEventListener (EventType "pointermove") moveFn false target
    addEventListener (EventType "pointerup") upFn false target
    addEventListener (EventType "pointercancel") upFn false target
    pure do
      removeEventListener (EventType "pointermove") moveFn false target
      removeEventListener (EventType "pointerup") upFn false target
      removeEventListener (EventType "pointercancel") upFn false target

detailContainerId :: String
detailContainerId = "#overview-detail"

-- | Re-render all pedal donuts + the active detail panel.
rerenderAll :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
rerenderAll = do
  st <- H.get
  -- Render thumbnails (all pedals, noop callbacks)
  for_ st.input.cardOrder \pid -> do
    let mDef = CRegistry.findPedal st.input.registry pid
        mPs = Map.lookup pid st.input.engine
    case mDef, mPs of
      Just def, Just ps ->
        if def.meta.id == PedalId "hedra" then
          H.liftEffect $ renderHedraInto (thumbContainerId pid) ps noopCallbacks
        else case def.layout of
          Just layout ->
            H.liftEffect $ renderHatsInto (thumbContainerId pid) layout ps noopCallbacks
          Nothing -> pure unit
      _, _ -> pure unit
  -- Render active pedal large in the detail panel (with real callbacks)
  case st.input.activePedal, st.hatsListener of
    Just pid, Just listener -> do
      let mDef = CRegistry.findPedal st.input.registry pid
          mPs = Map.lookup pid st.input.engine
          callbacks = makeCallbacks pid listener
      case mDef, mPs of
        Just def, Just ps ->
          if def.meta.id == PedalId "hedra" then
            H.liftEffect $ renderHedraInto detailContainerId ps callbacks
          else case def.layout of
            Just layout ->
              H.liftEffect $ renderHatsInto detailContainerId layout ps callbacks
            Nothing -> pure unit
        _, _ -> pure unit
    _, _ -> pure unit

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    { listener, emitter } <- H.liftEffect HS.create
    sid <- H.subscribe emitter
    -- Read initial window dimensions
    w <- H.liftEffect $ Window.innerWidth =<< window
    h <- H.liftEffect $ Window.innerHeight =<< window
    let cw = toNumber w - 32.0   -- 16px padding each side
        ch = (toNumber h - 75.0 - 32.0) * 0.9  -- 90% of available height
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

  -- Halogen calls `receive` on every parent render, not only when this
  -- component's input changed — and the parent re-renders ten times a second to
  -- poll the looper. Rebuilding thirteen HATS trees at that rate tears the DOM
  -- out from under the pointer: mousedown lands on one node, mouseup on its
  -- replacement, and the browser never synthesises a `click`. Footswitches went
  -- dead while knobs kept working, because knobs fire on `pointerdown`, which
  -- happens before the next rebuild.
  --
  -- So re-render only when something we actually draw from has moved.
  Receive input -> do
    st <- H.get
    H.modify_ _ { input = input }
    when (st.input.engine /= input.engine
       || st.input.activePedal /= input.activePedal
       || st.input.cardOrder /= input.cardOrder)
      rerenderAll

  WindowResize -> do
    w <- H.liftEffect $ Window.innerWidth =<< window
    h <- H.liftEffect $ Window.innerHeight =<< window
    let cw = toNumber w - 32.0
        ch = (toNumber h - 75.0 - 32.0) * 0.9
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

  -- Only when nothing is being dragged. A knob drag on the active card ends with
  -- the pointer still inside the cell, so the browser delivers a click too —
  -- and without this guard, finishing a drag would toggle the card shut.
  CellClicked pid -> do
    st <- H.get
    case st.dragging of
      Just _ -> pure unit
      Nothing -> H.raise (SelectPedal pid)
