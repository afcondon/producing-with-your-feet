module Component.Pedal.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Component.Pedal.Donut as Donut
import Component.Pedal.MoodLayout as Layout
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Data.Const (Const)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, unMidiValue, unsafeMidiValue)
import Data.Pedal (PedalId)
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, PedalState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
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
  }

data Action
  = Receive Input
  | ClickBack
  | HandleDonut Donut.DonutEvent
  | DragMove Int       -- current clientY
  | DragEnd

type Slot = H.Slot (Const Void) Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i -> { input: i, dragging: Nothing, dragSub: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
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
          Just def, Just ps ->
            HH.div [ HP.class_ (H.ClassName "pedal-view") ]
              [ renderHeader def.meta.name def.meta.brand ps
              , renderSvg ps
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

svgElement :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElement name = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr name val = HP.attr (HH.AttrName name) val

renderSvg :: forall m. PedalState -> H.ComponentHTML Action () m
renderSvg ps =
  svgElement "svg"
    [ svgAttr "viewBox" "0 0 320 470"
    , svgAttr "class" "pedal-svg"
    ]
    (  [ renderColumnHeaders ]
    <> map (renderKnobDonut ps) Layout.moodKnobs
    <> map (renderFsDonut ps) Layout.moodFootswitches
    <> [ Donut.renderSectionLine 356.0 ]
    <> [ Donut.renderConfigRow Layout.moodConfig (\cfg -> lookupCC cfg.cc ps) HandleDonut ]
    <> [ Donut.renderSectionLine 396.0 ]
    <> [ Donut.renderDipGrid Layout.moodDips (\dip -> lookupCC dip.cc ps) HandleDonut ]
    )

renderColumnHeaders :: forall w i. HH.HTML w i
renderColumnHeaders =
  svgElement "g" []
    [ colHead 60.0 "Wet"
    , colHead 160.0 "Shared"
    , colHead 260.0 "ML"
    ]
  where
  colHead x label =
    svgElement "text"
      [ svgAttr "x" (show x), svgAttr "y" "12"
      , svgAttr "text-anchor" "middle"
      , svgAttr "font-size" "10"
      , svgAttr "font-weight" "700"
      , svgAttr "fill" "#999"
      , svgAttr "letter-spacing" "0.05em"
      ] [ HH.text label ]

renderKnobDonut :: forall m. PedalState -> Layout.KnobPair -> H.ComponentHTML Action () m
renderKnobDonut ps knob =
  let
    primaryVal = lookupCC knob.primaryCC ps
    hiddenVal = lookupCC knob.hiddenCC ps
  in
    Donut.renderDonut knob ps { primaryVal, hiddenVal } HandleDonut

renderFsDonut :: forall m. PedalState -> Layout.Footswitch -> H.ComponentHTML Action () m
renderFsDonut ps fs =
  let
    ledVal = case fs.ledCC of
      Just lcc -> lookupCC lcc ps
      Nothing -> 0
  in
    Donut.renderFootswitch fs (lookupCC fs.cc ps) ledVal HandleDonut

lookupCC :: CC -> PedalState -> Int
lookupCC ccNum ps = fromMaybe 0 (map unMidiValue (Map.lookup ccNum ps.values))

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  ClickBack -> H.raise BackToGrid

  HandleDonut evt -> do
    st <- H.get
    let pid = st.input.pedalId
    case evt of
      Donut.KnobDragStart cc val me -> do
        -- Set up document-level drag tracking
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
        H.modify_ _ { dragging = Just { cc, startY: ME.clientY me, startVal: val }, dragSub = Just sid }

      Donut.SegmentClick cc val ->
        H.raise (ValueChanged pid cc (unsafeMidiValue val))

      Donut.ToggleClick cc val ->
        H.raise (ValueChanged pid cc (unsafeMidiValue (if val > 63 then 0 else 127)))

  DragMove clientY -> do
    st <- H.get
    case st.dragging of
      Just drag -> do
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
