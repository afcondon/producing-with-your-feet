module Component.Pedal.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Component.Pedal.Donut as Donut
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Data.Array (mapWithIndex, null) as Array
import Data.Const (Const)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeMidiValue)
import Data.Pedal (PedalId)
import Data.Pedal.Layout (PedalLayout)
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
          Just def, Just ps -> case def.layout of
            Just layout ->
              HH.div [ HP.class_ (H.ClassName "pedal-view") ]
                [ renderHeader def.meta.name def.meta.brand ps
                , renderSvg layout ps
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

svgElement :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElement name = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr name val = HP.attr (HH.AttrName name) val

renderSvg :: forall m. PedalLayout -> PedalState -> H.ComponentHTML Action () m
renderSvg layout ps =
  let
    vb = layout.viewBox
    viewBoxStr = "0 0 " <> show vb.width <> " " <> show vb.height
    fsY = Donut.fsRowY layout.knobRows
    configY = fsY + 44.0
    dipBaseY = configY + 36.0
    hasConfig = not (Array.null layout.config)
    hasDips = not (Array.null layout.dipBanks)
  in
    svgElement "svg"
      [ svgAttr "viewBox" viewBoxStr
      , svgAttr "class" "pedal-svg"
      ]
      (  [ renderColumnHeaders layout ]
      <> map (\knob -> Donut.renderDonut layout knob ps HandleDonut) layout.knobs
      <> map (\fs -> Donut.renderFootswitch layout fs ps HandleDonut) layout.footswitches
      <> (if hasConfig
            then [ Donut.renderSectionLine (configY - 18.0)
                 , Donut.renderConfigRow configY layout ps HandleDonut
                 ]
            else [])
      <> (if hasDips
            then [ Donut.renderSectionLine (dipBaseY - 14.0)
                 , Donut.renderDipGrid dipBaseY layout ps HandleDonut
                 ]
            else [])
      )

renderColumnHeaders :: forall w i. PedalLayout -> HH.HTML w i
renderColumnHeaders layout =
  svgElement "g" []
    (Array.mapWithIndex renderGroup layout.groups)
  where
  renderGroup idx group =
    let x = Donut.colXFor layout.viewBox.width layout.columns idx
    in svgElement "text"
      [ svgAttr "x" (show x), svgAttr "y" "12"
      , svgAttr "text-anchor" "middle"
      , svgAttr "font-size" "10"
      , svgAttr "font-weight" "700"
      , svgAttr "fill" "#999"
      , svgAttr "letter-spacing" "0.05em"
      ] [ HH.text group.label ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  ClickBack -> H.raise BackToGrid

  HandleDonut evt -> do
    st <- H.get
    let pid = st.input.pedalId
    case evt of
      Donut.KnobDragStart cc val me -> do
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
