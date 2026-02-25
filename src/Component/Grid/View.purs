module Component.Grid.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Color (toHexString)
import Component.Detail.Control as Control
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, unMidiValue)
import Data.Pedal (PedalId, Section, SectionLayout(..))
import Data.Pedal.Engage (EngageConfig(..))
import Data.Pedal.Modes (DualChannelModes)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, PedalState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pedals.Registry as Registry

type Input =
  { engine :: EngineState
  , cardOrder :: Array PedalId
  }

data Output
  = PedalClicked PedalId
  | OrderChanged (Array PedalId)
  | ValueChanged PedalId CC MidiValue
  | MomentarySent PedalId CC MidiValue
  | InfoChanged PedalId String Int

type State =
  { input :: Input
  , collapsedSections :: Array String
  , hiddenPedals :: Array PedalId
  }

data Action
  = Receive Input
  | ClickPedal PedalId
  | ToggleSection String
  | TogglePedal PedalId
  | ControlEvent Control.ControlOutput

type Slot = H.Slot (Const Void) Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i -> { input: i, collapsedSections: [], hiddenPedals: [] }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ renderOrderStrip state
    , HH.div [ HP.class_ (H.ClassName "grid-view") ]
        ( Array.mapMaybe renderCard visibleOrder )
    ]
  where
  visibleOrder = Array.filter (\pid -> not (Array.elem pid state.hiddenPedals)) state.input.cardOrder

  renderCard pid = do
    def <- Registry.findPedal pid
    ps <- Map.lookup pid state.input.engine
    pure $ HH.div
      [ HP.class_ (H.ClassName "pedal-card")
      , HP.attr (HH.AttrName "style") (case def.meta.color of
          Just c -> "--pedal-color: " <> toHexString c
          Nothing -> "")
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "card-header")
          , HE.onClick \_ -> ClickPedal pid
          ]
          [ HH.span [ HP.class_ (H.ClassName "card-name") ] [ HH.text def.meta.name ]
          , HH.span [ HP.class_ (H.ClassName "card-brand") ] [ HH.text def.meta.brand ]
          ]
      , HH.div [ HP.class_ (H.ClassName "card-status") ]
          [ engageIndicator def ps ]
      , HH.div [ HP.class_ (H.ClassName "card-sections") ]
          (map (renderSection def.meta.id ps def.modes) def.sections)
      ]

  engageIndicator def ps = case def.engage of
    SingleEngage cc' ->
      let val = fromMaybe 0 (map unMidiValue (Map.lookup cc' ps.values))
      in HH.span
        [ HP.class_ (H.ClassName (if val > 0 then "engaged" else "bypassed")) ]
        [ HH.text (if val > 0 then "On" else "Off") ]
    DualEngage { a, b } ->
      let valA = fromMaybe 0 (map unMidiValue (Map.lookup a.cc ps.values))
          valB = fromMaybe 0 (map unMidiValue (Map.lookup b.cc ps.values))
      in HH.span_
        [ HH.span
            [ HP.class_ (H.ClassName (if valA > 0 then "engaged" else "bypassed")) ]
            [ HH.text a.label ]
        , HH.text " "
        , HH.span
            [ HP.class_ (H.ClassName (if valB > 0 then "engaged" else "bypassed")) ]
            [ HH.text b.label ]
        ]

  renderSection :: PedalId -> PedalState -> Maybe DualChannelModes -> Section -> H.ComponentHTML Action () m
  renderSection pid ps mModes section =
    HH.div
      [ HP.class_ (H.ClassName cls) ]
      [ HH.div
          ( [ HP.class_ (H.ClassName headingCls) ]
            <> if section.collapsed
                 then [ HE.onClick \_ -> ToggleSection section.name ]
                 else []
          )
          [ HH.h3 [ HP.class_ (H.ClassName "section-title") ]
              ( if section.collapsed
                  then [ HH.span [ HP.class_ (H.ClassName "collapse-arrow") ]
                           [ HH.text (if isCollapsed then "\x25B8" else "\x25BE") ]
                       , HH.text (" " <> section.name)
                       ]
                  else [ HH.text section.name ]
              )
          ]
      , if isCollapsed
          then HH.text ""
          else renderBody
      ]
    where
    isCollapsed = section.collapsed && not (Array.elem section.name state.collapsedSections)
      || not section.collapsed && Array.elem section.name state.collapsedSections

    cls = "section" <> (if isCollapsed then " collapsed" else "")

    headingCls = "section-heading" <> (if section.collapsed then " collapsible" else "")

    renderCtrl = map ControlEvent <<< Control.renderControl pid ps mModes

    renderBody = case section.layout of
      DualColumn ->
        let half = (Array.length section.controls + 1) / 2
            leftCtrls = Array.take half section.controls
            rightCtrls = Array.drop half section.controls
        in HH.div [ HP.class_ (H.ClassName "section-body dual-column") ]
          [ HH.div [ HP.class_ (H.ClassName "dual-col") ] (map renderCtrl leftCtrls)
          , HH.div [ HP.class_ (H.ClassName "dual-col") ] (map renderCtrl rightCtrls)
          ]
      DipGrid ->
        HH.div [ HP.class_ (H.ClassName "section-body dip-grid") ]
          (map renderCtrl section.controls)
      DefaultLayout ->
        HH.div [ HP.class_ (H.ClassName "section-body") ]
          (map renderCtrl section.controls)

renderOrderStrip :: forall m. State -> H.ComponentHTML Action () m
renderOrderStrip state =
  HH.div [ HP.class_ (H.ClassName "order-strip") ]
    ( Array.mapMaybe renderPill state.input.cardOrder )
  where
  renderPill pid = do
    def <- Registry.findPedal pid
    let isHidden = Array.elem pid state.hiddenPedals
        colorStyle = case def.meta.color of
          Just c -> "background: " <> toHexString c
          Nothing -> ""
    pure $ HH.button
      [ HP.class_ (H.ClassName ("order-block" <> if isHidden then " hidden" else ""))
      , HP.attr (HH.AttrName "style") colorStyle
      , HE.onClick \_ -> TogglePedal pid
      ]
      [ HH.text def.meta.name ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  ClickPedal pid -> H.raise (PedalClicked pid)
  ToggleSection name -> H.modify_ \st ->
    st { collapsedSections =
      if Array.elem name st.collapsedSections
        then Array.filter (_ /= name) st.collapsedSections
        else Array.snoc st.collapsedSections name
    }
  TogglePedal pid -> H.modify_ \st ->
    st { hiddenPedals =
      if Array.elem pid st.hiddenPedals
        then Array.filter (_ /= pid) st.hiddenPedals
        else Array.snoc st.hiddenPedals pid
    }
  ControlEvent output -> case output of
    Control.SetCC pid cc val -> H.raise (ValueChanged pid cc val)
    Control.SetMultipleCC pid pairs -> for_ pairs (\(Tuple cc val) -> H.raise (ValueChanged pid cc val))
    Control.FireMomentary pid cc val -> H.raise (MomentarySent pid cc val)
    Control.SetInfo pid key val -> H.raise (InfoChanged pid key val)
