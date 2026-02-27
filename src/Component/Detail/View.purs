module Component.Detail.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Component.Detail.Control as Control
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue)
import Data.Pedal (PedalDef, PedalId, Section, SectionLayout(..))
import Data.Tuple (Tuple(..))
import Data.Pedal.Modes (DualChannelModes)
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, PedalState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry

type Input =
  { engine :: EngineState
  , pedalId :: PedalId
  , cardOrder :: Array PedalId
  , registry :: PedalRegistry
  }

data Output
  = ValueChanged PedalId CC MidiValue
  | MomentarySent PedalId CC MidiValue
  | PedalSelected PedalId
  | InfoChanged PedalId String Int

type State =
  { input :: Input
  , collapsedSections :: Array String
  }

data Action
  = Receive Input
  | ClickTab PedalId
  | ToggleSection String
  | ControlEvent Control.ControlOutput

type Slot = H.Slot (Const Void) Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i -> { input: i, collapsedSections: [] }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "detail-view") ]
    [ case mDef of
        Nothing -> HH.text "Pedal not found"
        Just def -> case mPs of
          Nothing -> HH.text "No state"
          Just ps -> renderPedal def ps state
    ]
  where
  mDef = CRegistry.findPedal state.input.registry state.input.pedalId
  mPs = Map.lookup state.input.pedalId state.input.engine

renderPedal :: forall m. PedalDef -> PedalState -> State -> H.ComponentHTML Action () m
renderPedal def ps state =
  HH.div [ HP.class_ (H.ClassName "pedal-view") ]
    [ HH.div [ HP.class_ (H.ClassName "pedal-header") ]
        [ HH.h2_ [ HH.text def.meta.name ]
        , HH.span [ HP.class_ (H.ClassName "brand") ] [ HH.text def.meta.brand ]
        , HH.span [ HP.class_ (H.ClassName "channel") ] [ HH.text ("Ch " <> show ps.channel) ]
        ]
    , HH.div [ HP.class_ (H.ClassName "sections") ]
        (map (renderSection def.meta.id ps def.modes state) def.sections)
    ]

renderSection :: forall m. PedalId -> PedalState -> Maybe DualChannelModes -> State -> Section -> H.ComponentHTML Action () m
renderSection pid ps mModes state section =
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
        , case section.description of
            Nothing -> HH.text ""
            Just d -> HH.span [ HP.class_ (H.ClassName "section-desc") ] [ HH.text d ]
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

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  ClickTab pid -> H.raise (PedalSelected pid)
  ToggleSection name -> H.modify_ \st ->
    st { collapsedSections =
      if Array.elem name st.collapsedSections
        then Array.filter (_ /= name) st.collapsedSections
        else Array.snoc st.collapsedSections name
    }
  ControlEvent output -> case output of
    Control.SetCC pid cc val -> H.raise (ValueChanged pid cc val)
    Control.SetMultipleCC pid pairs -> for_ pairs (\(Tuple cc val) -> H.raise (ValueChanged pid cc val))
    Control.FireMomentary pid cc val -> H.raise (MomentarySent pid cc val)
    Control.SetInfo pid key val -> H.raise (InfoChanged pid key val)
