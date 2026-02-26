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
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, ProgramNumber, makeProgramNumber, unProgramNumber)
import Data.String.CodeUnits as SCU
import Data.Pedal (PedalDef, PedalId, Section, SectionLayout(..))
import Data.Pedal.Modes (DualChannelModes)
import Data.Preset (PedalPreset)
import Data.Tuple (Tuple(..))
import Engine (EngineState, MidiConnections, PedalState)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pedals.Registry as Registry

-- Note: Pedal order pills are rendered in the Header component

type Input =
  { engine :: EngineState
  , cardOrder :: Array PedalId
  , hiddenPedals :: Array PedalId
  , presets :: Array PedalPreset
  , connections :: MidiConnections
  }

data Output
  = PedalClicked PedalId
  | PedalFocused PedalId
  | OrderChanged (Array PedalId)
  | ValueChanged PedalId CC MidiValue
  | MomentarySent PedalId CC MidiValue
  | InfoChanged PedalId String Int
  | RecallPreset PedalPreset
  | SendPC PedalId ProgramNumber

type State =
  { input :: Input
  , collapsedSections :: Array String
  , expandedPresets :: Array PedalId
  }

data Action
  = Receive Input
  | FocusPedal PedalId
  | OpenPedal PedalId
  | ToggleSection String
  | ControlEvent Control.ControlOutput
  | TogglePresets PedalId
  | ClickRecall PedalPreset
  | ClickSlot PedalId ProgramNumber

type Slot = H.Slot (Const Void) Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i -> { input: i, collapsedSections: [], expandedPresets: [] }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "grid-view") ]
    ( Array.mapMaybe renderCard visibleOrder )
  where
  visibleOrder = Array.filter (\pid -> not (Array.elem pid state.input.hiddenPedals)) state.input.cardOrder

  renderCard pid = do
    def <- Registry.findPedal pid
    ps <- Map.lookup pid state.input.engine
    let pedalPresets = Array.filter (\p -> p.pedalId == pid) state.input.presets
        isExpanded = Array.elem pid state.expandedPresets
    pure $ HH.div
      [ HP.class_ (H.ClassName "pedal-card")
      , HP.attr (HH.AttrName "style") (case def.meta.color of
          Just c -> "--pedal-color: " <> toHexString c
          Nothing -> "")
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "card-header")
          , HE.onClick \_ -> FocusPedal pid
          ]
          [ HH.span [ HP.class_ (H.ClassName "card-name") ] [ HH.text def.meta.name ]
          , HH.span [ HP.class_ (H.ClassName "card-brand") ] [ HH.text def.meta.brand ]
          ]
      , HH.div [ HP.class_ (H.ClassName "card-sections") ]
          (map (renderSection def.meta.id ps def.modes) def.sections)
      , renderPresetToggle pid pedalPresets isExpanded
      , if isExpanded
          then renderPresetSection def pedalPresets
          else HH.text ""
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

  renderPresetToggle :: PedalId -> Array PedalPreset -> Boolean -> H.ComponentHTML Action () m
  renderPresetToggle pid presets isExpanded =
    HH.div
      [ HP.class_ (H.ClassName "card-presets-toggle")
      , HE.onClick \_ -> TogglePresets pid
      ]
      [ HH.span [ HP.class_ (H.ClassName "collapse-arrow") ]
          [ HH.text (if isExpanded then "\x25BE" else "\x25B8") ]
      , HH.text (" Presets" <> if Array.null presets then "" else " (" <> show (Array.length presets) <> ")")
      ]

  renderPresetSection :: PedalDef -> Array PedalPreset -> H.ComponentHTML Action () m
  renderPresetSection def presets =
    HH.div [ HP.class_ (H.ClassName "card-presets-section") ]
      [ if Array.null presets
          then HH.div [ HP.class_ (H.ClassName "empty-state") ] [ HH.text "No presets saved" ]
          else HH.div [ HP.class_ (H.ClassName "preset-list") ]
            (map renderPresetItem presets)
      , case slotRange def.meta.brand of
          Nothing -> HH.text ""
          Just range -> renderSlotGrid def range presets
      ]

  renderPresetItem :: PedalPreset -> H.ComponentHTML Action () m
  renderPresetItem preset =
    HH.div [ HP.class_ (H.ClassName "preset-item") ]
      [ HH.div [ HP.class_ (H.ClassName "preset-info") ]
          [ HH.div [ HP.class_ (H.ClassName "preset-name-row") ]
              [ HH.span [ HP.class_ (H.ClassName "preset-name") ] [ HH.text preset.name ]
              , renderSlotBadge preset
              , HH.span [ HP.class_ (H.ClassName "preset-date") ] [ HH.text (formatDate preset.modified) ]
              ]
          ]
      , HH.div [ HP.class_ (H.ClassName "preset-item-actions") ]
          [ HH.button
              [ HP.class_ (H.ClassName "recall-btn")
              , HE.onClick \_ -> ClickRecall preset
              ]
              [ HH.text "Recall" ]
          ]
      ]

  renderSlotBadge :: PedalPreset -> H.ComponentHTML Action () m
  renderSlotBadge preset = case preset.savedSlot of
    Just slot ->
      HH.span
        [ HP.class_ (H.ClassName "preset-slot-badge")
        , HP.attr (HH.AttrName "style") (badgeColor preset.pedalId)
        ]
        [ HH.text ("Slot " <> show (unProgramNumber slot)) ]
    Nothing ->
      HH.span [ HP.class_ (H.ClassName "preset-slot-badge library") ]
        [ HH.text "LIB" ]

  badgeColor :: PedalId -> String
  badgeColor pid = case Registry.findPedal pid of
    Just def -> case def.meta.color of
      Just c -> "background: " <> toHexString c
      Nothing -> ""
    Nothing -> ""

  renderSlotGrid :: PedalDef -> { start :: Int, count :: Int } -> Array PedalPreset -> H.ComponentHTML Action () m
  renderSlotGrid def range presets =
    HH.div [ HP.class_ (H.ClassName "slot-grid-section") ]
      [ HH.div [ HP.class_ (H.ClassName "slot-grid-heading") ] [ HH.text "Slots" ]
      , HH.div [ HP.class_ (H.ClassName "slot-grid") ]
          (Array.mapMaybe renderSlotBtn (Array.range range.start (range.start + range.count - 1)))
      ]
    where
    assignedSlots = Array.mapMaybe _.savedSlot presets

    renderSlotBtn n = do
      pn <- makeProgramNumber n
      let isAssigned = Array.elem pn assignedSlots
          cls = "slot-btn" <> (if isAssigned then " assigned" else "")
      pure $ HH.button
        [ HP.class_ (H.ClassName cls)
        , HE.onClick \_ -> ClickSlot def.meta.id pn
        ]
        [ HH.text (show n) ]

  formatDate :: String -> String
  formatDate iso = SCU.take 10 iso

-- Slot ranges by brand
slotRange :: String -> Maybe { start :: Int, count :: Int }
slotRange = case _ of
  "Meris"      -> Just { start: 0, count: 16 }
  "Strymon"    -> Just { start: 50, count: 26 }
  "Chase Bliss" -> Just { start: 1, count: 122 }
  _            -> Nothing



handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  FocusPedal pid -> H.raise (PedalFocused pid)
  OpenPedal pid -> H.raise (PedalClicked pid)
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
  TogglePresets pid -> H.modify_ \st ->
    st { expandedPresets =
      if Array.elem pid st.expandedPresets
        then Array.filter (_ /= pid) st.expandedPresets
        else Array.snoc st.expandedPresets pid
    }
  ClickRecall preset -> H.raise (RecallPreset preset)
  ClickSlot pid pn -> H.raise (SendPC pid pn)
