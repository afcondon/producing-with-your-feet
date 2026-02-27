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
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, ProgramNumber, makeProgramNumber, unProgramNumber)
import Data.String.CodeUnits as SCU
import Data.Pedal (PedalDef, PedalId, Section, SectionLayout(..))
import Data.Pedal.Modes (DualChannelModes)
import Data.Preset (PedalPreset, PresetId)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Engine (EngineState, MidiConnections, PedalState)
import Effect.Aff.Class (class MonadAff)
import Foreign.FileIO as FileIO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry

-- Note: Pedal order pills are rendered in the Header component

type Input =
  { engine :: EngineState
  , cardOrder :: Array PedalId
  , hiddenPedals :: Array PedalId
  , presets :: Array PedalPreset
  , connections :: MidiConnections
  , registry :: PedalRegistry
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
  | SavePreset { pedalId :: PedalId, name :: String, description :: String, notes :: String }
  | OverwritePreset PresetId PedalId
  | DeletePreset PresetId
  | AssignSlot PresetId ProgramNumber
  | ExportPreset PedalPreset
  | ImportPresets (Array PedalPreset)

data SlotAssignPhase = EnterSlot | ConfirmSave

type State =
  { input :: Input
  , collapsedSections :: Array String
  , expandedPresets :: Array PedalId
  , showSaveForm :: Maybe PedalId
  , saveName :: String
  , saveDescription :: String
  , saveNotes :: String
  , assigningSlot :: Maybe { presetId :: PresetId, phase :: SlotAssignPhase, slotNumber :: Int }
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
  -- CRUD actions
  | ShowSaveForm PedalId
  | CancelSaveForm
  | UpdateSaveName String
  | UpdateSaveDescription String
  | UpdateSaveNotes String
  | CommitSave PedalId
  | ClickOverwrite PedalPreset
  | ClickDelete PresetId
  | ClickExport PedalPreset
  | StartAssignSlot PresetId
  | SetSlotNumber String
  | SendSlotPC PresetId Int
  | ConfirmSlotAssign PresetId Int
  | CancelAssign

type Slot = H.Slot (Const Void) Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i ->
        { input: i
        , collapsedSections: []
        , expandedPresets: []
        , showSaveForm: Nothing
        , saveName: ""
        , saveDescription: ""
        , saveNotes: ""
        , assigningSlot: Nothing
        }
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
    def <- CRegistry.findPedal state.input.registry pid
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
          , HH.span [ HP.class_ (H.ClassName "card-channel") ] [ HH.text ("#" <> show ps.channel) ]
          ]
      , HH.div [ HP.class_ (H.ClassName "card-sections") ]
          (map (renderSection def.meta.id ps def.modes) def.sections)
      , renderPresetToggle pid pedalPresets isExpanded
      , if isExpanded
          then renderPresetSection state def pedalPresets
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

  renderPresetSection :: State -> PedalDef -> Array PedalPreset -> H.ComponentHTML Action () m
  renderPresetSection st def presets =
    HH.div [ HP.class_ (H.ClassName "card-presets-section") ]
      [ HH.div [ HP.class_ (H.ClassName "preset-actions") ]
          [ HH.button
              [ HE.onClick \_ -> ShowSaveForm def.meta.id ]
              [ HH.text "Save Current" ]
          ]
      , case st.showSaveForm of
          Just formPid | formPid == def.meta.id ->
            renderSaveForm def.meta.id
          _ -> HH.text ""
      , if Array.null presets
          then HH.div [ HP.class_ (H.ClassName "empty-state") ] [ HH.text "No presets saved" ]
          else HH.div [ HP.class_ (H.ClassName "preset-list") ]
            (map (renderPresetItem st def) presets)
      , case CRegistry.slotRange st.input.registry def.meta.brand of
          Nothing -> HH.text ""
          Just range -> renderSlotGrid def range presets
      ]

  renderSaveForm :: PedalId -> H.ComponentHTML Action () m
  renderSaveForm pid =
    HH.div [ HP.class_ (H.ClassName "preset-save-form") ]
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.placeholder "Preset name"
          , HP.value state.saveName
          , HE.onValueInput UpdateSaveName
          ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.placeholder "Description (optional)"
          , HP.value state.saveDescription
          , HE.onValueInput UpdateSaveDescription
          ]
      , HH.textarea
          [ HP.placeholder "Notes (optional)"
          , HP.value state.saveNotes
          , HE.onValueInput UpdateSaveNotes
          ]
      , HH.div [ HP.class_ (H.ClassName "preset-form-buttons") ]
          [ HH.button
              [ HP.class_ (H.ClassName "save-confirm")
              , HE.onClick \_ -> CommitSave pid
              ]
              [ HH.text "Save to Library" ]
          , HH.button
              [ HE.onClick \_ -> CancelSaveForm ]
              [ HH.text "Cancel" ]
          ]
      ]

  renderPresetItem :: State -> PedalDef -> PedalPreset -> H.ComponentHTML Action () m
  renderPresetItem st def preset =
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
          , HH.button
              [ HE.onClick \_ -> ClickOverwrite preset ]
              [ HH.text "Overwrite" ]
          , HH.button
              [ HE.onClick \_ -> StartAssignSlot preset.id ]
              [ HH.text "Assign Slot" ]
          , HH.button
              [ HE.onClick \_ -> ClickExport preset ]
              [ HH.text "Export" ]
          , HH.button
              [ HP.class_ (H.ClassName "delete-btn")
              , HE.onClick \_ -> ClickDelete preset.id
              ]
              [ HH.text "Delete" ]
          ]
      , case st.assigningSlot of
          Just asg | asg.presetId == preset.id ->
            renderAssignSlot st def preset
          _ -> HH.text ""
      ]

  renderAssignSlot :: State -> PedalDef -> PedalPreset -> H.ComponentHTML Action () m
  renderAssignSlot st def preset = case st.assigningSlot of
    Nothing -> HH.text ""
    Just asg -> case asg.phase of
      EnterSlot ->
        HH.div [ HP.class_ (H.ClassName "preset-assign-slot") ]
          [ HH.div [ HP.class_ (H.ClassName "assign-slot-input-row") ]
              [ HH.label_ [ HH.text "Slot #" ]
              , HH.input
                  [ HP.type_ HP.InputNumber
                  , HP.value (show asg.slotNumber)
                  , HE.onValueInput SetSlotNumber
                  ]
              , HH.button
                  [ HE.onClick \_ -> SendSlotPC preset.id asg.slotNumber ]
                  [ HH.text "Send PC" ]
              , HH.button
                  [ HE.onClick \_ -> CancelAssign ]
                  [ HH.text "Cancel" ]
              ]
          ]
      ConfirmSave ->
        HH.div [ HP.class_ (H.ClassName "preset-assign-slot") ]
          [ HH.div [ HP.class_ (H.ClassName "assign-slot-confirm") ]
              [ HH.div [ HP.class_ (H.ClassName "assign-slot-instructions") ]
                  [ HH.text (fromMaybe "Save this preset on your pedal now." def.meta.saveInstructions) ]
              , HH.div [ HP.class_ (H.ClassName "assign-slot-hint") ]
                  [ HH.text ("Saving to slot " <> show asg.slotNumber) ]
              , HH.div [ HP.class_ (H.ClassName "assign-slot-buttons") ]
                  [ HH.button
                      [ HP.class_ (H.ClassName "save-confirm")
                      , HE.onClick \_ -> ConfirmSlotAssign preset.id asg.slotNumber
                      ]
                      [ HH.text "Confirm Saved" ]
                  , HH.button
                      [ HE.onClick \_ -> SendSlotPC preset.id asg.slotNumber ]
                      [ HH.text "Back" ]
                  , HH.button
                      [ HE.onClick \_ -> CancelAssign ]
                      [ HH.text "Cancel" ]
                  ]
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
  badgeColor pid = case CRegistry.findPedal state.input.registry pid of
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

  -- Save form
  ShowSaveForm pid ->
    H.modify_ _ { showSaveForm = Just pid, saveName = "", saveDescription = "", saveNotes = "" }
  CancelSaveForm ->
    H.modify_ _ { showSaveForm = Nothing }
  UpdateSaveName s -> H.modify_ _ { saveName = s }
  UpdateSaveDescription s -> H.modify_ _ { saveDescription = s }
  UpdateSaveNotes s -> H.modify_ _ { saveNotes = s }
  CommitSave pid -> do
    st <- H.get
    when (st.saveName /= "") do
      H.raise (SavePreset { pedalId: pid, name: st.saveName, description: st.saveDescription, notes: st.saveNotes })
      H.modify_ _ { showSaveForm = Nothing }

  -- Per-preset actions
  ClickOverwrite preset -> do
    ok <- liftEffect $ FileIO.confirm ("Overwrite \"" <> preset.name <> "\" with current values?")
    when ok $ H.raise (OverwritePreset preset.id preset.pedalId)

  ClickDelete presetId -> do
    ok <- liftEffect $ FileIO.confirm "Delete this preset?"
    when ok $ H.raise (DeletePreset presetId)

  ClickExport preset -> H.raise (ExportPreset preset)

  -- Slot assignment
  StartAssignSlot presetId ->
    H.modify_ _ { assigningSlot = Just { presetId, phase: EnterSlot, slotNumber: 0 } }
  SetSlotNumber s ->
    for_ (Int.fromString s) \n ->
      H.modify_ \st -> st { assigningSlot = map (\a -> a { slotNumber = n }) st.assigningSlot }
  SendSlotPC presetId slotNum -> do
    for_ (makeProgramNumber slotNum) \pn -> do
      -- Find pedal for this preset
      st <- H.get
      for_ (Array.find (\p -> p.id == presetId) st.input.presets) \preset ->
        H.raise (SendPC preset.pedalId pn)
      H.modify_ _ { assigningSlot = Just { presetId, phase: ConfirmSave, slotNumber: slotNum } }
  ConfirmSlotAssign presetId slotNum -> do
    for_ (makeProgramNumber slotNum) \pn ->
      H.raise (AssignSlot presetId pn)
    H.modify_ _ { assigningSlot = Nothing }
  CancelAssign ->
    H.modify_ _ { assigningSlot = Nothing }
