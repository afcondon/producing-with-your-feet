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
import Data.Preset as Preset
import Data.String (trim)
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
import Config.Types (BrandSlots)
import Config.Registry as CRegistry

-- Note: Pedal order pills are rendered in the Header component

type Input =
  { engine :: EngineState
  , cardOrder :: Array PedalId
  , hiddenPedals :: Array PedalId
  , presets :: Array PedalPreset
  , connections :: MidiConnections
  , registry :: PedalRegistry
  -- | Result of the last baseline sweep, rendered beside the button that
  -- | started it. A sweep is otherwise silent for about a second and then
  -- | silent again if there was no MIDI output to send on.
  , baselineStatus :: Maybe String
  }

data Output
  = PedalClicked PedalId
  | PedalViewClicked PedalId
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
  | SaveSlotRef { pedalId :: PedalId, slot :: ProgramNumber, name :: String }
  | BaselinePedal PedalId

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
  -- | The open slot browser, if any: which pedal, which number is loaded, and
  -- | the name being typed. One at a time, like `assigningSlot` — several cards
  -- | can have their preset sections expanded at once, but only one slot is
  -- | ever being auditioned, because there is only one board making the sound.
  , browsingSlot :: Maybe { pedalId :: PedalId, slotNumber :: Int, name :: String }
  -- | Which cards have the slot grid open. Closed by default; see the comment
  -- | at the render site for why this is a performance decision, not a visual one.
  , expandedSlots :: Array PedalId
  }

data Action
  = Receive Input
  | FocusPedal PedalId
  | OpenPedal PedalId
  | OpenPedalView PedalId
  | ToggleSection String
  | ControlEvent Control.ControlOutput
  | TogglePresets PedalId
  | ClickRecall PedalPreset
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
  -- Slot browsing (referencing a slot we never captured)
  | OpenSlotBrowser PedalId
  | PickSlot PedalId Int
  | SetRefSlot String
  | SetRefName String
  | AuditionRef
  | CommitSlotRef
  | CancelSlotRef
  | ClickBaseline PedalDef
  | ToggleSlotGrid PedalId

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
        , browsingSlot: Nothing
        , expandedSlots: []
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
          , HH.button
              [ HP.class_ (H.ClassName "card-donut-btn")
              , HE.onClick \_ -> OpenPedalView pid
              ]
              [ HH.text "\x25CE" ]
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
          , HH.button
              [ HE.onClick \_ -> OpenSlotBrowser def.meta.id ]
              [ HH.text "Reference Slot" ]
          , HH.button
              [ HP.class_ (H.ClassName "baseline-btn")
              , HP.title ("Send all " <> show (Map.size def.baseline)
                  <> " baseline CCs to " <> def.meta.name
                  <> ", so the app's picture of it is true again.")
              , HE.onClick \_ -> ClickBaseline def
              ]
              [ HH.text "Baseline" ]
          ]
      , case st.input.baselineStatus of
          Nothing -> HH.text ""
          Just msg -> HH.div [ HP.class_ (H.ClassName "baseline-status") ] [ HH.text msg ]
      , case st.showSaveForm of
          Just formPid | formPid == def.meta.id ->
            renderSaveForm def.meta.id
          _ -> HH.text ""
      , case st.browsingSlot of
          Just b | b.pedalId == def.meta.id -> renderSlotRefForm def b
          _ -> HH.text ""
      , if Array.null presets
          then HH.div [ HP.class_ (H.ClassName "empty-state") ] [ HH.text "No presets saved" ]
          else HH.div [ HP.class_ (H.ClassName "preset-list") ]
            (map (renderPresetItem st def) presets)
      -- Collapsed by default, and not merely for tidiness: Halogen rebuilds
      -- this subtree on every engine change, and at a full Program Change span
      -- that is 128 buttons per re-render. Left open it put about a second on
      -- each CC of a baseline sweep. Open it to browse, close it to work.
      , case CRegistry.brandSlots st.input.registry def.meta.brand of
          Nothing -> HH.text ""
          Just slots ->
            let open = Array.elem def.meta.id st.expandedSlots
            in HH.div [ HP.class_ (H.ClassName "slot-grid-section") ]
                 ( [ HH.div
                       [ HP.class_ (H.ClassName "slot-grid-heading collapsible")
                       , HE.onClick \_ -> ToggleSlotGrid def.meta.id
                       ]
                       [ HH.span [ HP.class_ (H.ClassName "collapse-arrow") ]
                           [ HH.text (if open then "\x25BE" else "\x25B8") ]
                       , HH.text (" Slots " <> show slots.range.start <> "\x2013"
                           <> show (slots.range.start + slots.range.count - 1))
                       ]
                   ]
                 <> if open then [ renderSlotGrid def slots presets ] else []
                 )
      ]

  -- | Audition a numbered slot, then keep it.
  -- |
  -- | The number field is free rather than bounded by the brand's slot grid,
  -- | because the grid says which slots *this app* manages and the good sound
  -- | may predate the app — Strymon's grid starts at 50, and there is nothing
  -- | wrong with liking 14. The only real bound is Program Change itself, and
  -- | `makeProgramNumber` enforces that.
  renderSlotRefForm :: PedalDef -> { pedalId :: PedalId, slotNumber :: Int, name :: String } -> H.ComponentHTML Action () m
  renderSlotRefForm def b =
    HH.div [ HP.class_ (H.ClassName "preset-slot-ref-form") ]
      [ HH.div [ HP.class_ (H.ClassName "slot-ref-row") ]
          [ HH.label_ [ HH.text "Slot" ]
          , HH.input
              [ HP.type_ HP.InputNumber
              , HP.class_ (H.ClassName "slot-ref-number")
              , HP.value (show b.slotNumber)
              , HE.onValueInput SetRefSlot
              ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.class_ (H.ClassName "slot-ref-name")
              , HP.placeholder (defaultRefName def b.slotNumber)
              , HP.value b.name
              , HE.onValueInput SetRefName
              ]
          , HH.button
              [ HE.onClick \_ -> AuditionRef ]
              [ HH.text "Audition" ]
          , HH.button
              [ HP.class_ (H.ClassName "save-confirm")
              , HE.onClick \_ -> CommitSlotRef
              ]
              [ HH.text "Add to Library" ]
          , HH.button
              [ HE.onClick \_ -> CancelSlotRef ]
              [ HH.text "Cancel" ]
          ]
      , HH.div [ HP.class_ (H.ClassName "slot-ref-hint") ]
          [ HH.text "Recalls by Program Change. No knob values are captured — the sound stays in the pedal, and this is just the number that fetches it." ]
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
          ( [ HH.button
                [ HP.class_ (H.ClassName "recall-btn")
                , HE.onClick \_ -> ClickRecall preset
                ]
                [ HH.text "Recall" ]
            ]
          -- A slot reference has no values, so both of these would lie about
          -- it: Overwrite would silently turn it into a captured preset holding
          -- whatever happens to be dialled in, and Assign Slot would walk you
          -- through saving a sound to the pedal that the app never had.
          -- Parenthesised: `<>` is infixr, so an unbracketed `if` here would
          -- swallow everything after it into the `else` branch and a reference
          -- would lose Export and Delete too.
          <> ( if Preset.isSlotRef preset then [] else
                 [ HH.button
                     [ HE.onClick \_ -> ClickOverwrite preset ]
                     [ HH.text "Overwrite" ]
                 , HH.button
                     [ HE.onClick \_ -> StartAssignSlot preset.id ]
                     [ HH.text "Assign Slot" ]
                 ]
             )
          <>
          [ HH.button
              [ HE.onClick \_ -> ClickExport preset ]
              [ HH.text "Export" ]
          , HH.button
              [ HP.class_ (H.ClassName "delete-btn")
              , HE.onClick \_ -> ClickDelete preset.id
              ]
              [ HH.text "Delete" ]
          ] )
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

  -- | Three states, and the difference matters when you are building a board:
  -- | LIB has values but no slot, so it recalls by streaming CCs and cannot be
  -- | put on an MC6 switch; "Slot N" has both; REF has only the number.
  renderSlotBadge :: PedalPreset -> H.ComponentHTML Action () m
  renderSlotBadge preset
    | Preset.isSlotRef preset =
        HH.span
          [ HP.class_ (H.ClassName "preset-slot-badge ref")
          , HP.attr (HH.AttrName "style") (badgeColor preset.pedalId)
          ]
          [ HH.text (case preset.savedSlot of
              Just slot -> "REF " <> show (unProgramNumber slot)
              Nothing -> "REF") ]
    | otherwise = case preset.savedSlot of
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

  -- | Every slot a Program Change can reach on this pedal, not just the ones we
  -- | put things in. The factory presets are the reason to browse at all.
  renderSlotGrid :: PedalDef -> BrandSlots -> Array PedalPreset -> H.ComponentHTML Action () m
  renderSlotGrid def slots presets =
    HH.div_
      [ HH.div [ HP.class_ (H.ClassName "slot-grid") ]
          (Array.mapMaybe renderSlotBtn (Array.range range.start (range.start + range.count - 1)))
      , case slots.managed of
          Nothing -> HH.text ""
          Just m ->
            HH.div [ HP.class_ (H.ClassName "slot-grid-legend") ]
              [ HH.span [ HP.class_ (H.ClassName "slot-legend-swatch") ] []
              , HH.text (show m.start <> "\x2013" <> show (m.start + m.count - 1)
                  <> " is where this app saves. Everything else is the pedal's own — audition freely, but saving there overwrites it.")
              ]
      ]
    where
    range = slots.range
    assignedSlots = Array.mapMaybe _.savedSlot presets

    inManaged n = case slots.managed of
      Nothing -> false
      Just m -> n >= m.start && n < m.start + m.count

    renderSlotBtn n = do
      pn <- makeProgramNumber n
      let isAssigned = Array.elem pn assignedSlots
          cls = "slot-btn"
            <> (if isAssigned then " assigned" else "")
            <> (if inManaged n then " managed" else "")
      pure $ HH.button
        [ HP.class_ (H.ClassName cls)
        , HE.onClick \_ -> PickSlot def.meta.id n
        ]
        [ HH.text (show n) ]

  formatDate :: String -> String
  formatDate iso = SCU.take 10 iso

-- | What a slot reference is called when you do not name it. The number is the
-- | only thing we know for certain, so it is the thing in the name.
defaultRefName :: PedalDef -> Int -> String
defaultRefName def n = def.meta.name <> " " <> show n


handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  FocusPedal pid -> H.raise (PedalFocused pid)
  OpenPedal pid -> H.raise (PedalClicked pid)
  OpenPedalView pid -> H.raise (PedalViewClicked pid)
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
  -- Saving, unlike browsing, opens inside the managed region — that convention
  -- exists precisely so a save never lands on a factory preset by default.
  StartAssignSlot presetId -> do
    st <- H.get
    let start = fromMaybe 0 do
          preset <- Array.find (\p -> p.id == presetId) st.input.presets
          def <- CRegistry.findPedal st.input.registry preset.pedalId
          slots <- CRegistry.brandSlots st.input.registry def.meta.brand
          pure (case slots.managed of
            Just m -> m.start
            Nothing -> slots.range.start)
    H.modify_ _ { assigningSlot = Just { presetId, phase: EnterSlot, slotNumber: start } }
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

  -- Slot browsing
  OpenSlotBrowser pid -> do
    st <- H.get
    -- Open on the first slot the brand actually has, so the field is never
    -- sitting on a number the pedal will not answer to.
    let start = case CRegistry.findPedal st.input.registry pid of
          Just def -> case CRegistry.brandSlots st.input.registry def.meta.brand of
            Just slots -> slots.range.start
            Nothing -> 0
          Nothing -> 0
    H.modify_ _ { browsingSlot = Just { pedalId: pid, slotNumber: start, name: "" } }

  -- Clicking the grid loads the number *and* auditions it, because finding out
  -- what is in a slot is the whole reason to click it.
  PickSlot pid n -> do
    H.modify_ \st -> st
      { browsingSlot = Just
          { pedalId: pid
          , slotNumber: n
          , name: case st.browsingSlot of
              Just b | b.pedalId == pid -> b.name
              _ -> ""
          }
      }
    for_ (makeProgramNumber n) \pn -> H.raise (SendPC pid pn)

  SetRefSlot s ->
    for_ (Int.fromString s) \n ->
      H.modify_ \st -> st { browsingSlot = map (_ { slotNumber = n }) st.browsingSlot }

  SetRefName s ->
    H.modify_ \st -> st { browsingSlot = map (_ { name = s }) st.browsingSlot }

  AuditionRef -> do
    st <- H.get
    for_ st.browsingSlot \b ->
      for_ (makeProgramNumber b.slotNumber) \pn -> H.raise (SendPC b.pedalId pn)

  CommitSlotRef -> do
    st <- H.get
    for_ st.browsingSlot \b ->
      -- A number outside 0-127 is not a slot Program Change can reach, so the
      -- form stays open rather than saving a reference that can never fire.
      for_ (makeProgramNumber b.slotNumber) \pn -> do
        let fallback = case CRegistry.findPedal st.input.registry b.pedalId of
              Just def -> defaultRefName def b.slotNumber
              Nothing -> "Slot " <> show b.slotNumber
        H.raise (SaveSlotRef
          { pedalId: b.pedalId
          , slot: pn
          , name: if SCU.length (trim b.name) == 0 then fallback else trim b.name
          })
        H.modify_ _ { browsingSlot = Nothing }

  CancelSlotRef ->
    H.modify_ _ { browsingSlot = Nothing }

  ToggleSlotGrid pid -> H.modify_ \st ->
    st { expandedSlots =
      if Array.elem pid st.expandedSlots
        then Array.filter (_ /= pid) st.expandedSlots
        else Array.snoc st.expandedSlots pid
    }

  -- Confirmed, because it overwrites whatever the pedal is currently doing —
  -- including a sound dialled in by hand that was never saved.
  ClickBaseline def -> do
    ok <- liftEffect $ FileIO.confirm
      ("Sweep " <> def.meta.name <> " back to baseline? This sends all "
        <> show (Map.size def.baseline)
        <> " CCs and replaces the pedal's current settings.")
    when ok $ H.raise (BaselinePedal def.meta.id)
