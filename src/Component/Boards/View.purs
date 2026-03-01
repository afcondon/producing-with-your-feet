module Component.Boards.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Color (toHexString)
import Data.Array as Array
import Data.Const (Const)
import Data.Int as Int
import Data.Foldable (any)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, ProgramNumber, unsafeMidiValue, unProgramNumber)
import Data.Pedal (PedalDef, PedalId)
import Data.Pedal.Engage (EngageConfig(..), EngageState(..))
import Data.Preset (BoardPreset, BoardPresetEntry, PedalPreset, PresetId)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, MC6Assignment, MidiConnections)
import Foreign.FileIO as FileIO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Data.MC6.Types (MC6NativeBank)

type Input =
  { engine :: EngineState
  , connections :: MidiConnections
  , presets :: Array PedalPreset
  , boardPresets :: Array BoardPreset
  , registry :: PedalRegistry
  , mc6ActiveBank :: Maybe MC6NativeBank
  , mc6Assignments :: Array MC6Assignment
  }

data Output
  = RecallBoard BoardPreset
  | SendEngageAudition PedalId EngageState
  | SendPCAudition PedalId ProgramNumber
  | RecallPresetAudition PedalPreset
  | ValueChanged PedalId CC MidiValue
  | SaveBoard { name :: String, notes :: String, pedals :: Map.Map PedalId BoardPresetEntry }
  | UpdateBoard PresetId { name :: String, notes :: String }
  | OverwriteBoard PresetId (Map.Map PedalId BoardPresetEntry)
  | DeleteBoard PresetId
  | ExportBoard BoardPreset
  | ImportBoards (Array BoardPreset)
  | FocusPedal PedalId
  | SendEngageAll EngageState
  | AssignBoardToSwitch PresetId Int
  | UnassignBoard PresetId

type GridEntry =
  { engage :: EngageState
  , selectedPresetId :: Maybe PresetId
  }

type State =
  { input :: Input
  , grid :: Map.Map PedalId GridEntry
  , recalling :: Maybe PresetId
  , showSaveForm :: Boolean
  , saveName :: String
  , saveNotes :: String
  , editingBoard :: Maybe PresetId
  , editName :: String
  , editNotes :: String
  }

data Action
  = Receive Input
  | ChangeEngage PedalId String
  | ChangePreset PedalId String
  | AllOn
  | AllOff
  | AllNoChange
  | ClickRecallBoard PresetId
  | ClickLoadBoard PresetId
  -- Board CRUD
  | ShowBoardSaveForm
  | CancelBoardSaveForm
  | UpdateBoardSaveName String
  | UpdateBoardSaveNotes String
  | CommitBoardSave
  | StartEditBoard PresetId
  | CancelEditBoard
  | UpdateEditName String
  | UpdateEditNotes String
  | CommitEditBoard PresetId
  | ClickOverwriteBoard PresetId
  | ClickDeleteBoard PresetId
  | ClickExportBoard BoardPreset
  | AssignToSwitch PresetId String

type Slot = H.Slot (Const Void) Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState i =
  { input: i
  , grid: Map.fromFoldable $ map (\def -> Tuple def.meta.id { engage: EngageNoChange, selectedPresetId: Nothing }) (CRegistry.registryPedals i.registry)
  , recalling: Nothing
  , showSaveForm: false
  , saveName: ""
  , saveNotes: ""
  , editingBoard: Nothing
  , editName: ""
  , editNotes: ""
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "boards-view") ]
    [ renderBuilderGrid state
    , renderBoardList state
    ]

renderBuilderGrid :: forall m. State -> H.ComponentHTML Action () m
renderBuilderGrid state =
  HH.div_
    [ HH.div [ HP.class_ (H.ClassName "boards-header") ]
        [ HH.span [ HP.class_ (H.ClassName "boards-heading") ] [ HH.text "Boards" ]
        , HH.div [ HP.class_ (H.ClassName "boards-actions") ]
            [ HH.button [ HE.onClick \_ -> AllOn ] [ HH.text "All On" ]
            , HH.button [ HE.onClick \_ -> AllOff ] [ HH.text "All Off" ]
            , HH.button [ HE.onClick \_ -> AllNoChange ] [ HH.text "All \x2014\x2014" ]
            , HH.button [ HE.onClick \_ -> ShowBoardSaveForm ] [ HH.text "Save New" ]
            ]
        ]
    , if state.showSaveForm
        then renderBoardSaveForm state
        else HH.text ""
    , HH.div [ HP.class_ (H.ClassName "boards-pedal-grid") ]
        (map (renderPedalRow state) (CRegistry.registryPedals state.input.registry))
    ]

renderBoardSaveForm :: forall m. State -> H.ComponentHTML Action () m
renderBoardSaveForm state =
  HH.div [ HP.class_ (H.ClassName "preset-save-form") ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Board name"
        , HP.value state.saveName
        , HE.onValueInput UpdateBoardSaveName
        ]
    , HH.textarea
        [ HP.placeholder "Notes (optional)"
        , HP.value state.saveNotes
        , HE.onValueInput UpdateBoardSaveNotes
        ]
    , HH.div [ HP.class_ (H.ClassName "preset-form-buttons") ]
        [ HH.button
            [ HP.class_ (H.ClassName "save-confirm")
            , HE.onClick \_ -> CommitBoardSave
            ]
            [ HH.text "Save to Library" ]
        , HH.button
            [ HE.onClick \_ -> CancelBoardSaveForm ]
            [ HH.text "Cancel" ]
        ]
    ]

renderPedalRow :: forall m. State -> PedalDef -> H.ComponentHTML Action () m
renderPedalRow state def =
  HH.div [ HP.class_ (H.ClassName "boards-pedal-row") ]
    [ HH.span
        [ HP.class_ (H.ClassName "boards-pedal-name")
        , HP.attr (HH.AttrName "style") (case def.meta.color of
            Just c -> "color: " <> toHexString c
            Nothing -> "")
        ]
        [ HH.text def.meta.name ]
    , renderEngageSelect def.meta.id def.engage currentEntry.engage
    , renderPresetSelect def.meta.id currentEntry.selectedPresetId pedalPresets state.input.engine
    ]
  where
  currentEntry = fromMaybe { engage: EngageNoChange, selectedPresetId: Nothing }
    (Map.lookup def.meta.id state.grid)
  pedalPresets = Array.filter (\p -> p.pedalId == def.meta.id) state.input.presets

renderEngageSelect :: forall m. PedalId -> EngageConfig -> EngageState -> H.ComponentHTML Action () m
renderEngageSelect pid engageCfg currentState =
  HH.select
    [ HP.class_ (H.ClassName "boards-engage-select")
    , HP.value (engageToValue currentState)
    , HE.onValueChange (ChangeEngage pid)
    ]
    (engageOptions engageCfg)

engageOptions :: forall m. EngageConfig -> Array (H.ComponentHTML Action () m)
engageOptions = case _ of
  SingleEngage _ ->
    [ HH.option [ HP.value "no-change" ] [ HH.text "\x2014\x2014" ]
    , HH.option [ HP.value "on" ] [ HH.text "On" ]
    , HH.option [ HP.value "off" ] [ HH.text "Off" ]
    ]
  DualEngage { a, b } ->
    [ HH.option [ HP.value "no-change" ] [ HH.text "\x2014\x2014" ]
    , HH.option [ HP.value "on" ] [ HH.text "Both" ]
    , HH.option [ HP.value "a" ] [ HH.text a.label ]
    , HH.option [ HP.value "b" ] [ HH.text b.label ]
    , HH.option [ HP.value "off" ] [ HH.text "Off" ]
    ]

engageToValue :: EngageState -> String
engageToValue = case _ of
  EngageOn -> "on"
  EngageOff -> "off"
  EngageA -> "a"
  EngageB -> "b"
  EngageNoChange -> "no-change"

valueToEngage :: String -> EngageState
valueToEngage = case _ of
  "on" -> EngageOn
  "off" -> EngageOff
  "a" -> EngageA
  "b" -> EngageB
  _ -> EngageNoChange

renderPresetSelect :: forall m. PedalId -> Maybe PresetId -> Array PedalPreset -> EngineState -> H.ComponentHTML Action () m
renderPresetSelect pid selectedId presets engine =
  HH.select
    [ HP.class_ (H.ClassName "boards-preset-select")
    , HP.value (fromMaybe "" selectedId)
    , HE.onValueChange (ChangePreset pid)
    ]
    ( [ HH.option [ HP.value "" ] [ HH.text "No change" ] ]
      <> map presetOption presets
    )
  where
  presetOption p =
    let slotLabel = case p.savedSlot of
          Just slot -> " [" <> show (unProgramNumber slot) <> "]"
          Nothing -> " [LIB]"
        modified = selectedId == Just p.id && isPresetModified engine p
        prefix = if modified then "* " else ""
    in HH.option [ HP.value p.id ] [ HH.text (prefix <> p.name <> slotLabel) ]

renderBoardList :: forall m. State -> H.ComponentHTML Action () m
renderBoardList state =
  HH.div [ HP.class_ (H.ClassName "boards-list") ]
    [ HH.div [ HP.class_ (H.ClassName "boards-header") ]
        [ HH.span [ HP.class_ (H.ClassName "boards-heading") ] [ HH.text "Saved Boards" ] ]
    , if Array.null state.input.boardPresets
        then HH.div [ HP.class_ (H.ClassName "boards-empty") ] [ HH.text "No boards saved" ]
        else HH.div_ (map (renderBoardItem state) state.input.boardPresets)
    ]

renderBoardItem :: forall m. State -> BoardPreset -> H.ComponentHTML Action () m
renderBoardItem state bp =
  HH.div [ HP.class_ (H.ClassName "boards-item") ]
    [ case state.editingBoard of
        Just eid | eid == bp.id -> renderEditForm state bp
        _ -> renderBoardItemNormal state bp
    ]

renderBoardItemNormal :: forall m. State -> BoardPreset -> H.ComponentHTML Action () m
renderBoardItemNormal state bp =
  let assignedSwitch = Array.findMap (\a ->
        if a.boardPresetId == bp.id then Just a.switchIndex else Nothing
        ) state.input.mc6Assignments
  in HH.div_
    [ HH.div [ HP.class_ (H.ClassName "boards-item-header") ]
        [ HH.span [ HP.class_ (H.ClassName "boards-item-name") ] [ HH.text bp.name ]
        , case assignedSwitch of
            Just idx -> HH.span [ HP.class_ (H.ClassName "boards-item-assigned-badge") ]
              [ HH.text (switchLetter idx) ]
            Nothing -> HH.text ""
        , HH.span [ HP.class_ (H.ClassName "boards-item-date") ] [ HH.text (SCU.take 10 bp.modified) ]
        ]
    , if bp.notes /= ""
        then HH.div [ HP.class_ (H.ClassName "preset-description") ] [ HH.text bp.notes ]
        else HH.text ""
    , HH.div [ HP.class_ (H.ClassName "preset-description") ]
        [ HH.text (boardSummary state.input.registry bp state.input.presets) ]
    , HH.div [ HP.class_ (H.ClassName "boards-item-actions") ]
        [ HH.button
            [ HP.class_ (H.ClassName "recall-btn")
            , HE.onClick \_ -> ClickRecallBoard bp.id
            ]
            [ HH.text (if state.recalling == Just bp.id then "Sending..." else "Recall") ]
        , HH.button
            [ HE.onClick \_ -> ClickLoadBoard bp.id ]
            [ HH.text "Load" ]
        , HH.button
            [ HE.onClick \_ -> StartEditBoard bp.id ]
            [ HH.text "Edit" ]
        , HH.button
            [ HE.onClick \_ -> ClickOverwriteBoard bp.id ]
            [ HH.text "Overwrite" ]
        , HH.button
            [ HE.onClick \_ -> ClickExportBoard bp ]
            [ HH.text "Export" ]
        , HH.button
            [ HP.class_ (H.ClassName "delete-btn")
            , HE.onClick \_ -> ClickDeleteBoard bp.id
            ]
            [ HH.text "Delete" ]
        ]
    , renderAssignDropdown state bp
    ]

renderEditForm :: forall m. State -> BoardPreset -> H.ComponentHTML Action () m
renderEditForm state bp =
  HH.div [ HP.class_ (H.ClassName "preset-save-form") ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Board name"
        , HP.value state.editName
        , HE.onValueInput UpdateEditName
        ]
    , HH.textarea
        [ HP.placeholder "Notes (optional)"
        , HP.value state.editNotes
        , HE.onValueInput UpdateEditNotes
        ]
    , HH.div [ HP.class_ (H.ClassName "preset-form-buttons") ]
        [ HH.button
            [ HP.class_ (H.ClassName "save-confirm")
            , HE.onClick \_ -> CommitEditBoard bp.id
            ]
            [ HH.text "Save" ]
        , HH.button
            [ HE.onClick \_ -> CancelEditBoard ]
            [ HH.text "Cancel" ]
        ]
    ]

switchLetter :: Int -> String
switchLetter = case _ of
  0 -> "A"
  1 -> "B"
  2 -> "C"
  3 -> "D"
  4 -> "E"
  5 -> "F"
  6 -> "G"
  7 -> "H"
  8 -> "I"
  _ -> "?"

renderAssignDropdown :: forall m. State -> BoardPreset -> H.ComponentHTML Action () m
renderAssignDropdown state bp =
  case state.input.mc6ActiveBank of
    Nothing -> HH.text ""
    Just bank ->
      let currentAssignment = Array.find (\a -> a.boardPresetId == bp.id) state.input.mc6Assignments
          currentVal = case currentAssignment of
            Just a -> show a.switchIndex
            Nothing -> ""
      in HH.div [ HP.class_ (H.ClassName "boards-item-assign") ]
        [ HH.span [ HP.class_ (H.ClassName "boards-item-assign-label") ] [ HH.text "MC6:" ]
        , HH.select
            [ HP.class_ (H.ClassName "boards-assign-select")
            , HP.value currentVal
            , HE.onValueChange (AssignToSwitch bp.id)
            ]
            ( [ HH.option [ HP.value "" ] [ HH.text "\x2014" ] ]
              <> Array.mapWithIndex (\idx _ ->
                  let letter = switchLetter idx
                      occupant = case Array.find (\a -> a.bankNumber == bank.bankNumber && a.switchIndex == idx) state.input.mc6Assignments of
                        Just a | a.boardPresetId /= bp.id ->
                          case Array.find (\b -> b.id == a.boardPresetId) state.input.boardPresets of
                            Just b -> " (" <> b.name <> ")"
                            Nothing -> " (assigned)"
                        _ -> case Array.index bank.presets idx of
                          Just p | p.shortName /= "" -> " (" <> p.shortName <> ")"
                          _ -> ""
                  in HH.option [ HP.value (show idx) ] [ HH.text (letter <> occupant) ]
                ) (Array.replicate 9 unit)
            )
        ]

boardSummary :: PedalRegistry -> BoardPreset -> Array PedalPreset -> String
boardSummary reg bp allPresets =
  let entries = Map.toUnfoldable bp.pedals :: Array (Tuple PedalId BoardPresetEntry)
      parts = Array.mapMaybe summarizeEntry entries
  in if Array.null parts then "Empty" else Array.intercalate "  " parts
  where
  summarizeEntry (Tuple pid entry) = case entry.engage of
    EngageNoChange -> Nothing
    EngageOff -> do
      def <- CRegistry.findPedal reg pid
      Just (def.meta.name <> " off")
    _ -> do
      def <- CRegistry.findPedal reg pid
      let presetLabel = case entry.presetId of
            Just presetId -> case Array.find (\p -> p.id == presetId) allPresets of
              Just preset -> case preset.savedSlot of
                Just slot -> "[" <> show (unProgramNumber slot) <> "]"
                Nothing -> "[LIB]"
              Nothing -> ""
            Nothing -> ""
      Just (def.meta.name <> presetLabel)

-- Build BoardPresetEntry map from current grid state
captureGrid :: Map.Map PedalId GridEntry -> Map.Map PedalId BoardPresetEntry
captureGrid grid =
  map (\entry -> { presetId: entry.selectedPresetId, engage: entry.engage }) grid

-- | Infer engage state from engine CC values for a pedal
inferEngageState :: PedalDef -> Map.Map CC MidiValue -> EngageState
inferEngageState def vals = case def.engage of
  SingleEngage cc ->
    if Map.lookup cc vals > Just (unsafeMidiValue 63) then EngageOn else EngageOff
  DualEngage { a, b } ->
    let aOn = Map.lookup a.cc vals > Just (unsafeMidiValue 63)
        bOn = Map.lookup b.cc vals > Just (unsafeMidiValue 63)
    in case aOn, bOn of
         true, true   -> EngageOn
         true, false  -> EngageA
         false, true  -> EngageB
         false, false -> EngageOff

-- | Check if engine state has diverged from a preset's stored values
isPresetModified :: EngineState -> PedalPreset -> Boolean
isPresetModified engine preset =
  case Map.lookup preset.pedalId engine of
    Nothing -> false
    Just ps ->
      let presetEntries = Map.toUnfoldable preset.values :: Array (Tuple CC MidiValue)
      in any (\(Tuple cc val) -> Map.lookup cc ps.values /= Just val) presetEntries

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> do
    st <- H.get
    -- Sync engage state from engine for each pedal
    let updatedGrid = Array.foldl (\g def ->
          let pid = def.meta.id
          in case Map.lookup pid input.engine of
               Nothing -> g
               Just ps -> Map.update (\e -> Just e { engage = inferEngageState def ps.values }) pid g
          ) st.grid (CRegistry.registryPedals input.registry)
    H.modify_ _ { input = input, grid = updatedGrid }

  ChangeEngage pid valStr -> do
    let eng = valueToEngage valStr
    H.modify_ \st -> st { grid = Map.update (\e -> Just e { engage = eng }) pid st.grid }
    H.raise (SendEngageAudition pid eng)

  ChangePreset pid valStr -> do
    let mPresetId = if valStr == "" then Nothing else Just valStr
    H.modify_ \st -> st { grid = Map.update (\e -> Just e { selectedPresetId = mPresetId }) pid st.grid }
    -- Live audition: send PC if preset has a saved slot, otherwise stream CCs
    case mPresetId of
      Nothing -> pure unit
      Just presetId -> do
        st <- H.get
        case Array.find (\p -> p.id == presetId) st.input.presets of
          Just preset -> do
            case preset.savedSlot of
              Just slot -> H.raise (SendPCAudition pid slot)
              Nothing -> H.raise (RecallPresetAudition preset)
            H.raise (FocusPedal pid)
          Nothing -> pure unit

  AllOn ->
    H.raise (SendEngageAll EngageOn)

  AllOff ->
    H.raise (SendEngageAll EngageOff)

  AllNoChange ->
    H.modify_ \st -> st { grid = map (\e -> e { engage = EngageNoChange, selectedPresetId = Nothing }) st.grid }

  ClickRecallBoard presetId -> do
    st <- H.get
    case Array.find (\bp -> bp.id == presetId) st.input.boardPresets of
      Nothing -> pure unit
      Just bp -> do
        let entries = Map.toUnfoldable bp.pedals :: Array (Tuple PedalId BoardPresetEntry)
            newGrid = Map.fromFoldable $ map (\(Tuple pid entry) ->
              Tuple pid { engage: entry.engage, selectedPresetId: entry.presetId }
            ) entries
        H.modify_ \s -> s { grid = Map.union newGrid s.grid, recalling = Just presetId }
        H.raise (RecallBoard bp)
        H.modify_ _ { recalling = Nothing }

  ClickLoadBoard presetId -> do
    st <- H.get
    case Array.find (\bp -> bp.id == presetId) st.input.boardPresets of
      Nothing -> pure unit
      Just bp -> do
        let entries = Map.toUnfoldable bp.pedals :: Array (Tuple PedalId BoardPresetEntry)
            newGrid = Map.fromFoldable $ map (\(Tuple pid entry) ->
              Tuple pid { engage: entry.engage, selectedPresetId: entry.presetId }
            ) entries
        H.modify_ \s -> s { grid = Map.union newGrid s.grid }

  -- Board save form
  ShowBoardSaveForm ->
    H.modify_ _ { showSaveForm = true, saveName = "", saveNotes = "" }
  CancelBoardSaveForm ->
    H.modify_ _ { showSaveForm = false }
  UpdateBoardSaveName s -> H.modify_ _ { saveName = s }
  UpdateBoardSaveNotes s -> H.modify_ _ { saveNotes = s }
  CommitBoardSave -> do
    st <- H.get
    when (st.saveName /= "") do
      H.raise (SaveBoard { name: st.saveName, notes: st.saveNotes, pedals: captureGrid st.grid })
      H.modify_ _ { showSaveForm = false }

  -- Edit board
  StartEditBoard presetId -> do
    st <- H.get
    case Array.find (\bp -> bp.id == presetId) st.input.boardPresets of
      Nothing -> pure unit
      Just bp ->
        H.modify_ _ { editingBoard = Just presetId, editName = bp.name, editNotes = bp.notes }
  CancelEditBoard ->
    H.modify_ _ { editingBoard = Nothing }
  UpdateEditName s -> H.modify_ _ { editName = s }
  UpdateEditNotes s -> H.modify_ _ { editNotes = s }
  CommitEditBoard presetId -> do
    st <- H.get
    when (st.editName /= "") do
      H.raise (UpdateBoard presetId { name: st.editName, notes: st.editNotes })
      H.modify_ _ { editingBoard = Nothing }

  -- Overwrite / Delete / Export
  ClickOverwriteBoard presetId -> do
    ok <- liftEffect $ FileIO.confirm "Overwrite this board with current grid state?"
    when ok do
      st <- H.get
      H.raise (OverwriteBoard presetId (captureGrid st.grid))

  ClickDeleteBoard presetId -> do
    ok <- liftEffect $ FileIO.confirm "Delete this board?"
    when ok $ H.raise (DeleteBoard presetId)

  ClickExportBoard bp -> H.raise (ExportBoard bp)

  AssignToSwitch boardId valStr -> do
    case Int.fromString valStr of
      Just switchIdx -> H.raise (AssignBoardToSwitch boardId switchIdx)
      Nothing -> H.raise (UnassignBoard boardId)
