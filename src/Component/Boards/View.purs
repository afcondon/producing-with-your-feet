module Component.Boards.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Data.MC6.BankNumber as BankNumber
import Color (toHexString)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (unProgramNumber)
import Data.Pedal (PedalDef, PedalId)
import Data.Pedal.Engage (EngageConfig(..), EngageState(..))
import Data.Preset (BoardPreset, BoardPresetEntry, PedalPreset, PresetId)
import Data.Preset.Usage (Chip(..), boardNow, chipFor, chipText, isPresetModified)
import Data.String as String
import Data.String.CodeUnits as SCU
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, MC6Assignment)
import Foreign.FileIO as FileIO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Helm.MC6.ControlBank (switchLetter)
import Data.MC6.Board as Board

-- | **Area 3 of the Presets page: board presets** (plan `pwyf-layout.md`).
-- |
-- | Each board is a row of chips, one per pedal: its preset, On/Off, or "no
-- | change". A chip is the link to the other two areas: clicking one selects
-- | that pedal there. Editing a board is changing its chips in place, which
-- | replaced the separate builder grid (one row per pedal with engage and
-- | preset menus): those two menus are now what a chip opens.
type Input =
  { engine :: EngineState
  , presets :: Array PedalPreset
  , boardPresets :: Array BoardPreset
  , registry :: PedalRegistry
  -- | Chip order: the pedals as the cards and pills order them.
  , cardOrder :: Array PedalId
  -- | The preset each pedal was last recalled from, for "what every pedal is
  -- | now" (`Data.Preset.Usage.boardNow`).
  , recalledPresets :: Map.Map PedalId PresetId
  -- | The pedal areas 1 and 2 show, so its chips stand out in every row.
  , selectedPedal :: Maybe PedalId
  , mc6Assignments :: Array MC6Assignment
  -- | The control bank a board switch long-presses into, if any. Needed here
  -- | only because it adds one message to the compiled board, and a budget
  -- | that ignored it would be wrong by exactly the amount that matters.
  , controlBankNum :: Maybe Int
  }

data Output
  = RecallBoard BoardPreset
  | SendEngageAudition PedalId EngageState
  | AuditionPreset PedalPreset
  | SaveBoard { name :: String, notes :: String, pedals :: Map.Map PedalId BoardPresetEntry }
  | UpdateBoard PresetId { name :: String, notes :: String, pedals :: Map.Map PedalId BoardPresetEntry }
  | OverwriteBoard PresetId (Map.Map PedalId BoardPresetEntry)
  | DeleteBoard PresetId
  | ExportBoard BoardPreset
  | SelectPedal PedalId
  | SendEngageAll EngageState

-- | A board being written: a new one (from "Save the board as…") or an
-- | existing one being edited. Both are the same row of chips.
data DraftOf = NewBoard | ExistingBoard PresetId

derive instance Eq DraftOf

type Draft =
  { target :: DraftOf
  , name :: String
  , notes :: String
  , pedals :: Map.Map PedalId BoardPresetEntry
  }

type State =
  { input :: Input
  , draft :: Maybe Draft
  -- | The chip whose menu is open, in the draft row. One at a time.
  , chipMenu :: Maybe PedalId
  , recalling :: Maybe PresetId
  }

data Action
  = Receive Input
  | ClickChip PedalId
  | ChangeEngage PedalId String
  | ChangePreset PedalId String
  | AllOn
  | AllOff
  | AllNoChange
  | ClickLoadBoard PresetId
  | StartNewBoard
  | StartEditBoard PresetId
  | CancelDraft
  | UpdateDraftName String
  | UpdateDraftNotes String
  | CommitDraft
  | CommitDraftAsNew
  | ClickOverwriteBoard PresetId
  | ClickDeleteBoard PresetId
  | ClickExportBoard BoardPreset

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
  , draft: Nothing
  , chipMenu: Nothing
  , recalling: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "boards-view") ]
    [ HH.div [ HP.class_ (H.ClassName "presets-area-heading") ]
        [ HH.span [ HP.class_ (H.ClassName "presets-area-number") ] [ HH.text "3" ]
        , HH.text "Board presets"
        ]
    , case state.draft of
        Just draft | draft.target == NewBoard -> renderDraft state draft
        _ -> HH.text ""
    , if Array.null state.input.boardPresets
        then HH.div [ HP.class_ (H.ClassName "boards-empty") ] [ HH.text "No boards saved" ]
        else HH.div [ HP.class_ (H.ClassName "boards-list") ]
          (map (renderBoard state) state.input.boardPresets)
    , case state.draft of
        Just draft | draft.target == NewBoard -> HH.text ""
        _ ->
          HH.button
            [ HP.class_ (H.ClassName "boards-save-as")
            , HP.title "Keep what every pedal is now: its engage state and the preset it was recalled from."
            , HE.onClick \_ -> StartNewBoard
            ]
            [ HH.text "Save the board as\x2026" ]
    ]

-- | The pedals a chip row shows, in card order. A pedal the registry has lost
-- | (a stored order can outlive a pedal) has no chip.
chipPedals :: State -> Array PedalDef
chipPedals state =
  Array.mapMaybe (CRegistry.findPedal state.input.registry) state.input.cardOrder

renderBoard :: forall m. State -> BoardPreset -> H.ComponentHTML Action () m
renderBoard state board = case state.draft of
  Just draft | draft.target == ExistingBoard board.id -> renderDraft state draft
  _ ->
    -- Read-only: assignment is made from the MC6 page, where you are looking
    -- at the switch you are filling. This just says where the sound ended up.
    let assignedPlaces = Array.filter (\a -> a.boardPresetId == board.id) state.input.mc6Assignments
    in HH.div [ HP.class_ (H.ClassName "boards-item") ]
      [ HH.div [ HP.class_ (H.ClassName "boards-item-header") ]
          [ HH.span [ HP.class_ (H.ClassName "boards-item-name") ] [ HH.text board.name ]
          , if Array.null assignedPlaces then HH.text "" else
              HH.span [ HP.class_ (H.ClassName "boards-item-assigned-badge") ]
                [ HH.text (String.joinWith " "
                    (map (\a -> BankNumber.shown a.bankNumber <> switchLetter a.switchIndex) assignedPlaces)) ]
          , HH.span [ HP.class_ (H.ClassName "boards-item-date") ] [ HH.text (SCU.take 10 board.modified) ]
          , HH.button
              [ HP.class_ (H.ClassName "boards-load")
              , HP.title "Send this board to the pedals"
              , HE.onClick \_ -> ClickLoadBoard board.id
              ]
              [ HH.text (if state.recalling == Just board.id then "Sending\x2026" else "Load \x25B8") ]
          ]
      , renderChipRow state false board.pedals
      , renderBudget state board
      , if board.notes /= ""
          then HH.div [ HP.class_ (H.ClassName "preset-description") ] [ HH.text board.notes ]
          else HH.text ""
      , HH.div [ HP.class_ (H.ClassName "boards-item-actions") ]
          [ HH.button [ HE.onClick \_ -> StartEditBoard board.id ] [ HH.text "Edit" ]
          , HH.button
              [ HP.title "Replace this board with what every pedal is now"
              , HE.onClick \_ -> ClickOverwriteBoard board.id
              ]
              [ HH.text "Overwrite" ]
          , HH.button [ HE.onClick \_ -> ClickExportBoard board ] [ HH.text "Export" ]
          , HH.button
              [ HP.class_ (H.ClassName "delete-btn")
              , HE.onClick \_ -> ClickDeleteBoard board.id
              ]
              [ HH.text "Delete" ]
          ]
      ]

-- | One chip per pedal. In the draft row a chip also opens its menu: the
-- | engage and preset selects the builder grid had, one pedal at a time.
renderChipRow :: forall m. State -> Boolean -> Map.Map PedalId BoardPresetEntry -> H.ComponentHTML Action () m
renderChipRow state editing pedals =
  HH.div [ HP.class_ (H.ClassName "board-chips") ]
    (map renderChip (chipPedals state))
  where
  renderChip def =
    let
      pid = def.meta.id
      entry = Map.lookup pid pedals
      chip = chipFor state.input.presets entry
      menuOpen = editing && state.chipMenu == Just pid
      cls = "board-chip"
        <> chipClass chip
        <> (if state.input.selectedPedal == Just pid then " selected" else "")
        <> (if menuOpen then " open" else "")
    in
      HH.div [ HP.class_ (H.ClassName "board-chip-cell") ]
        [ HH.button
            [ HP.class_ (H.ClassName cls)
            , HP.title (def.meta.name <> ": " <> chipTitle def chip)
            , HE.onClick \_ -> ClickChip pid
            ]
            [ HH.span
                [ HP.class_ (H.ClassName "board-chip-pedal")
                , HP.attr (HH.AttrName "style") (case def.meta.color of
                    Just c -> "color: " <> toHexString c
                    Nothing -> "")
                ]
                [ HH.text def.meta.shortName ]
            , HH.span [ HP.class_ (H.ClassName "board-chip-value") ]
                [ HH.text (chipText def.engage chip) ]
            ]
        , if menuOpen then renderChipMenu state def entry else HH.text ""
        ]

  chipClass = case _ of
    ChipNoChange -> " no-change"
    ChipEngage EngageOff -> " off"
    ChipEngage _ -> " engage"
    ChipPreset _ EngageOff -> " preset off"
    ChipPreset _ _ -> " preset"
    ChipMissingPreset _ -> " missing"

  -- The hover says the whole entry, including the half the chip's one word
  -- leaves out (a preset recalled and then switched off).
  chipTitle def = case _ of
    ChipNoChange -> "no change"
    ChipEngage engage -> engageTitle def.engage engage
    ChipPreset preset engage ->
      "\x201C" <> preset.name <> "\x201D" <> slotLabel preset
        <> (if engage == EngageNoChange then "" else ", " <> engageTitle def.engage engage)
    ChipMissingPreset _ -> "names a preset that has been deleted"

  engageTitle config engage = chipText config (ChipEngage engage)

  slotLabel preset = case preset.savedSlot of
    Just slot -> " [" <> show (unProgramNumber slot) <> "]"
    Nothing -> " [LIB]"

renderChipMenu :: forall m. State -> PedalDef -> Maybe BoardPresetEntry -> H.ComponentHTML Action () m
renderChipMenu state def entry =
  HH.div [ HP.class_ (H.ClassName "board-chip-menu") ]
    [ renderEngageSelect def.meta.id def.engage current.engage
    , renderPresetSelect def.meta.id current.presetId pedalPresets state.input.engine
    ]
  where
  current = fromMaybe { engage: EngageNoChange, presetId: Nothing } entry
  pedalPresets = Array.filter (\p -> p.pedalId == def.meta.id) state.input.presets

-- | The board being written, in place of its row: name, notes, the chips (each
-- | opening its menu), the whole-row actions the builder grid had, and save.
renderDraft :: forall m. State -> Draft -> H.ComponentHTML Action () m
renderDraft state draft =
  HH.div [ HP.class_ (H.ClassName "boards-item editing") ]
    [ HH.div [ HP.class_ (H.ClassName "boards-draft-fields") ]
        [ HH.input
            [ HP.type_ HP.InputText
            , HP.class_ (H.ClassName "boards-draft-name")
            , HP.placeholder "Board name"
            , HP.value draft.name
            , HE.onValueInput UpdateDraftName
            ]
        , HH.textarea
            [ HP.placeholder "Notes (optional)"
            , HP.value draft.notes
            , HE.onValueInput UpdateDraftNotes
            ]
        ]
    , renderChipRow state true draft.pedals
    , renderBudgetOf state draft.pedals
    , HH.div [ HP.class_ (H.ClassName "boards-row-actions") ]
        [ HH.span [ HP.class_ (H.ClassName "boards-row-actions-label") ] [ HH.text "Every chip:" ]
        , HH.button
            [ HP.title "Set every pedal on, here and on the board"
            , HE.onClick \_ -> AllOn
            ]
            [ HH.text "All On" ]
        , HH.button
            [ HP.title "Set every pedal off, here and on the board"
            , HE.onClick \_ -> AllOff
            ]
            [ HH.text "All Off" ]
        , HH.button
            [ HP.title "Leave every pedal alone: start from nothing and add the pedals this board is about"
            , HE.onClick \_ -> AllNoChange
            ]
            [ HH.text "All \x2014\x2014" ]
        ]
    , HH.div [ HP.class_ (H.ClassName "preset-form-buttons") ]
        ( [ HH.button
              [ HP.class_ (H.ClassName "save-confirm")
              , HE.onClick \_ -> CommitDraft
              ]
              [ HH.text (case draft.target of
                  NewBoard -> "Save to Library"
                  ExistingBoard _ -> "Save") ]
          ]
        -- Editing a board and keeping the result as another is what the old
        -- page did with Load (into the builder) and Save New.
        <> ( case draft.target of
               ExistingBoard _ ->
                 [ HH.button [ HE.onClick \_ -> CommitDraftAsNew ] [ HH.text "Save as new board" ] ]
               NewBoard -> []
           )
        <> [ HH.button [ HE.onClick \_ -> CancelDraft ] [ HH.text "Cancel" ] ]
        )
    ]

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

-- | What this board costs on the MC6, out of the sixteen a preset can hold.
-- |
-- | Worth showing on every board rather than only when it overflows, because
-- | the number is not guessable: a dual-engage pedal costs two messages to
-- | bypass unless it declares a whole-pedal bypass, so two boards touching the
-- | same twelve pedals can differ by four. The count comes from compiling the
-- | board — the same function that sends it — so it cannot flatter.
renderBudget :: forall m. State -> BoardPreset -> H.ComponentHTML Action () m
renderBudget state board =
  HH.div
    [ HP.class_ (H.ClassName ("boards-item-budget" <> if over then " over" else ""))
    , HP.title (if over
        then "Over the MC6's 16-message limit: the last "
               <> show (n - Board.messageLimit)
               <> " would be dropped. Set a pedal to \x2014\x2014 to get back under."
        else "Compiles to " <> show n <> " of the MC6's 16 messages.")
    ]
    [ HH.text (show n <> " of " <> show Board.messageLimit <> " MC6 messages") ]
  where
  n = Board.boardMessageCount state.input.registry state.input.presets
        state.input.controlBankNum board
  over = n > Board.messageLimit

-- | The budget of a board still being written, compiled as if it were saved.
renderBudgetOf :: forall m. State -> Map.Map PedalId BoardPresetEntry -> H.ComponentHTML Action () m
renderBudgetOf state pedals =
  renderBudget state
    { id: "", name: "", description: "", notes: "", pedals, created: "", modified: "" }

modifyDraft :: forall m. (Draft -> Draft) -> H.HalogenM State Action () Output m Unit
modifyDraft f = H.modify_ \st -> st { draft = map f st.draft }

-- | Set one pedal's entry in the draft; a pedal not yet in it starts as "no
-- | change".
setEntry :: PedalId -> (BoardPresetEntry -> BoardPresetEntry) -> Draft -> Draft
setEntry pid f draft =
  let current = fromMaybe { engage: EngageNoChange, presetId: Nothing } (Map.lookup pid draft.pedals)
  in draft { pedals = Map.insert pid (f current) draft.pedals }

-- | Every chip the row shows, set alike.
setEvery :: Array PedalId -> BoardPresetEntry -> Draft -> Draft
setEvery pids entry draft =
  draft { pedals = Map.union (Map.fromFoldable (map (\pid -> Tuple pid entry) pids)) draft.pedals }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }

  -- A chip always selects its pedal in areas 1 and 2. In the row being
  -- edited it also opens (or closes) that pedal's menu.
  ClickChip pid -> do
    st <- H.get
    case st.draft of
      Just _ -> H.modify_ _ { chipMenu = if st.chipMenu == Just pid then Nothing else Just pid }
      Nothing -> pure unit
    H.raise (SelectPedal pid)

  -- Changing a chip auditions it, as the builder grid's menus did: what you
  -- set is what you hear before you save it.
  ChangeEngage pid valStr -> do
    let engage = valueToEngage valStr
    modifyDraft (setEntry pid _ { engage = engage })
    H.raise (SendEngageAudition pid engage)

  ChangePreset pid valStr -> do
    let presetId = if valStr == "" then Nothing else Just valStr
    modifyDraft (setEntry pid _ { presetId = presetId })
    st <- H.get
    case presetId >>= \id -> Array.find (\p -> p.id == id) st.input.presets of
      Just preset -> H.raise (AuditionPreset preset)
      Nothing -> pure unit

  -- All On and All Off send, as the builder's did; All "no change" only sets
  -- the row, because there is nothing to send for leaving a pedal alone.
  AllOn -> do
    st <- H.get
    modifyDraft (setEvery (map _.meta.id (chipPedals st)) { engage: EngageOn, presetId: Nothing })
    H.raise (SendEngageAll EngageOn)
  AllOff -> do
    st <- H.get
    modifyDraft (setEvery (map _.meta.id (chipPedals st)) { engage: EngageOff, presetId: Nothing })
    H.raise (SendEngageAll EngageOff)
  AllNoChange -> do
    st <- H.get
    modifyDraft (setEvery (map _.meta.id (chipPedals st)) { engage: EngageNoChange, presetId: Nothing })

  ClickLoadBoard presetId -> do
    st <- H.get
    case Array.find (\board -> board.id == presetId) st.input.boardPresets of
      Nothing -> pure unit
      Just board -> do
        H.modify_ _ { recalling = Just presetId }
        H.raise (RecallBoard board)
        H.modify_ _ { recalling = Nothing }

  StartNewBoard -> do
    st <- H.get
    H.modify_ _
      { draft = Just
          { target: NewBoard
          , name: ""
          , notes: ""
          , pedals: boardNow st.input.registry st.input.engine st.input.recalledPresets
          }
      , chipMenu = Nothing
      }

  StartEditBoard presetId -> do
    st <- H.get
    for_ (Array.find (\board -> board.id == presetId) st.input.boardPresets) \board ->
      H.modify_ _
        { draft = Just { target: ExistingBoard presetId, name: board.name, notes: board.notes, pedals: board.pedals }
        , chipMenu = Nothing
        }

  CancelDraft -> H.modify_ _ { draft = Nothing, chipMenu = Nothing }
  UpdateDraftName name -> modifyDraft _ { name = name }
  UpdateDraftNotes notes -> modifyDraft _ { notes = notes }

  CommitDraft -> do
    st <- H.get
    for_ st.draft \draft -> when (draft.name /= "") do
      case draft.target of
        NewBoard -> H.raise (SaveBoard { name: draft.name, notes: draft.notes, pedals: draft.pedals })
        ExistingBoard presetId ->
          H.raise (UpdateBoard presetId { name: draft.name, notes: draft.notes, pedals: draft.pedals })
      H.modify_ _ { draft = Nothing, chipMenu = Nothing }

  CommitDraftAsNew -> do
    st <- H.get
    for_ st.draft \draft -> when (draft.name /= "") do
      H.raise (SaveBoard { name: draft.name, notes: draft.notes, pedals: draft.pedals })
      H.modify_ _ { draft = Nothing, chipMenu = Nothing }

  ClickOverwriteBoard presetId -> do
    ok <- liftEffect $ FileIO.confirm "Overwrite this board with what every pedal is now?"
    when ok do
      st <- H.get
      H.raise (OverwriteBoard presetId
        (boardNow st.input.registry st.input.engine st.input.recalledPresets))

  ClickDeleteBoard presetId -> do
    ok <- liftEffect $ FileIO.confirm "Delete this board?"
    when ok $ H.raise (DeleteBoard presetId)

  ClickExportBoard board -> H.raise (ExportBoard board)
