module Component.Controls.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Component.Controls.Survey as Survey
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MC6.Board as Board
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch, ccToggleMessages, ccMomentaryMessages)
import Data.MC6.Survey as MC6Survey
import Data.MC6.Types (MC6Action(..), MC6Message, MC6MsgType(..), MC6NativeBank, MC6TogglePosition(..), intToMC6Action, intToMC6MsgType, mc6ActionToInt, mc6ToggleToInt)
import Data.Midi (unCC)
import Data.Pedal (PedalDef, PedalId, Control(..), LabelSource(..), Section)
import Data.Preset (BoardPreset, PedalPreset, PresetId)
import Engine (MC6Assignment)
import Data.String as String
import Data.String.CodeUnits (contains)
import Data.String.Common (toLower)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input =
  { controlBanks :: Array ControlBank
  , activeControlBankIdx :: Maybe Int
  , registry :: PedalRegistry
  , mc6BoardBankNum :: Int
  -- Everything below is for the instrument survey: what the config declares,
  -- and what the device itself last said. Held separately from `controlBanks`
  -- on purpose — one is intent, the others are observation, and the survey's
  -- whole job is to show where they disagree.
  , mc6NativeBanks :: Array MC6NativeBank
  , mc6BankNames :: Map Int String
  , mc6BankSwitches :: Map Int (Array String)
  , mc6ReadStatus :: Maybe String
  -- What a switch can hold besides messages. A board preset compiles to
  -- messages, so it is an alternative filling for the same slot rather than a
  -- different kind of thing — but the compilation has a budget, which is why
  -- the presets come along too: the count shown here must be the count that
  -- gets sent, so it comes from the same function.
  , boardPresets :: Array BoardPreset
  , presets :: Array PedalPreset
  , mc6Assignments :: Array MC6Assignment
  }

data Output
  = SaveControlBanks (Array ControlBank) (Maybe Int)
  | SyncControlBankToMC6
  -- | Ask the app to open a session with the MC6 and listen to what it
  -- | volunteers. Raised here because the survey is where you look at the
  -- | answer, and a read button somewhere else would be a button you press and
  -- | then go looking for the result of.
  | ReadMC6
  -- | Put a board on a switch, or take it off. Addressed by (bank, switch)
  -- | rather than by board, because a switch holds exactly one thing while a
  -- | board can sit on many switches — the other direction can express a
  -- | contradiction and this one cannot.
  | AssignBoard Int Int PresetId
  | UnassignSwitch Int Int

-- | Flat searchable entry for one CC control from the pedal registry
type CCEntry =
  { pedalId :: PedalId
  , pedalName :: String
  , pedalShort :: String
  , brand :: String
  , channel :: Int
  , sectionName :: String
  , cc :: Int
  , label :: String
  , controlKind :: String
  , isToggle :: Boolean
  }

type State =
  { input :: Input
  , selectedBankIdx :: Int
  , editBankName :: String
  , editBankNumber :: String
  , editBankDescription :: String
  , editReturnSwitch :: Int
  , ccIndex :: Array CCEntry
  , searchQuery :: String
  , expandedPedals :: Array PedalId
  , browserTargetSwitch :: Int
  , showDictionary :: Boolean
  -- The selection cascade. `selectedBankNumber` is a wire bank number and is
  -- the thing the survey grid sets; `selectedBankIdx` is where that bank lives
  -- in `controlBanks`, or -1 when the bank exists on the device but has never
  -- been authored here. Keeping both means every existing edit handler, which
  -- works by index, is untouched.
  , selectedBankNumber :: Maybe Int
  , selectedSwitchIdx :: Maybe Int
  , elideUnknown :: Boolean
  }

data Action
  = Receive Input
  | SelectBank Int
  -- Bank editing
  | UpdateBankName String
  | UpdateBankNumber String
  | UpdateBankDescription String
  | UpdateReturnSwitch String
  -- Bank CRUD
  | DuplicateBank
  | DeleteBank
  -- Switch editing (switchIdx, value)
  | UpdateLabel Int String
  | UpdateLongName Int String
  | UpdateToggle Int Boolean
  | UpdateLooper Int Boolean
  -- Message CRUD (switchIdx, ...)
  | AddMessage Int MC6MsgType
  | AddCCToggle Int
  | AddCCMomentary Int
  | DeleteMessage Int Int           -- switchIdx, msgIdx
  | UpdateMsgChannel Int Int String -- switchIdx, msgIdx, value
  | UpdateMsgData1 Int Int String
  | UpdateMsgData2 Int Int String
  | UpdateMsgData3 Int Int String
  | UpdateMsgData4 Int Int String
  | UpdateMsgAction Int Int String
  | UpdateMsgToggle Int Int String
  | UpdateMsgType Int Int String
  -- CC Search + Dictionary
  | UpdateSearch String
  | ToggleDictionary
  | ToggleDictPedal PedalId
  | SelectBrowserTarget String
  | AddFromBrowser Int Int Boolean  -- channel, cc, isToggle
  -- Sync
  | SyncToMC6
  -- The selection cascade
  | SelectBankNumber Int
  | ClearBankSelection
  | SelectSwitch Int
  | ClearSwitchSelection
  | CreatePageHere Int
  | ToggleElideUnknown
  | RequestRead
  | SetSwitchHolds Int String

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
  let bank = Array.index i.controlBanks 0
  in { input: i
     , selectedBankIdx: 0
     , editBankName: fromMaybe "" (bank <#> _.name)
     , editBankNumber: fromMaybe "" (bank <#> \b -> show b.mc6BankNumber)
     , editBankDescription: fromMaybe "" (bank <#> _.description)
     , editReturnSwitch: fromMaybe 6 (bank <#> _.returnSwitchIndex)
     , ccIndex: buildCCIndex i.registry
     , searchQuery: ""
     , expandedPedals: []
     , browserTargetSwitch: 0
     , showDictionary: false
     , selectedBankNumber: Nothing
     , selectedSwitchIdx: Nothing
     , elideUnknown: false
     }

-- | Flatten the entire pedal registry into a searchable array of CC entries
buildCCIndex :: PedalRegistry -> Array CCEntry
buildCCIndex reg = Array.concatMap flattenPedal (CRegistry.registryPedals reg)
  where
  flattenPedal :: PedalDef -> Array CCEntry
  flattenPedal def = Array.concatMap (flattenSection def) def.sections

  flattenSection :: PedalDef -> Section -> Array CCEntry
  flattenSection def section = Array.mapMaybe (toEntry def section) section.controls

  toEntry :: PedalDef -> Section -> Control -> Maybe CCEntry
  toEntry def section ctrl = case ctrl of
    Slider r -> Just (mk (resolveLabel r.label) (unCC r.cc) "slider" false)
    Toggle r -> Just (mk r.label (unCC r.cc) "toggle" true)
    Momentary r -> Just (mk r.label (unCC r.cc) "momentary" false)
    Segmented r -> Just (mk r.label (unCC r.cc) "segmented" false)
    Dropdown r -> Just (mk r.label (unCC r.cc) "dropdown" false)
    _ -> Nothing
    where
    mk lbl ccNum kind isTgl =
      { pedalId: def.meta.id
      , pedalName: def.meta.name
      , pedalShort: def.meta.shortName
      , brand: def.meta.brand
      , channel: def.meta.defaultChannel
      , sectionName: section.name
      , cc: ccNum
      , label: lbl
      , controlKind: kind
      , isToggle: isTgl
      }

  resolveLabel :: LabelSource -> String
  resolveLabel = case _ of
    Static s -> s
    ModeMap r -> "CC " <> show (unCC r.cc)
    ChannelMode _ -> "CC (mode)"

selectedBank :: State -> Maybe ControlBank
selectedBank st = Array.index st.input.controlBanks st.selectedBankIdx

-- | Rotating palette for bank identity colors
bankColor :: Int -> String
bankColor idx = case idx `mod` 8 of
  0 -> "#1a1a1a"
  1 -> "#1e40af"
  2 -> "#7e22ce"
  3 -> "#b45309"
  4 -> "#15803d"
  5 -> "#be123c"
  6 -> "#0e7490"
  7 -> "#6d28d9"
  _ -> "#1a1a1a"

-- | Per-switch accent colors (muted, distinguishable)
switchColor :: Int -> String
switchColor = case _ of
  0 -> "#2563eb"  -- A blue
  1 -> "#7c3aed"  -- B violet
  2 -> "#db2777"  -- C pink
  3 -> "#d97706"  -- D amber
  4 -> "#059669"  -- E emerald
  5 -> "#0891b2"  -- F cyan
  6 -> "#4f46e5"  -- G indigo
  7 -> "#c026d3"  -- H fuchsia
  8 -> "#dc2626"  -- I red
  _ -> "#666"

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

mc6MsgTypeLabel :: MC6MsgType -> String
mc6MsgTypeLabel = case _ of
  MsgEmpty -> "Empty"
  MsgPC -> "PC"
  MsgCC -> "CC"
  MsgNote -> "Note"
  MsgBankJump -> "BankJump"
  MsgDelay -> "Delay"
  MsgSetToggle -> "SetToggle"
  MsgEngagePreset -> "Engage"
  MsgBankUp -> "BankUp"
  MsgBankDown -> "BankDown"
  MsgTogglePage -> "TogglePage"
  MsgOther n -> "Type " <> show n
  _ -> "Other"

-- ──── Render ────

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "controls-view") ]
    ( [ renderSurvey state ]
        <> (case state.selectedBankNumber of
              Nothing -> []
              Just n -> [ renderBankZone state n ])
        <> (case state.selectedBankNumber, state.selectedSwitchIdx of
              Just _, Just i -> [ renderSwitchZone state i ]
              _, _ -> [])
        <> [ if state.showDictionary then renderDictionaryOverlay state else HH.text "" ]
    )

-- | The whole device, built fresh from the four sources the survey merges.
-- |
-- | Rebuilt on every render rather than cached in state, deliberately: the
-- | inputs are thirty small records and the alternative is a cache that can
-- | disagree with the device reading that just arrived — which is the one kind
-- | of staleness this particular view exists to expose.
renderSurvey :: forall m. State -> H.ComponentHTML Action () m
renderSurvey state =
  Survey.render
    { cards: surveyCards state
    , homeBank: state.input.mc6BoardBankNum
    , readStatus: state.input.mc6ReadStatus
    , elideUnknown: state.elideUnknown
    , selected: state.selectedBankNumber
    , compact: state.selectedBankNumber /= Nothing
    , onToggleElide: ToggleElideUnknown
    , onSelect: SelectBankNumber
    , onRead: RequestRead
    }

surveyCards :: State -> Array MC6Survey.BankCard
surveyCards state =
  MC6Survey.survey
    state.input.registry
    Board.boardRecallChannel
    state.input.controlBanks
    state.input.mc6NativeBanks
    state.input.mc6BankNames
    state.input.mc6BankSwitches

-- ──── Zoom 2: one bank ────

-- | What a bank is, once you are inside it.
-- |
-- | Three things share the space, and they are three different kinds of claim:
-- | the switches we authored, what the device says is there, and the page's own
-- | properties. Where the first two disagree the switch says so, because a
-- | disagreement discovered here is free and the same disagreement discovered
-- | mid-set is not.
renderBankZone :: forall m. State -> Int -> H.ComponentHTML Action () m
renderBankZone state bankNum =
  let mCard = Array.find (\c -> c.bankNumber == bankNum) (surveyCards state)
      mBank = selectedBank state
  in HH.div [ HP.class_ (H.ClassName "controls-zone controls-zone-bank") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-zone-head") ]
        [ HH.button
            [ HP.class_ (H.ClassName "controls-zone-back")
            , HE.onClick \_ -> ClearBankSelection
            ]
            [ HH.text "\x2190 instrument" ]
        , HH.h3_
            [ HH.text ("Bank " <> show bankNum <> " \x00b7 editor " <> show (bankNum + 1)) ]
        , HH.span [ HP.class_ (H.ClassName "controls-zone-sub") ]
            [ HH.text (case mCard of
                Just c -> Survey.provenanceLabel c.provenance
                Nothing -> "") ]
        ]
    , case mBank of
        Nothing -> renderUnauthoredBank mCard bankNum
        Just bank -> HH.div [ HP.class_ (H.ClassName "controls-bank-body") ]
          [ HH.div [ HP.class_ (H.ClassName "controls-bank-switches") ]
              (map (renderBankSwitchCell state bank mCard) Survey.physicalOrder)
          , HH.div [ HP.class_ (H.ClassName "controls-bank-side") ]
              [ renderBankIdentity state
              , renderBankProperties state
              , HH.div [ HP.class_ (H.ClassName "controls-bank-actions") ]
                  [ HH.button
                      [ HP.class_ (H.ClassName "controls-btn controls-btn-accent")
                      , HE.onClick \_ -> SyncToMC6
                      ]
                      [ HH.text "Sync to MC6" ]
                  , HH.button
                      [ HP.class_ (H.ClassName "controls-btn-small")
                      , HE.onClick \_ -> DuplicateBank
                      ]
                      [ HH.text "Duplicate" ]
                  , HH.button
                      [ HP.class_ (H.ClassName "controls-btn-small controls-btn-danger")
                      , HE.onClick \_ -> DeleteBank
                      ]
                      [ HH.text "Delete" ]
                  ]
              ]
          ]
    ]

renderBankIdentity :: forall m. State -> H.ComponentHTML Action () m
renderBankIdentity state =
  HH.div [ HP.class_ (H.ClassName "controls-field-row") ]
    [ HH.label_ [ HH.text "Name" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HP.value state.editBankName
        , HE.onValueInput UpdateBankName
        ]
    ]

-- | A bank the device has but this app has never written.
-- |
-- | The offer to author here is deliberately guarded rather than a plain
-- | button. Now that reads work we can tell the difference between "bank 11 is
-- | free" and "bank 11 is somebody's LoopyPro page", and syncing over the
-- | second is the kind of loss that has already happened once — a generated
-- | looper bank landed on a hand-built one and kept its name, so nothing looked
-- | wrong.
renderUnauthoredBank
  :: forall m. Maybe MC6Survey.BankCard -> Int -> H.ComponentHTML Action () m
renderUnauthoredBank mCard bankNum =
  let occupied = case mCard of
        Just c -> c.name /= "" || not (Array.null (Array.filter (_ /= "") c.observedNames))
        Nothing -> false
  in HH.div [ HP.class_ (H.ClassName "controls-bank-empty") ]
    ( [ HH.p_ [ HH.text "This app has never written this bank." ] ]
        <> (case mCard of
              Just c | not (Array.null c.observedNames) ->
                [ HH.p [ HP.class_ (H.ClassName "controls-observed-names") ]
                    [ HH.text ("The device says: "
                        <> String.joinWith "  \x00b7  "
                             (Array.filter (_ /= "") c.observedNames)) ]
                ]
              _ -> [])
        <> (if occupied
              then [ HH.p [ HP.class_ (H.ClassName "controls-warn") ]
                       [ HH.text "Something is already here. Authoring a page at this number and syncing would replace it, and the device will not warn you." ]
                   ]
              else [])
        <> [ HH.button
               [ HP.class_ (H.ClassName ("controls-btn-small" <> if occupied then " controls-btn-danger" else ""))
               , HE.onClick \_ -> CreatePageHere bankNum
               ]
               [ HH.text (if occupied then "Author a page here anyway" else "Author a page here") ]
           ]
    )

-- | One switch as it sits underfoot: our label, and the device's if it differs.
renderBankSwitchCell
  :: forall m. State -> ControlBank -> Maybe MC6Survey.BankCard -> Int
  -> H.ComponentHTML Action () m
renderBankSwitchCell state bank mCard idx =
  let mSw = Array.index bank.switches idx
      verb = mCard >>= \c -> Array.index c.slots idx
      observed = mCard >>= \c -> Array.index c.observedNames idx
      isSelected = state.selectedSwitchIdx == Just idx
      isReturn = bank.returnSwitchIndex == idx
      -- Authored pages carry nine switches; the device has twelve. The last
      -- three are not empty, they are simply never written, and a blank cell
      -- would claim otherwise.
      unwritable = idx >= Array.length bank.switches
      cls = String.joinWith " "
        ([ "controls-bank-switch" ]
          <> (if isSelected then [ "selected" ] else [])
          <> (if isReturn then [ "return" ] else [])
          <> (if unwritable then [ "unwritable" ] else []))
  in HH.div
    [ HP.class_ (H.ClassName cls)
    , HP.attr (HH.AttrName "style")
        ("border-left: 3px solid " <> maybe "#e6e6ea" Survey.verbColor verb)
    , HE.onClick \_ -> if unwritable then ClearSwitchSelection else SelectSwitch idx
    ]
    [ HH.div [ HP.class_ (H.ClassName "controls-bank-switch-head") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-bank-switch-letter") ]
            [ HH.text (switchLetter idx) ]
        , if isReturn
            then HH.span [ HP.class_ (H.ClassName "controls-sw-return-badge") ] [ HH.text "RTN" ]
            else HH.text ""
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-bank-switch-label") ]
        [ HH.text (if unwritable then "not written" else fromMaybe "" (mSw <#> _.label)) ]
    , case assignedBoard state bank.mc6BankNumber idx of
        Just bp -> HH.div [ HP.class_ (H.ClassName "controls-bank-switch-board") ]
          [ HH.text (bp.name <> "  " <> show (boardBudget state bp)
                      <> "/" <> show Board.messageLimit) ]
        Nothing -> HH.div [ HP.class_ (H.ClassName "controls-bank-switch-verb") ]
          [ HH.text (maybe "" Survey.verbName verb) ]
    , case observed of
        Just nm | nm /= "" && Just nm /= (mSw <#> _.label) ->
          HH.div [ HP.class_ (H.ClassName "controls-bank-switch-observed") ]
            [ HH.text ("device: " <> nm) ]
        _ -> HH.text ""
    ]

-- | The board assigned to a switch, if any.
assignedBoard :: State -> Int -> Int -> Maybe BoardPreset
assignedBoard state bankNum idx = do
  a <- Array.find (\x -> x.bankNumber == bankNum && x.switchIndex == idx)
         state.input.mc6Assignments
  Array.find (\b -> b.id == a.boardPresetId) state.input.boardPresets

-- | How many messages a board compiles to.
-- |
-- | Derived exactly as the sync path derives it, including the return jump, so
-- | the number shown is the number sent. A budget display that disagrees with
-- | the transmission is worse than none.
boardBudget :: State -> BoardPreset -> Int
boardBudget state bp =
  Board.boardMessageCount state.input.registry state.input.presets
    (state.input.activeControlBankIdx
      >>= Array.index state.input.controlBanks
      <#> _.mc6BankNumber)
    bp

-- ──── Zoom 3: one switch ────

renderSwitchZone :: forall m. State -> Int -> H.ComponentHTML Action () m
renderSwitchZone state idx = case selectedBank state of
  Nothing -> HH.text ""
  Just bank -> case Array.index bank.switches idx of
    Nothing -> HH.text ""
    Just sw ->
      let bankNum = bank.mc6BankNumber
          mBoard = assignedBoard state bankNum idx
      in HH.div [ HP.class_ (H.ClassName "controls-zone controls-zone-switch") ]
        [ HH.div [ HP.class_ (H.ClassName "controls-zone-head") ]
            [ HH.button
                [ HP.class_ (H.ClassName "controls-zone-back")
                , HE.onClick \_ -> ClearSwitchSelection
                ]
                [ HH.text "\x2190 bank" ]
            , HH.h3_ [ HH.text ("Switch " <> switchLetter idx
                                  <> (if sw.label == "" then "" else " \x00b7 " <> sw.label)) ]
            ]
        , renderHoldsSelector state bankNum idx mBoard
        , case mBoard of
            Just bp -> renderBoardHeld state bp
            Nothing ->
              HH.div [ HP.class_ (H.ClassName "controls-switch-body") ]
                [ renderSwitchSection (bankColor state.selectedBankIdx) bank.returnSwitchIndex idx sw
                , renderSearchPanel state
                ]
        ]

-- | What this switch holds: messages you author, or a board that compiles to
-- | them.
-- |
-- | Asked from the switch rather than from the board, which is the direction
-- | that cannot lie: a switch holds one thing, so a select box is the whole
-- | truth about it. Asked from the board — "which switch is this on?" — two
-- | boards can name the same switch and the model has no way to say which won.
renderHoldsSelector
  :: forall m. State -> Int -> Int -> Maybe BoardPreset -> H.ComponentHTML Action () m
renderHoldsSelector state bankNum idx mBoard =
  HH.div [ HP.class_ (H.ClassName "controls-holds") ]
    [ HH.label_ [ HH.text "This switch holds" ]
    , HH.select
        [ HP.class_ (H.ClassName "controls-holds-select")
        , HP.value (maybe "" _.id mBoard)
        , HE.onValueChange (SetSwitchHolds idx)
        ]
        ( [ HH.option [ HP.value "" ] [ HH.text "control switch \x2014 the messages below" ] ]
            <> map boardOption state.input.boardPresets
        )
    , case mBoard of
        Nothing -> HH.text ""
        Just bp ->
          let n = boardBudget state bp
          in HH.span
            [ HP.class_ (H.ClassName ("controls-holds-budget"
                          <> if n > Board.messageLimit then " over" else "")) ]
            [ HH.text (show n <> "/" <> show Board.messageLimit) ]
    , HH.span [ HP.class_ (H.ClassName "controls-holds-where") ]
        [ HH.text ("bank " <> show bankNum <> " \x00b7 switch " <> switchLetter idx) ]
    ]
  where
  boardOption bp =
    HH.option [ HP.value bp.id ]
      [ HH.text (bp.name <> "  (" <> show (boardBudget state bp) <> "/"
                  <> show Board.messageLimit <> ")") ]

-- | A switch holding a board shows the compilation, not an editor.
-- |
-- | The messages are generated from the board every time it is synced, so an
-- | editable message list here would be a field you can type into and watch get
-- | overwritten. Better to say where the content actually comes from.
renderBoardHeld :: forall m. State -> BoardPreset -> H.ComponentHTML Action () m
renderBoardHeld state bp =
  let n = boardBudget state bp
  in HH.div [ HP.class_ (H.ClassName "controls-board-held") ]
    ( [ HH.p_
          [ HH.text "The messages on this switch are compiled from the board "
          , HH.strong_ [ HH.text bp.name ]
          , HH.text ", and are rewritten from it on every sync. Edit the board in Boards to change what this switch does."
          ]
      ]
        <> (if bp.notes == "" then [] else
              [ HH.p [ HP.class_ (H.ClassName "preset-description") ] [ HH.text bp.notes ] ])
        <> (if n > Board.messageLimit
              then [ HH.p [ HP.class_ (H.ClassName "controls-warn") ]
                       [ HH.text ("This board compiles to " <> show n <> " messages and a switch holds "
                                   <> show Board.messageLimit <> ". Sync will refuse rather than send a switch that does most of what you asked.") ]
                   ]
              else [])
    )

renderBankProperties :: forall m. State -> H.ComponentHTML Action () m
renderBankProperties state =
  HH.div [ HP.class_ (H.ClassName "controls-bank-props") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-field-row") ]
        [ HH.label_ [ HH.text "Return Sw" ]
        , HH.select
            [ HP.value (show state.editReturnSwitch)
            , HE.onValueChange UpdateReturnSwitch
            ]
            (Array.range 0 8 <#> \i ->
              HH.option [ HP.value (show i) ] [ HH.text (switchLetter i) ]
            )
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-field-row controls-field-row-top") ]
        [ HH.label_ [ HH.text "Notes" ]
        , HH.textarea
            [ HP.value state.editBankDescription
            , HP.placeholder "Description"
            , HP.rows 3
            , HE.onValueInput UpdateBankDescription
            ]
        ]
    ]

-- ──── All Switches Panel (right column) ────

renderSwitchSection :: forall m. String -> Int -> Int -> ControlBankSwitch -> H.ComponentHTML Action () m
renderSwitchSection bankCol returnIdx swIdx sw =
  let isReturn = swIdx == returnIdx
      swColor = switchColor swIdx
  in HH.div
    [ HP.class_ (H.ClassName "controls-sw-section")
    , HP.attr (HH.AttrName "style") ("border-left: 3px solid " <> swColor)
    ]
    [ HH.div [ HP.class_ (H.ClassName "controls-sw-section-header") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-sw-section-letter"), HP.attr (HH.AttrName "style") ("color: " <> swColor) ] [ HH.text (switchLetter swIdx) ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.class_ (H.ClassName "controls-sw-label-input")
            , HP.value sw.label
            , HP.attr (HH.AttrName "maxlength") "8"
            , HP.placeholder "label"
            , HE.onValueInput (UpdateLabel swIdx)
            ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.class_ (H.ClassName "controls-sw-long-input")
            , HP.value sw.longName
            , HP.attr (HH.AttrName "maxlength") "24"
            , HP.placeholder "long name"
            , HE.onValueInput (UpdateLongName swIdx)
            ]
        , HH.label [ HP.class_ (H.ClassName "controls-sw-toggle-label") ]
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.checked sw.toToggle
                , HE.onChecked (UpdateToggle swIdx)
                ]
            , HH.text " Tgl"
            ]
        , HH.label [ HP.class_ (H.ClassName "controls-sw-toggle-label") ]
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.checked (hasLooperMode sw)
                , HE.onChecked (UpdateLooper swIdx)
                ]
            , HH.text " Loop"
            ]
        , if isReturn
            then HH.span [ HP.class_ (H.ClassName "controls-sw-return-badge") ] [ HH.text "RTN" ]
            else HH.text ""
        ]
    , let indexed = Array.mapWithIndex (\i msg -> { idx: i, msg }) sw.messages
          visible = Array.filter (\r -> r.msg.msgType /= MsgLooperMode) indexed
      in if Array.null visible
        then HH.text ""
        else HH.div [ HP.class_ (H.ClassName "controls-messages") ]
          (map (\r -> renderMessageRow bankCol swIdx r.idx r.msg) visible)
    , renderAddButtons swIdx
    ]

renderMessageRow :: forall m. String -> Int -> Int -> MC6Message -> H.ComponentHTML Action () m
renderMessageRow color swIdx msgIdx msg =
  HH.div [ HP.class_ (H.ClassName "controls-message-row") ]
    [ HH.span [ HP.class_ (H.ClassName "controls-msg-type-badge"), HP.attr (HH.AttrName "style") ("background: " <> color) ]
        [ HH.text (mc6MsgTypeLabel msg.msgType) ]
    , renderMsgFields swIdx msgIdx msg
    , HH.button
        [ HP.class_ (H.ClassName "controls-msg-delete")
        , HE.onClick \_ -> DeleteMessage swIdx msgIdx
        ]
        [ HH.text "\x00D7" ]
    ]

renderMsgFields :: forall m. Int -> Int -> MC6Message -> H.ComponentHTML Action () m
renderMsgFields swIdx msgIdx msg = case msg.msgType of
  MsgCC ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Ch" (show msg.channel) (UpdateMsgChannel swIdx msgIdx) "38px"
      , msgField "CC" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , msgField "Val" (show msg.data2) (UpdateMsgData2 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      , toggleSelect swIdx msgIdx msg.togglePosition
      ]
  MsgPC ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Ch" (show msg.channel) (UpdateMsgChannel swIdx msgIdx) "38px"
      , msgField "PC" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      ]
  MsgBankJump ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Bank" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      ]
  MsgDelay ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "ms" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "55px" ]
  MsgEngagePreset ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Preset" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "55px"
      , actionSelect swIdx msgIdx msg.action
      ]
  MsgNote ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Ch" (show msg.channel) (UpdateMsgChannel swIdx msgIdx) "38px"
      , msgField "Note" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , msgField "Vel" (show msg.data2) (UpdateMsgData2 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      ]
  MsgSetToggle ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "d1" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      ]
  _ ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Ch" (show msg.channel) (UpdateMsgChannel swIdx msgIdx) "38px"
      , msgField "d1" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , msgField "d2" (show msg.data2) (UpdateMsgData2 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      , toggleSelect swIdx msgIdx msg.togglePosition
      ]

msgField :: forall m. String -> String -> (String -> Action) -> String -> H.ComponentHTML Action () m
msgField lbl val onChange w =
  HH.label [ HP.class_ (H.ClassName "controls-msg-field") ]
    [ HH.span [ HP.class_ (H.ClassName "controls-msg-field-label") ] [ HH.text lbl ]
    , HH.input
        [ HP.type_ HP.InputNumber
        , HP.value val
        , HP.attr (HH.AttrName "style") ("width: " <> w)
        , HE.onValueInput onChange
        ]
    ]

actionSelect :: forall m. Int -> Int -> MC6Action -> H.ComponentHTML Action () m
actionSelect swIdx msgIdx current =
  HH.select
    [ HP.class_ (H.ClassName "controls-msg-select")
    , HP.value (show (mc6ActionToInt current))
    , HE.onValueChange (UpdateMsgAction swIdx msgIdx)
    ]
    [ HH.option [ HP.value "0" ] [ HH.text "None" ]
    , HH.option [ HP.value "1" ] [ HH.text "Press" ]
    , HH.option [ HP.value "2" ] [ HH.text "Release" ]
    , HH.option [ HP.value "3" ] [ HH.text "LongPress" ]
    , HH.option [ HP.value "4" ] [ HH.text "LPRelease" ]
    , HH.option [ HP.value "5" ] [ HH.text "DblTap" ]
    , HH.option [ HP.value "6" ] [ HH.text "DblTapRel" ]
    , HH.option [ HP.value "9" ] [ HH.text "RelAll" ]
    ]

toggleSelect :: forall m. Int -> Int -> MC6TogglePosition -> H.ComponentHTML Action () m
toggleSelect swIdx msgIdx current =
  HH.select
    [ HP.class_ (H.ClassName "controls-msg-select")
    , HP.value (show (mc6ToggleToInt current))
    , HE.onValueChange (UpdateMsgToggle swIdx msgIdx)
    ]
    [ HH.option [ HP.value "0" ] [ HH.text "Tg Off" ]
    , HH.option [ HP.value "1" ] [ HH.text "Tg On" ]
    , HH.option [ HP.value "2" ] [ HH.text "Both" ]
    ]

renderAddButtons :: forall m. Int -> H.ComponentHTML Action () m
renderAddButtons swIdx =
  HH.div [ HP.class_ (H.ClassName "controls-add-message") ]
    [ HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddCCToggle swIdx ] [ HH.text "+Toggle" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddCCMomentary swIdx ] [ HH.text "+Mom" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddMessage swIdx MsgCC ] [ HH.text "+CC" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddMessage swIdx MsgPC ] [ HH.text "+PC" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddMessage swIdx MsgBankJump ] [ HH.text "+BkJmp" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddMessage swIdx MsgDelay ] [ HH.text "+Delay" ]
    ]

-- ──── Search Panel (right side of top row) ────

renderSearchPanel :: forall m. State -> H.ComponentHTML Action () m
renderSearchPanel state =
  HH.div [ HP.class_ (H.ClassName "controls-search-panel") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-search-input-row") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-search-icon") ] [ HH.text "\x1F50D" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.class_ (H.ClassName "controls-search-input")
            , HP.placeholder "Search CCs across all pedals..."
            , HP.value state.searchQuery
            , HE.onValueInput UpdateSearch
            ]
        , HH.span [ HP.class_ (H.ClassName "controls-search-target") ]
            [ HH.text "Add to:"
            , HH.select
                [ HP.value (show state.browserTargetSwitch)
                , HE.onValueChange SelectBrowserTarget
                ]
                (Array.range 0 8 <#> \i ->
                  HH.option [ HP.value (show i) ] [ HH.text (switchLetter i) ]
                )
            ]
        , HH.button
            [ HP.class_ (H.ClassName "controls-btn-small")
            , HE.onClick \_ -> ToggleDictionary
            ]
            [ HH.text "Browse All" ]
        ]
    , renderSearchResults state
    ]

renderSearchResults :: forall m. State -> H.ComponentHTML Action () m
renderSearchResults state =
  let q = toLower state.searchQuery
  in if q == ""
    then HH.text ""
    else
      let matches = Array.filter (matchEntry q) state.ccIndex
      in if Array.null matches
        then HH.div [ HP.class_ (H.ClassName "controls-search-empty") ] [ HH.text "No matches" ]
        else HH.div [ HP.class_ (H.ClassName "controls-search-results") ]
          (map (renderSearchRow state) matches)

matchEntry :: String -> CCEntry -> Boolean
matchEntry q entry =
  contains (Pattern q) (toLower entry.label)
  || contains (Pattern q) (toLower (show entry.cc))
  || contains (Pattern q) (toLower entry.pedalName)
  || contains (Pattern q) (toLower entry.pedalShort)
  || contains (Pattern q) (toLower entry.sectionName)
  || contains (Pattern q) (toLower entry.controlKind)

renderSearchRow :: forall m. State -> CCEntry -> H.ComponentHTML Action () m
renderSearchRow state entry =
  HH.div [ HP.class_ (H.ClassName "controls-search-row") ]
    [ HH.span [ HP.class_ (H.ClassName "controls-search-pedal") ] [ HH.text entry.pedalShort ]
    , HH.span [ HP.class_ (H.ClassName "controls-cc-num") ] [ HH.text ("CC" <> show entry.cc) ]
    , HH.span [ HP.class_ (H.ClassName "controls-search-ch") ] [ HH.text ("ch" <> show entry.channel) ]
    , HH.span [ HP.class_ (H.ClassName "controls-cc-label") ] [ HH.text entry.label ]
    , HH.span [ HP.class_ (H.ClassName "controls-search-kind") ] [ HH.text entry.controlKind ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser entry.channel entry.cc true
        ]
        [ HH.text ("+" <> switchLetter state.browserTargetSwitch <> " Tgl") ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser entry.channel entry.cc false
        ]
        [ HH.text ("+" <> switchLetter state.browserTargetSwitch <> " Mom") ]
    ]

-- ──── CC Dictionary (modal overlay) ────

renderDictionaryOverlay :: forall m. State -> H.ComponentHTML Action () m
renderDictionaryOverlay state =
  HH.div [ HP.class_ (H.ClassName "controls-dict-overlay") ]
    [ HH.div
        [ HP.class_ (H.ClassName "controls-dict-backdrop")
        , HE.onClick \_ -> ToggleDictionary
        ]
        []
    , HH.div [ HP.class_ (H.ClassName "controls-dict-modal") ]
        [ HH.div [ HP.class_ (H.ClassName "controls-dict-modal-header") ]
            [ HH.span [ HP.class_ (H.ClassName "controls-dict-header") ] [ HH.text "CC Dictionary" ]
            , HH.button
                [ HP.class_ (H.ClassName "controls-dict-close")
                , HE.onClick \_ -> ToggleDictionary
                ]
                [ HH.text "\x00D7" ]
            ]
        , renderDictionary state
        ]
    ]

renderDictionary :: forall m. State -> H.ComponentHTML Action () m
renderDictionary state =
  let pedals = CRegistry.registryPedals state.input.registry
  in HH.div [ HP.class_ (H.ClassName "controls-dict-list") ]
    (map (renderDictPedal state) pedals)

renderDictPedal :: forall m. State -> PedalDef -> H.ComponentHTML Action () m
renderDictPedal state def =
  let pid = def.meta.id
      isExpanded = Array.elem pid state.expandedPedals
      arrow = if isExpanded then "\x25BE " else "\x25B8 "
  in HH.div [ HP.class_ (H.ClassName ("controls-dict-pedal" <> if isExpanded then " expanded" else "")) ]
    [ HH.div
        [ HP.class_ (H.ClassName "controls-dict-pedal-header")
        , HE.onClick \_ -> ToggleDictPedal pid
        ]
        [ HH.span [ HP.class_ (H.ClassName "controls-dict-arrow") ] [ HH.text arrow ]
        , HH.span [ HP.class_ (H.ClassName "controls-dict-pedal-name") ] [ HH.text def.meta.name ]
        , HH.span [ HP.class_ (H.ClassName "controls-dict-pedal-meta") ]
            [ HH.text (def.meta.brand <> ", ch " <> show def.meta.defaultChannel) ]
        ]
    , if isExpanded
        then HH.div [ HP.class_ (H.ClassName "controls-dict-sections") ]
          (Array.mapMaybe (renderDictSection state def.meta.defaultChannel) def.sections)
        else HH.text ""
    ]

renderDictSection :: forall m. State -> Int -> Section -> Maybe (H.ComponentHTML Action () m)
renderDictSection state ch section =
  let entries = Array.mapMaybe (controlToCCEntry ch) section.controls
  in if Array.null entries
    then Nothing
    else Just $
      HH.div [ HP.class_ (H.ClassName "controls-dict-section") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-dict-section-name") ] [ HH.text (section.name <> ":") ]
        , HH.span [ HP.class_ (H.ClassName "controls-dict-controls") ]
            (Array.concatMap (\e -> [ renderDictControl state e ]) entries)
        ]

type DictEntry = { ch :: Int, cc :: Int, label :: String, kind :: String, isToggle :: Boolean }

controlToCCEntry :: Int -> Control -> Maybe DictEntry
controlToCCEntry ch = case _ of
  Slider r -> Just { ch, cc: unCC r.cc, label: resolveLabel r.label, kind: "slider", isToggle: false }
  Toggle r -> Just { ch, cc: unCC r.cc, label: r.label, kind: "toggle", isToggle: true }
  Momentary r -> Just { ch, cc: unCC r.cc, label: r.label, kind: "momentary", isToggle: false }
  Segmented r -> Just { ch, cc: unCC r.cc, label: r.label, kind: "segmented", isToggle: false }
  Dropdown r -> Just { ch, cc: unCC r.cc, label: r.label, kind: "dropdown", isToggle: false }
  _ -> Nothing
  where
  resolveLabel = case _ of
    Static s -> s
    ModeMap r -> "CC " <> show (unCC r.cc)
    ChannelMode _ -> "CC (mode)"

renderDictControl :: forall m. State -> DictEntry -> H.ComponentHTML Action () m
renderDictControl _state entry =
  HH.span [ HP.class_ (H.ClassName "controls-dict-control") ]
    [ HH.span [ HP.class_ (H.ClassName "controls-dict-control-label") ]
        [ HH.text (entry.label <> " CC" <> show entry.cc) ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser entry.ch entry.cc true
        ]
        [ HH.text "+Tgl" ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser entry.ch entry.cc false
        ]
        [ HH.text "+Mom" ]
    ]

-- ──── Action Handlers ────

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> do
    st <- H.get
    if Array.length input.controlBanks /= Array.length st.input.controlBanks
      then H.put (initialState input)
      else do
        H.modify_ _ { input = input }
        -- The CC index is built from the registry at mount. The registry
        -- arrives asynchronously and can grow — a pedal added to `rig.json`
        -- would otherwise be missing from the browser until a reload, which
        -- reads as "that pedal can't be assigned" rather than "the index is
        -- stale". Cheap to rebuild, and only when the count actually moved.
        when (Array.length (CRegistry.registryPedals input.registry)
                /= Array.length (CRegistry.registryPedals st.input.registry)) $
          H.modify_ _ { ccIndex = buildCCIndex input.registry }

  SelectBank idx -> do
    -- Save pending bank property edits first
    commitBankProps
    H.modify_ \st ->
      let mBank = Array.index st.input.controlBanks idx
      in st { selectedBankIdx = idx
            , selectedBankNumber = mBank <#> _.mc6BankNumber
            , selectedSwitchIdx = Nothing
            , editBankName = fromMaybe "" (mBank <#> _.name)
            , editBankNumber = fromMaybe "" (mBank <#> \b -> show b.mc6BankNumber)
            , editBankDescription = fromMaybe "" (mBank <#> _.description)
            , editReturnSwitch = fromMaybe 6 (mBank <#> _.returnSwitchIndex)
            }

  -- Bank properties
  UpdateBankName s -> do
    H.modify_ _ { editBankName = s }
    commitBankProps
  UpdateBankNumber s -> do
    H.modify_ _ { editBankNumber = s }
    commitBankProps
  UpdateBankDescription s -> do
    H.modify_ _ { editBankDescription = s }
    commitBankProps
  UpdateReturnSwitch s -> case Int.fromString s of
    Nothing -> pure unit
    Just i -> do
      H.modify_ _ { editReturnSwitch = i }
      commitBankProps

  -- Bank CRUD
  DuplicateBank -> do
    st <- H.get
    case selectedBank st of
      Nothing -> pure unit
      Just bank -> do
        let dup = bank { id = bank.id <> "-copy", name = bank.name <> " Copy", mc6BankNumber = bank.mc6BankNumber + 1 }
            banks = Array.snoc st.input.controlBanks dup
            newIdx = Array.length banks - 1
        H.raise (SaveControlBanks banks (Just newIdx))
        handleAction (SelectBank newIdx)

  DeleteBank -> do
    st <- H.get
    when (Array.length st.input.controlBanks > 0) do
      let banks = fromMaybe st.input.controlBanks (Array.deleteAt st.selectedBankIdx st.input.controlBanks)
          newIdx = if st.selectedBankIdx >= Array.length banks
                   then max 0 (Array.length banks - 1)
                   else st.selectedBankIdx
          mActive = if Array.null banks then Nothing else Just newIdx
      H.raise (SaveControlBanks banks mActive)
      when (not (Array.null banks)) do
        handleAction (SelectBank newIdx)

  -- Switch editing — modify bank data directly
  UpdateLabel swIdx s -> do
    modifySwitch swIdx \sw -> sw { label = s }
    save
  UpdateLongName swIdx s -> do
    modifySwitch swIdx \sw -> sw { longName = s }
    save
  UpdateToggle swIdx b -> do
    modifySwitch swIdx \sw -> sw { toToggle = b }
    save
  UpdateLooper swIdx b -> do
    modifySwitch swIdx \sw ->
      let without = Array.filter (\m -> m.msgType /= MsgLooperMode) sw.messages
      in sw { messages = if b then without <> [looperModeMessage] else without }
    save

  -- Message CRUD
  AddMessage swIdx msgType -> do
    modifySwitchMessages swIdx \msgs -> msgs <> [newMessage msgType]
    save
  AddCCToggle swIdx -> do
    modifySwitchMessages swIdx \msgs -> msgs <> ccToggleMessages 1 0
    save
  AddCCMomentary swIdx -> do
    modifySwitchMessages swIdx \msgs -> msgs <> ccMomentaryMessages 1 0
    save
  DeleteMessage swIdx msgIdx -> do
    modifySwitchMessages swIdx \msgs -> fromMaybe msgs (Array.deleteAt msgIdx msgs)
    save

  -- Message field updates
  UpdateMsgChannel swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { channel = v }
    Nothing -> msg
  UpdateMsgData1 swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { data1 = v }
    Nothing -> msg
  UpdateMsgData2 swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { data2 = v }
    Nothing -> msg
  UpdateMsgData3 swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { data3 = v }
    Nothing -> msg
  UpdateMsgData4 swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { data4 = v }
    Nothing -> msg
  UpdateMsgAction swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { action = intToMC6Action v }
    Nothing -> msg
  UpdateMsgToggle swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { togglePosition = intToToggle v }
    Nothing -> msg
  UpdateMsgType swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { msgType = intToMC6MsgType v }
    Nothing -> msg

  -- CC Search + Dictionary
  UpdateSearch s -> H.modify_ _ { searchQuery = s }
  ToggleDictionary -> H.modify_ \st -> st { showDictionary = not st.showDictionary }
  ToggleDictPedal pid -> H.modify_ \st ->
    if Array.elem pid st.expandedPedals
      then st { expandedPedals = Array.filter (_ /= pid) st.expandedPedals }
      else st { expandedPedals = st.expandedPedals <> [pid] }
  SelectBrowserTarget s -> case Int.fromString s of
    Just i -> H.modify_ _ { browserTargetSwitch = i }
    Nothing -> pure unit
  AddFromBrowser ch cc isToggle -> do
    st <- H.get
    let msgs = if isToggle then ccToggleMessages ch cc else ccMomentaryMessages ch cc
    modifySwitchMessages st.browserTargetSwitch \existing -> existing <> msgs
    save

  SyncToMC6 -> do
    commitBankProps
    H.raise SyncControlBankToMC6

  -- Moving between zoom levels commits whatever was half-typed into the bank
  -- property fields. Without this, stepping out to check something silently
  -- discards a rename, and the survey then shows the old name — which reads as
  -- a device disagreement rather than as unsaved input.
  SelectBankNumber n -> do
    commitBankProps
    st <- H.get
    let idx = fromMaybe (-1)
          (Array.findIndex (\b -> b.mc6BankNumber == n) st.input.controlBanks)
        mBank = Array.index st.input.controlBanks idx
    H.modify_ _
      { selectedBankNumber = Just n
      , selectedSwitchIdx = Nothing
      , selectedBankIdx = idx
      , editBankName = fromMaybe "" (mBank <#> _.name)
      , editBankNumber = fromMaybe (show n) (mBank <#> \b -> show b.mc6BankNumber)
      , editBankDescription = fromMaybe "" (mBank <#> _.description)
      , editReturnSwitch = fromMaybe 6 (mBank <#> _.returnSwitchIndex)
      }

  ClearBankSelection -> do
    commitBankProps
    H.modify_ _ { selectedBankNumber = Nothing, selectedSwitchIdx = Nothing }

  -- The CC browser adds to whatever switch you are inside of, so the separate
  -- "add to" target selector no longer has anything to disambiguate.
  SelectSwitch i -> H.modify_ _ { selectedSwitchIdx = Just i, browserTargetSwitch = i }

  ClearSwitchSelection -> H.modify_ _ { selectedSwitchIdx = Nothing }

  CreatePageHere n -> do
    st <- H.get
    let newBank = emptyControlBank ("Bank " <> show n) n
        banks = Array.snoc st.input.controlBanks newBank
        newIdx = Array.length banks - 1
    H.raise (SaveControlBanks banks (Just newIdx))
    H.modify_ _
      { selectedBankIdx = newIdx
      , selectedBankNumber = Just n
      , selectedSwitchIdx = Nothing
      , editBankName = newBank.name
      , editBankNumber = show n
      , editBankDescription = newBank.description
      , editReturnSwitch = newBank.returnSwitchIndex
      }

  ToggleElideUnknown -> H.modify_ \st -> st { elideUnknown = not st.elideUnknown }

  RequestRead -> H.raise ReadMC6

  SetSwitchHolds idx boardId -> do
    st <- H.get
    for_ (selectedBank st) \bank ->
      if boardId == ""
        then H.raise (UnassignSwitch bank.mc6BankNumber idx)
        else H.raise (AssignBoard bank.mc6BankNumber idx boardId)

-- ──── Helpers ────

intToToggle :: Int -> MC6TogglePosition
intToToggle = case _ of
  0 -> ToggleOff
  1 -> ToggleOn
  _ -> ToggleBoth

hasLooperMode :: ControlBankSwitch -> Boolean
hasLooperMode sw = Array.any (\m -> m.msgType == MsgLooperMode) sw.messages

looperModeMessage :: MC6Message
looperModeMessage =
  { msgType: MsgLooperMode
  , channel: 0
  , data1: 0
  , data2: 0
  , data3: 0
  , data4: 0
  , action: ActionPress
  , togglePosition: ToggleBoth
  , msgIndex: 0
  }

newMessage :: MC6MsgType -> MC6Message
newMessage msgType =
  { msgType
  , channel: 1
  , data1: 0
  , data2: 0
  , data3: 0
  , data4: 0
  , action: ActionPress
  , togglePosition: ToggleBoth
  , msgIndex: 0
  }

emptySwitch :: ControlBankSwitch
emptySwitch =
  { label: ""
  , longName: ""
  , toToggle: false
  , messages: []
  }

emptyControlBank :: String -> Int -> ControlBank
emptyControlBank name bankNum =
  { id: "bank-" <> show bankNum
  , name
  , description: ""
  , mc6BankNumber: bankNum
  , returnSwitchIndex: 6
  , switches: Array.replicate 9 emptySwitch
  }

-- | Modify a switch in the selected bank
modifySwitch :: forall m. MonadAff m => Int -> (ControlBankSwitch -> ControlBankSwitch) -> H.HalogenM State Action () Output m Unit
modifySwitch swIdx f = do
  st <- H.get
  case Array.index st.input.controlBanks st.selectedBankIdx of
    Nothing -> pure unit
    Just bank -> do
      let newSwitches = fromMaybe bank.switches (Array.modifyAt swIdx f bank.switches)
          newBank = bank { switches = newSwitches }
          newBanks = fromMaybe st.input.controlBanks (Array.updateAt st.selectedBankIdx newBank st.input.controlBanks)
      H.modify_ _ { input = st.input { controlBanks = newBanks } }

-- | Modify messages of a specific switch
modifySwitchMessages :: forall m. MonadAff m => Int -> (Array MC6Message -> Array MC6Message) -> H.HalogenM State Action () Output m Unit
modifySwitchMessages swIdx f =
  modifySwitch swIdx \sw -> sw { messages = reindexMessages (f sw.messages) }

-- | Update a single message field
updateMsg :: forall m. MonadAff m => Int -> Int -> (MC6Message -> MC6Message) -> H.HalogenM State Action () Output m Unit
updateMsg swIdx msgIdx f = do
  modifySwitchMessages swIdx \msgs -> fromMaybe msgs (Array.modifyAt msgIdx f msgs)
  save

-- | Apply bank property edits to data and save
commitBankProps :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
commitBankProps = do
  st <- H.get
  case Array.index st.input.controlBanks st.selectedBankIdx of
    Nothing -> pure unit
    Just bank -> do
      let bankNum = fromMaybe bank.mc6BankNumber (Int.fromString st.editBankNumber)
          updatedBank = bank
            { name = st.editBankName
            , mc6BankNumber = bankNum
            , description = st.editBankDescription
            , returnSwitchIndex = st.editReturnSwitch
            }
          banks = fromMaybe st.input.controlBanks
            (Array.updateAt st.selectedBankIdx updatedBank st.input.controlBanks)
      H.modify_ _ { input = st.input { controlBanks = banks } }
      H.raise (SaveControlBanks banks (Just st.selectedBankIdx))

-- | Save current bank data
save :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
save = do
  st <- H.get
  H.raise (SaveControlBanks st.input.controlBanks (Just st.selectedBankIdx))

reindexMessages :: Array MC6Message -> Array MC6Message
reindexMessages = Array.mapWithIndex \i m -> m { msgIndex = i }
