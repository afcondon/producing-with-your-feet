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
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.MC6.Board as Board
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch, ccToggleMessages, ccMomentaryMessages, emptySwitch, switchCount, switchLetter)
import Data.MC6.Survey as MC6Survey
import Data.MC6.Types (MC6Action(..), MC6Message, MC6MsgType(..), MC6NativeBank, MC6TogglePosition(..), intToMC6Action, intToMC6MsgType, mc6ActionToInt, mc6ToggleToInt)
import Data.Midi (unCC)
import Data.Pedal (PedalDef, PedalId, Control(..), LabelSource(..), Section)
import Data.MC6.Global (GlobalSwitch)
import Data.MC6.Global as Global
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
  -- | Every bank the app intends the device to hold — generated pages and the
  -- | blanks a whole-map write would clear — as opposed to `controlBanks`,
  -- | which is the editable handful. The survey compares intent against
  -- | observation, and it was being handed the handful, so it could speak for
  -- | one bank in thirty and said nothing about the rest. `agrees = Nothing`
  -- | reads as "no disagreement" at a glance and means "nothing to compare".
  , intendedBanks :: Array ControlBank
  , activeControlBankIdx :: Maybe Int
  , registry :: PedalRegistry
  , mc6BoardBankNum :: Int
  -- Everything below is for the instrument survey: what the config declares,
  -- and what the device itself last said. Held separately from `controlBanks`
  -- on purpose — one is intent, the others are observation, and the survey's
  -- whole job is to show where they disagree.
  , mc6NativeBanks :: Array MC6NativeBank
  -- | What a full dump read back: every bank, with messages. The only source
  -- | that can make a bank this app never wrote reproducible rather than merely
  -- | describable.
  , mc6DumpedBanks :: Array MC6NativeBank
  , mc6BankNames :: Map Int String
  , mc6BankSwitches :: Map Int (Array String)
  , mc6ReadStatus :: Maybe String
  -- | Whether an editor session is currently being held open. A Boolean here
  -- | rather than the session itself: the view needs to say which mode the
  -- | instrument is in, and it has no business being able to send anything.
  , mc6SessionHeld :: Boolean
  -- | Set once the bank-change probe has found a channel; until then there is
  -- | no way to read more than the bank the device happens to be showing.
  , mc6Reading :: Boolean
  , mc6ReadAt :: Maybe String
  -- What a switch can hold besides messages. A board preset compiles to
  -- messages, so it is an alternative filling for the same slot rather than a
  -- different kind of thing — but the compilation has a budget, which is why
  -- the presets come along too: the count shown here must be the count that
  -- gets sent, so it comes from the same function.
  , boardPresets :: Array BoardPreset
  , presets :: Array PedalPreset
  , mc6Assignments :: Array MC6Assignment
  -- Switches that belong to the instrument rather than to any one page.
  , globalSwitches :: Array GlobalSwitch
  }

data Output
  = SaveControlBanks (Array ControlBank) (Maybe Int)
  -- | Sync *this* bank. Carries the bank rather than leaving the app to look up
  -- | whichever one it last recorded as active: selecting a bank in the survey
  -- | does not itself save anything, so the app's idea of "active" could lag a
  -- | page behind what you are looking at — and a sync that writes the wrong
  -- | bank number is the one mistake on this page with no undo.
  | SyncControlBankToMC6 ControlBank
  -- | Write every authored page. The only correct answer after a global
  -- | changes: a global occupies its slot on all of them, so one page's worth
  -- | of SysEx leaves the other twenty-nine holding the previous version with
  -- | nothing on the device or in this app saying they disagree.
  | SyncAllBanksToMC6
  -- | Ask the app to open a session with the MC6 and listen to what it
  -- | volunteers. Raised here because the survey is where you look at the
  -- | answer, and a read button somewhere else would be a button you press and
  -- | then go looking for the result of.
  | ReadMC6
  -- | Read the entire device: find how to move it if that is not yet known,
  -- | then walk every bank until all thirty have answered. One output because
  -- | it is one intention — a complete reading — and splitting it made the
  -- | prerequisite something the user had to know about.
  | DeepReadMC6
  -- | Hold an editor session open, or let it go. The device will not change
  -- | bank for us without one, so this is what makes the app able to move the
  -- | MC6 while the board is being played rather than only while it is idle.
  | ToggleMC6Session
  -- | Put a board on a switch, or take it off. Addressed by (bank, switch)
  -- | rather than by board, because a switch holds exactly one thing while a
  -- | board can sit on many switches — the other direction can express a
  -- | contradiction and this one cannot.
  | AssignBoard Int Int PresetId
  | UnassignSwitch Int Int
  -- | The instrument's global switches, whole. Small enough to send entire, and
  -- | sending the whole list means there is never a partial write to reconcile.
  | SaveGlobalSwitches (Array GlobalSwitch)
  -- | Move the device to the bank being looked at. Selecting a card here and
  -- | standing on that bank underfoot are the same intention — checking a page
  -- | means checking it against the switches, and walking to the MC6 to find it
  -- | showing something else is how you end up editing the wrong page.
  | JumpMC6ToBank Int

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
  -- | The globals page has the work column instead of a bank. Not a variant of
  -- | `selectedBankNumber` because it is not a bank: it has no wire number, it
  -- | is never synced on its own, and it compiles into all of them.
  , viewingGlobals :: Boolean
  -- | An in-progress stamp: which slot is being copied, and onto which wire
  -- | bank numbers. Held here rather than committed as you tick, because a
  -- | stamp is a single destructive write and half a stamp is not a state the
  -- | store should ever be in.
  , stampSlot :: Maybe Int
  , stampTargets :: Array Int
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
  | SyncAllToMC6
  -- The selection cascade
  | SelectBankNumber Int
  | ClearBankSelection
  | SelectSwitch Int
  | ClearSwitchSelection
  | CreatePageHere Int
  | AdoptBankFromDevice Int
  | ToggleElideUnknown
  | RequestRead
  | RequestDeepRead
  | RequestToggleSession
  | SetSwitchHolds Int String
  -- Global switches
  | OpenGlobals
  | OpenGlobalSlot Int
  | PromoteSwitch Int
  | DissolveGlobal Int
  | DiscardGlobal Int
  -- Stamping one switch onto many pages
  | OpenStamp Int
  | CloseStamp
  | ToggleStampBank Int
  | ToggleStampAll
  | ApplyStamp

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
     , viewingGlobals: false
     , stampSlot: Nothing
     , stampTargets: []
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

-- | Two columns: the instrument on the left, what you are working on to the
-- | right.
-- |
-- | Stacked, the thirty cards pushed the actual work below the fold and the
-- | page only ever got shorter as you went deeper. Side by side, the selection
-- | cascade runs top-to-bottom in the right column while the instrument stays
-- | put — you can see which page you are on without scrolling back for it,
-- | which is the whole reason for drawing all thirty in the first place.
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "controls-view") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-columns") ]
        [ HH.div [ HP.class_ (H.ClassName "controls-col-instrument") ]
            -- Globals first. Underneath thirty bank cards it was below the fold
            -- whenever the survey was expanded, which put the one place a
            -- global is defined behind a scroll on exactly the screens where
            -- you would be looking for it.
            [ renderGlobalsCard state
            , renderSurvey state
            ]
        , HH.div [ HP.class_ (H.ClassName "controls-col-work") ]
            ( if state.viewingGlobals
                then [ renderGlobalsZone state ] <> switchZone
                else case state.selectedBankNumber of
                  Nothing ->
                    [ HH.p [ HP.class_ (H.ClassName "controls-empty") ]
                        [ HH.text "Pick a bank." ] ]
                  Just n -> [ renderBankZone state n ] <> switchZone
            )
        ]
    , if state.showDictionary then renderDictionaryOverlay state else HH.text ""
    , case state.stampSlot of
        Just slot -> renderStampOverlay state slot
        Nothing -> HH.text ""
    ]
  where
  switchZone = case state.selectedSwitchIdx of
    Just i -> [ renderSwitchZone state i ]
    Nothing -> []

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
    , onDeepRead: RequestDeepRead
    , sessionHeld: state.input.mc6SessionHeld
    , onToggleSession: RequestToggleSession
    , reading: state.input.mc6Reading
    , readAt: state.input.mc6ReadAt
    }

-- | The instrument's own switches, sitting where the instrument is.
-- |
-- | A card rather than a list, and always present rather than appearing once
-- | something has been promoted, because it is a *place* — the one place a
-- | global is defined. Making it come and go would leave the answer to "where
-- | do globals live" depending on whether any exist yet.
renderGlobalsCard :: forall m. State -> H.ComponentHTML Action () m
renderGlobalsCard state =
  HH.div
    [ HP.class_ (H.ClassName ("controls-globals-card"
        <> if state.viewingGlobals then " selected" else ""))
    , HE.onClick \_ -> OpenGlobals
    ]
    [ HH.div [ HP.class_ (H.ClassName "controls-globals-card-head") ]
        [ HH.h4_ [ HH.text "Globals" ]
        , HH.span [ HP.class_ (H.ClassName "controls-globals-card-sub") ]
            [ HH.text (case Array.length pages of
                0 -> "no pages yet"
                n -> "on all " <> show n <> " pages") ]
        ]
    , if Array.null globals
        then HH.p [ HP.class_ (H.ClassName "controls-globals-card-empty") ]
               [ HH.text "Nothing is global yet. Open a switch and promote it, or author one here." ]
        else HH.div_ (map row (Array.sortWith _.slot globals))
    ]
  where
  globals = state.input.globalSwitches
  pages = state.input.controlBanks
  row g =
    HH.div [ HP.class_ (H.ClassName "controls-globals-row") ]
      [ HH.span [ HP.class_ (H.ClassName "controls-globals-slot") ]
          [ HH.text (switchLetter g.slot) ]
      , HH.span [ HP.class_ (H.ClassName "controls-globals-label") ]
          [ HH.text (if g.label == "" then "\x2014" else g.label) ]
      ]

-- | Globals are written in first, so the survey draws the pages that will
-- | actually reach the device. Surveying the raw pages showed a hole where
-- | every global sits — and since the survey's universal-edge detection is what
-- | draws furniture faint, it was the one view most entitled to see it.
surveyCards :: State -> Array MC6Survey.BankCard
surveyCards state =
  MC6Survey.survey
    state.input.registry
    Board.boardRecallChannel
    -- NOT re-applied here. `intendedBanks` arrives with globals already in it,
    -- because the list that is written and the list that is checked have to be
    -- the same list — applying them on one side only is what made every bank
    -- the sweep wrote disagree at switch G.
    state.input.intendedBanks
    state.input.mc6NativeBanks
    state.input.mc6DumpedBanks
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
      mBank = effectiveBank state
      -- One numbering, zero-based, the same as the wire. Morningstar's editor
      -- counts from one; showing both here is what produced the off-by-ones it
      -- was meant to prevent.
      meta = "bank " <> show bankNum
               <> (case mCard of
                     Just c -> " \x00b7 " <> Survey.provenanceLabel c.provenance
                     Nothing -> "")
  in HH.div [ HP.class_ (H.ClassName "controls-zone controls-zone-bank") ]
    ( case mBank of
        Nothing ->
          [ HH.div [ HP.class_ (H.ClassName "controls-zone-head") ]
              [ backToInstrument
              , HH.h3_ [ HH.text ("Bank " <> show bankNum) ]
              , HH.span [ HP.class_ (H.ClassName "controls-zone-sub") ] [ HH.text meta ]
              ]
          , renderUnauthoredBank state mCard bankNum
          ]
        Just bank ->
          [ HH.div [ HP.class_ (H.ClassName "controls-bank-head") ]
              [ backToInstrument
              -- The bank's name IS its heading. A separate title line saying
              -- "Bank 22" alongside a field containing "Default Controls" was
              -- the smaller of the two facts taking the larger of the two
              -- sizes; the number is identification, not a name.
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.class_ (H.ClassName "controls-bank-name-input")
                  , HP.value state.editBankName
                  , HP.placeholder "Bank name"
                  , HE.onValueInput UpdateBankName
                  ]
              , HH.span [ HP.class_ (H.ClassName "controls-zone-sub") ] [ HH.text meta ]
              ]
          , HH.div [ HP.class_ (H.ClassName "controls-bank-bar") ]
              [ HH.input
                  [ HP.type_ HP.InputText
                  , HP.class_ (H.ClassName "controls-bank-notes-input")
                  , HP.value state.editBankDescription
                  , HP.placeholder "What this page is for"
                  , HE.onValueInput UpdateBankDescription
                  ]
              , HH.button
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
          , HH.div [ HP.class_ (H.ClassName "controls-bank-switches") ]
              (map (renderBankSwitchCell state bank mCard) Survey.physicalOrder)
          ]
    )

backToInstrument :: forall m. H.ComponentHTML Action () m
backToInstrument =
  HH.button
    [ HP.class_ (H.ClassName "controls-zone-back")
    , HE.onClick \_ -> ClearBankSelection
    ]
    [ HH.text "\x2190 MC6" ]

-- | The globals, laid out as a page — because that is how they are used.
-- |
-- | It looks like a bank and is not one: no wire number, no Sync, no Delete.
-- | The resemblance is the point, since a global occupies a slot underfoot
-- | exactly as a bank switch does, and this is the layout your foot already
-- | knows. What it does not have is the bank bar, because there is nothing
-- | here to send on its own — globals reach the device inside every page.
renderGlobalsZone :: forall m. State -> H.ComponentHTML Action () m
renderGlobalsZone state =
  HH.div [ HP.class_ (H.ClassName "controls-zone controls-zone-bank") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-bank-head") ]
        [ backToInstrument
        , HH.h3_ [ HH.text "Globals" ]
        , HH.span [ HP.class_ (H.ClassName "controls-zone-sub") ]
            [ HH.text ("written into all " <> show (Array.length state.input.controlBanks)
                        <> " pages at sync") ]
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-bank-bar") ]
        [ HH.p [ HP.class_ (H.ClassName "controls-globals-note") ]
            [ HH.text "A global is on every page or it is not a global. To make one page different, dissolve it \x2014 every page keeps a copy it owns, and you edit the odd one. To put a switch on some pages only, stamp it from that page instead." ]
        -- A global changes every page at once, so there is no per-page sync
        -- that could be the right one. This writes all of them.
        , HH.button
            [ HP.class_ (H.ClassName "controls-btn controls-btn-accent")
            , HE.onClick \_ -> SyncAllToMC6
            ]
            [ HH.text ("Write all " <> show (Array.length state.input.controlBanks) <> " pages") ]
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-bank-switches") ]
        (map (renderGlobalCell state) Survey.physicalOrder)
    ]

renderGlobalCell :: forall m. State -> Int -> H.ComponentHTML Action () m
renderGlobalCell state idx =
  let mG = Global.globalAt state.input.globalSwitches idx
      isSelected = state.selectedSwitchIdx == Just idx
      cls = String.joinWith " "
        ([ "controls-bank-switch" ]
          <> (if isSelected then [ "selected" ] else [])
          <> (if isJust mG then [ "global" ] else [ "vacant" ]))
  in HH.div
    [ HP.class_ (H.ClassName cls)
    , HE.onClick \_ -> OpenGlobalSlot idx
    ]
    [ HH.div [ HP.class_ (H.ClassName "controls-bank-switch-head") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-bank-switch-letter") ]
            [ HH.text (switchLetter idx) ]
        , case mG of
            Just _ -> HH.span [ HP.class_ (H.ClassName "controls-global-mark") ] [ HH.text "\x25c9" ]
            Nothing -> HH.text ""
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-bank-switch-label") ]
        [ HH.text (maybe "" _.label mG) ]
    , HH.div [ HP.class_ (H.ClassName "controls-bank-switch-verb") ]
        [ HH.text (case mG of
            Just g -> show (Array.length g.messages)
                        <> (if Array.length g.messages == 1 then " message" else " messages")
            Nothing -> "free on every page") ]
    ]

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

-- | A bank the device has but this app has never written.
-- |
-- | The offer to author here is deliberately guarded rather than a plain
-- | button. Now that reads work we can tell the difference between "bank 11 is
-- | free" and "bank 11 is somebody's LoopyPro page", and syncing over the
-- | second is the kind of loss that has already happened once — a generated
-- | looper bank landed on a hand-built one and kept its name, so nothing looked
-- | wrong.
renderUnauthoredBank
  :: forall m. State -> Maybe MC6Survey.BankCard -> Int
  -> H.ComponentHTML Action () m
renderUnauthoredBank state mCard bankNum =
  let occupied = case mCard of
        Just c -> c.name /= "" || not (Array.null (Array.filter (_ /= "") c.observedNames))
        Nothing -> false
      names = case mCard of
        Just c -> c.observedNames
        Nothing -> []
      known = not (Array.null (Array.filter (_ /= "") names))
      -- A dump gives messages, and messages are the difference between
      -- describing this bank and being able to reproduce it.
      mDumped = Array.find (\nb -> nb.bankNumber == bankNum) state.input.mc6DumpedBanks
      globals = state.input.globalSwitches
      -- Switches the device is using that a global would take over. This is the
      -- one place the globals rule destroys evidence rather than merely
      -- overruling intent, so it gets counted before the copy rather than
      -- regretted after it.
      displaced nb = Array.filter
        (\i -> isJust (Global.globalAt globals i) && deviceCarries nb i)
        (Array.range 0 (switchCount - 1))
      deviceCarries nb i = case Array.find (\p -> p.presetNum == i) nb.presets of
        Just p -> not (Array.null p.messages)
        Nothing -> false
  in HH.div [ HP.class_ (H.ClassName "controls-bank-empty") ]
    ( [ HH.p_ [ HH.text "This app has never written this bank." ] ]
        <> (if not known then [] else
              [ HH.p [ HP.class_ (H.ClassName "controls-observed-names") ]
                  [ HH.text ("The device says: "
                      <> String.joinWith "  \x00b7  " (Array.filter (_ /= "") names)) ]
              ])
        <> (case mDumped of
              -- We have read the messages. Adopting is now a faithful copy and
              -- syncing it back writes what is already there, so the warning that
              -- stood here for months no longer applies — and leaving it up would
              -- teach you to ignore it for the one case where it still does.
              Just nb ->
                [ HH.p [ HP.class_ (H.ClassName "controls-observed-names") ]
                    [ HH.text ("Read from the device in full: "
                        <> show (Array.length (Array.filter (\p -> not (Array.null p.messages)) nb.presets))
                        <> " of its switches carry messages. Taking a copy is exact \x2014 nothing is inferred, and syncing it back writes what is already there.") ]
                , renderDevicePreview state nb mCard
                ]
                <> (case displaced nb of
                      [] -> []
                      ds ->
                        [ HH.p [ HP.class_ (H.ClassName "controls-warn") ]
                            [ HH.text (String.joinWith ", " (map switchLetter ds)
                                <> (if Array.length ds == 1 then " is" else " are")
                                <> " doing something here that a global would take over, marked \x25c9 above. A page carries every global, so this is the copy where that work stops existing \x2014 and after it there is nothing left to read it back from. To keep one of them, dissolve that global first: every page then owns its copy and this one can differ.") ]
                        ])
              Nothing | occupied ->
                [ HH.p [ HP.class_ (H.ClassName "controls-warn") ]
                    [ HH.text "Something is already here, and we know its labels but not what its switches do. Read the whole device first \x2014 authoring from labels alone and syncing would replace working switches with silent ones, and the device will not warn you." ]
                ]
              Nothing -> [])
        <> [ HH.div [ HP.class_ (H.ClassName "controls-bank-empty-actions") ]
              ( (case mDumped of
                  Just _ ->
                    [ HH.button
                        [ HP.class_ (H.ClassName "controls-btn controls-btn-accent")
                        , HE.onClick \_ -> AdoptBankFromDevice bankNum
                        ]
                        [ HH.text "Take a copy of this bank" ]
                    ]
                  Nothing | known ->
                    [ HH.button
                        [ HP.class_ (H.ClassName "controls-btn-small")
                        , HE.onClick \_ -> AdoptBankFromDevice bankNum
                        ]
                        [ HH.text "Start from the device's labels" ]
                    ]
                  Nothing -> [])
                  <> [ HH.button
                         [ HP.class_ (H.ClassName ("controls-btn-small"
                             <> if occupied && mDumped == Nothing then " controls-btn-danger" else ""))
                         , HE.onClick \_ -> CreatePageHere bankNum
                         ]
                         [ HH.text "Start from an empty page" ]
                     ]
              )
           ]
    )

-- | The device's own twelve switches, before we take the bank over.
-- |
-- | This exists because of a hole the globals rule opens rather than for
-- | completeness. A global is unconditional — every page carries it — so the
-- | moment a bank becomes a page, whatever the device had at a global's slot is
-- | gone, and gone from the only copy that held it. The disagreement badge that
-- | appears afterwards reports *that* it changed and can never say what it was.
-- | So the last chance to look is here, and it is deliberately the same
-- | twelve-cell layout as the editor: recognising a switch by where it sits
-- | underfoot is how you judge whether losing it matters.
-- |
-- | Read-only, and not because editing would be hard. A third editable thing on
-- | this page — neither our page nor the device's bank — is exactly the
-- | ambiguity the globals redesign removed.
renderDevicePreview
  :: forall m. State -> MC6NativeBank -> Maybe MC6Survey.BankCard
  -> H.ComponentHTML Action () m
renderDevicePreview state nb mCard =
  HH.div [ HP.class_ (H.ClassName "controls-device-preview") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-device-preview-head") ]
        [ HH.text "On the device now" ]
    , HH.div [ HP.class_ (H.ClassName "controls-bank-switches") ]
        (map cell Survey.physicalOrder)
    ]
  where
  globals = state.input.globalSwitches
  cell idx =
    let mP = Array.find (\p -> p.presetNum == idx) nb.presets
        verb = mCard >>= \c -> Array.index c.slots idx
        count = maybe 0 (Array.length <<< _.messages) mP
        taken = isJust (Global.globalAt globals idx)
        cls = String.joinWith " "
          ([ "controls-bank-switch", "readonly" ]
            <> (if count == 0 then [ "vacant" ] else [])
            <> (if taken && count > 0 then [ "displaced" ] else []))
    in HH.div
      [ HP.class_ (H.ClassName cls)
      , HP.attr (HH.AttrName "style")
          ("border-left: 3px solid " <> maybe "#e6e6ea" Survey.verbColor verb)
      ]
      [ HH.div [ HP.class_ (H.ClassName "controls-bank-switch-head") ]
          [ HH.span [ HP.class_ (H.ClassName "controls-bank-switch-letter") ]
              [ HH.text (switchLetter idx) ]
          , if taken
              then HH.span
                     [ HP.class_ (H.ClassName "controls-global-mark")
                     , HP.title (if count > 0
                         then "a global takes this slot \x2014 what is here now will not survive the copy"
                         else "a global takes this slot, which is free on the device")
                     ]
                     [ HH.text "\x25c9" ]
              else HH.text ""
          ]
      , HH.div [ HP.class_ (H.ClassName "controls-bank-switch-label") ]
          [ HH.text (maybe "" _.shortName mP) ]
      , HH.div [ HP.class_ (H.ClassName "controls-bank-switch-verb") ]
          [ HH.text (if count == 0
              then "empty"
              else maybe "" Survey.verbName verb <> " \x00b7 " <> show count
                     <> (if count == 1 then " message" else " messages")) ]
      , case mP of
          Just p | p.longName /= "" && p.longName /= p.shortName ->
            HH.div [ HP.class_ (H.ClassName "controls-bank-switch-observed") ]
              [ HH.text p.longName ]
          _ -> HH.text ""
      ]

-- | One switch as it sits underfoot, as the device actually has it.
-- |
-- | **One view, not two.** This used to show the app's stored label in large
-- | type and append `device: X` in red whenever the two disagreed — so a bank
-- | the app had never authored showed a *different bank's* switches
-- | prominently, with the truth as a footnote. Bank 22 is the case that made it
-- | obvious: the looper family is generated in `Data.Looper.Banks` and written
-- | to the device, never stored, so the Controls page had no entry for it and
-- | fell back to whichever saved bank claimed that number. It could never come
-- | right, because the disagreement was not drift — it was a category the store
-- | does not model.
-- |
-- | So once the device has been read, what it says *is* the label. The card
-- | already took its verb from the survey rather than from the store, which
-- | made the mixture worse: device-derived colour under an app-derived name.
-- |
-- | The reasonable worry is reading an empty device and losing authored work.
-- | That is an argument for backup and recovery, which this app has, and not
-- | for rendering two truths at once and leaving the reader to arbitrate.
renderBankSwitchCell
  :: forall m. State -> ControlBank -> Maybe MC6Survey.BankCard -> Int
  -> H.ComponentHTML Action () m
renderBankSwitchCell state bank mCard idx =
  let mSw = Array.index bank.switches idx
      verb = mCard >>= \c -> Array.index c.slots idx
      -- What the device said this switch is called, when there is a reading.
      -- Empty means the device reported a blank switch, which is a fact and not
      -- an absence, so it stays.
      fromDevice = mCard >>= \c -> Array.index c.observedNames idx
      isSelected = state.selectedSwitchIdx == Just idx
      global = isJust (Global.globalAt state.input.globalSwitches idx)
      cls = String.joinWith " "
        ([ "controls-bank-switch" ]
          <> (if isSelected then [ "selected" ] else [])
          <> (if global then [ "global" ] else []))
  in HH.div
    [ HP.class_ (H.ClassName cls)
    , HP.attr (HH.AttrName "style")
        ("border-left: 3px solid " <> maybe "#e6e6ea" Survey.verbColor verb)
    , HE.onClick \_ -> SelectSwitch idx
    ]
    [ HH.div [ HP.class_ (H.ClassName "controls-bank-switch-head") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-bank-switch-letter") ]
            [ HH.text (switchLetter idx) ]
        , if global
            then HH.span [ HP.class_ (H.ClassName "controls-global-mark")
                         , HP.title "global \x2014 the same on every page" ]
                   [ HH.text "\x25c9" ]
            else HH.text ""
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-bank-switch-label") ]
        -- The device when it has been read, ours only when it has not.
        [ HH.text (fromMaybe (fromMaybe "" (mSw <#> _.label)) fromDevice) ]
    , case assignedBoard state bank.mc6BankNumber idx of
        Just bp -> HH.div [ HP.class_ (H.ClassName "controls-bank-switch-board") ]
          [ HH.text (bp.name <> "  " <> show (boardBudget state bp)
                      <> "/" <> show Board.messageLimit) ]
        Nothing -> HH.div [ HP.class_ (H.ClassName "controls-bank-switch-verb") ]
          [ HH.text (maybe "" Survey.verbName verb) ]
    ]

-- ──── Zoom 3: one switch ────

-- | The switch you are working on, on whichever surface you reached it from.
-- |
-- | Three surfaces, and each one can only do the thing that surface is for:
-- | the globals page edits globals, a bank page edits its own switches, and a
-- | global met on a bank page is shown and not edited. That last case is the
-- | whole design — the old version let you type into it and left you guessing
-- | whether the keystrokes were landing on this page or on thirty.
renderSwitchZone :: forall m. State -> Int -> H.ComponentHTML Action () m
renderSwitchZone state idx
  | state.viewingGlobals = renderGlobalEditor state idx
  | otherwise = case effectiveBank state of
      Nothing -> HH.text ""
      Just bank -> case Array.index bank.switches idx of
        Nothing -> HH.text ""
        Just sw ->
          let bankNum = bank.mc6BankNumber
              mBoard = assignedBoard state bankNum idx
          in switchShell (switchTitle idx sw.label) "\x2190 bank" ClearSwitchSelection $
            case Global.globalAt state.input.globalSwitches idx of
              Just g ->
                [ banner "global"
                    [ HH.text ("\x25c9 Global \x2014 the same on every page. This page shows it; it is edited in Globals.") ]
                    [ button "Edit global \x2192" (OpenGlobalSlot idx)
                    , button "Dissolve into every page" (DissolveGlobal idx)
                    ]
                , renderGlobalPreview g
                ]
              Nothing ->
                [ banner "local"
                    [ HH.text "Local to this page." ]
                    [ button "Make global" (PromoteSwitch idx)
                    , button "Copy to pages\x2026" (OpenStamp idx)
                    ]
                , renderHoldsSelector state bankNum idx mBoard
                , case mBoard of
                    Just bp -> renderBoardHeld state bp
                    Nothing ->
                      HH.div [ HP.class_ (H.ClassName "controls-switch-body") ]
                        [ renderSwitchSection (bankColor state.selectedBankIdx) idx sw
                        , renderSearchPanel state
                        ]
                ]

-- | A global, on the one surface that can change it.
-- |
-- | An empty slot is editable too: typing here authors a global from nothing,
-- | which is the other half of `promote`. Promoting is the ergonomic path —
-- | build it on a page where you can hear it, then say it is furniture — but a
-- | home you cannot write in is not really a home.
renderGlobalEditor :: forall m. State -> Int -> H.ComponentHTML Action () m
renderGlobalEditor state idx =
  let sw = globalSlotSwitch state idx
      exists = isJust (Global.globalAt state.input.globalSwitches idx)
      n = Array.length state.input.controlBanks
  in switchShell (switchTitle idx sw.label) "\x2190 globals" ClearSwitchSelection
    ( [ banner "global"
          [ HH.text (if exists
              then "\x25c9 Global \x2014 written into all " <> show n
                     <> " pages at sync. Editing here changes all of them."
              else "Nothing is global on " <> switchLetter idx
                     <> " yet. Anything you author here goes on every page.") ]
          (if exists
             then [ button "Dissolve \x2014 every page keeps a copy" (DissolveGlobal idx)
                  , button "Discard \x2014 pages keep their own" (DiscardGlobal idx)
                  ]
             else [])
      , HH.div [ HP.class_ (H.ClassName "controls-switch-body") ]
          [ renderSwitchSection "#7e22ce" idx sw
          , renderSearchPanel state
          ]
      ]
    )

-- | What a global holds, stated rather than offered for editing.
renderGlobalPreview :: forall m. GlobalSwitch -> H.ComponentHTML Action () m
renderGlobalPreview g =
  HH.div [ HP.class_ (H.ClassName "controls-global-preview") ]
    ( [ HH.div [ HP.class_ (H.ClassName "controls-global-preview-name") ]
          [ HH.text (g.label <> (if g.longName == "" then "" else "  \x00b7  " <> g.longName)
                      <> (if g.toToggle then "  \x00b7  toggle" else "")) ]
      ]
        <> (if Array.null g.messages
              then [ HH.div [ HP.class_ (H.ClassName "controls-global-preview-msg") ]
                       [ HH.text "no messages" ] ]
              else map line g.messages)
    )
  where
  line m =
    HH.div [ HP.class_ (H.ClassName "controls-global-preview-msg") ]
      [ HH.span [ HP.class_ (H.ClassName "controls-global-preview-type") ]
          [ HH.text (mc6MsgTypeLabel m.msgType) ]
      , HH.text (" ch " <> show m.channel <> "  " <> show m.data1 <> " " <> show m.data2)
      ]

switchTitle :: Int -> String -> String
switchTitle idx label =
  "Switch " <> switchLetter idx <> (if label == "" then "" else " \x00b7 " <> label)

switchShell
  :: forall m
   . String -> String -> Action -> Array (H.ComponentHTML Action () m)
  -> H.ComponentHTML Action () m
switchShell title backLabel backAction body =
  HH.div [ HP.class_ (H.ClassName "controls-zone controls-zone-switch") ]
    ( [ HH.div [ HP.class_ (H.ClassName "controls-zone-head") ]
          [ HH.button
              [ HP.class_ (H.ClassName "controls-zone-back")
              , HE.onClick \_ -> backAction
              ]
              [ HH.text backLabel ]
          , HH.h3_ [ HH.text title ]
          ]
      ] <> body
    )

-- | Where this switch is defined, said before the fields rather than after.
banner
  :: forall m
   . String -> Array (H.ComponentHTML Action () m) -> Array (H.ComponentHTML Action () m)
  -> H.ComponentHTML Action () m
banner kind text actions =
  HH.div [ HP.class_ (H.ClassName ("controls-scope-banner " <> kind)) ]
    ( [ HH.span [ HP.class_ (H.ClassName "controls-scope-text") ] text ] <> actions )

button :: forall m. String -> Action -> H.ComponentHTML Action () m
button label act =
  HH.button
    [ HP.class_ (H.ClassName "controls-btn-small"), HE.onClick \_ -> act ]
    [ HH.text label ]

-- | Which pages a stamp lands on.
-- |
-- | A stamp is the answer to everything a global refuses: most pages, or the
-- | five pages of one group. It writes copies and raises no link, so the switch
-- | stays local everywhere and can drift — which is exactly what you wanted
-- | when you reached for it instead of a global.
renderStampOverlay :: forall m. State -> Int -> H.ComponentHTML Action () m
renderStampOverlay state slot =
  HH.div [ HP.class_ (H.ClassName "controls-dict-overlay") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-stamp") ]
        [ HH.div [ HP.class_ (H.ClassName "controls-stamp-head") ]
            [ HH.h3_ [ HH.text ("Copy switch " <> switchLetter slot <> " to\x2026") ]
            , HH.button
                [ HP.class_ (H.ClassName "controls-btn-small"), HE.onClick \_ -> CloseStamp ]
                [ HH.text "Cancel" ]
            ]
        , HH.p [ HP.class_ (H.ClassName "controls-stamp-note") ]
            [ HH.text ("Each page gets its own copy on " <> switchLetter slot
                        <> ", replacing whatever is there. No link afterwards \x2014 change one later and the rest stay put.") ]
        , HH.div [ HP.class_ (H.ClassName "controls-stamp-bar") ]
            [ HH.button
                [ HP.class_ (H.ClassName "controls-btn-small"), HE.onClick \_ -> ToggleStampAll ]
                [ HH.text (if allPicked then "Clear all" else "All pages") ]
            , HH.span [ HP.class_ (H.ClassName "controls-stamp-count") ]
                [ HH.text (show (Array.length state.stampTargets) <> " selected") ]
            ]
        , HH.div [ HP.class_ (H.ClassName "controls-stamp-grid") ] (map cell state.input.controlBanks)
        , HH.div [ HP.class_ (H.ClassName "controls-stamp-foot") ]
            [ HH.button
                [ HP.class_ (H.ClassName "controls-btn controls-btn-accent")
                , HP.disabled (Array.null state.stampTargets)
                , HE.onClick \_ -> ApplyStamp
                ]
                [ HH.text ("Copy to " <> show (Array.length state.stampTargets)
                            <> (if Array.length state.stampTargets == 1 then " page" else " pages")) ]
            ]
        ]
    ]
  where
  allPicked = Array.length state.stampTargets == Array.length state.input.controlBanks
  cell cb =
    let picked = Array.elem cb.mc6BankNumber state.stampTargets
        here = Just cb.mc6BankNumber == state.selectedBankNumber
    in HH.div
      [ HP.class_ (H.ClassName ("controls-stamp-cell"
          <> (if picked then " picked" else "")
          <> (if here then " here" else "")))
      , HE.onClick \_ -> ToggleStampBank cb.mc6BankNumber
      ]
      [ HH.span [ HP.class_ (H.ClassName "controls-stamp-cell-num") ]
          [ HH.text (show cb.mc6BankNumber) ]
      , HH.span [ HP.class_ (H.ClassName "controls-stamp-cell-name") ]
          [ HH.text (if cb.name == "" then "\x2014" else cb.name) ]
      , HH.span [ HP.class_ (H.ClassName "controls-stamp-cell-was") ]
          [ HH.text (case Array.index cb.switches slot of
              Just sw | sw.label /= "" -> "replaces " <> sw.label
              _ -> "empty") ]
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

renderSwitchSection :: forall m. String -> Int -> ControlBankSwitch -> H.ComponentHTML Action () m
renderSwitchSection bankCol swIdx sw =
  let swColor = switchColor swIdx
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
                (Array.range 0 (switchCount - 1) <#> \i ->
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
    addMessages swIdx true (ccToggleMessages 1 0)
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
    -- The browser lives inside the switch you are editing now, so that is the
    -- target; browserTargetSwitch is only still consulted for the dictionary
    -- overlay, which can be opened without a switch selected.
    let target = fromMaybe st.browserTargetSwitch st.selectedSwitchIdx
    addMessages target isToggle
      (if isToggle then ccToggleMessages ch cc else ccMomentaryMessages ch cc)
    save

  SyncToMC6 -> do
    commitBankProps
    st <- H.get
    for_ (selectedBank st) (H.raise <<< SyncControlBankToMC6)

  SyncAllToMC6 -> do
    commitBankProps
    H.raise SyncAllBanksToMC6

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
      , viewingGlobals = false
      , selectedBankIdx = idx
      , editBankName = fromMaybe "" (mBank <#> _.name)
      , editBankNumber = fromMaybe (show n) (mBank <#> \b -> show b.mc6BankNumber)
      , editBankDescription = fromMaybe "" (mBank <#> _.description)
      , editReturnSwitch = fromMaybe 6 (mBank <#> _.returnSwitchIndex)
      }
    H.raise (JumpMC6ToBank n)

  ClearBankSelection -> do
    commitBankProps
    H.modify_ _ { selectedBankNumber = Nothing, selectedSwitchIdx = Nothing
                , viewingGlobals = false }

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
      , viewingGlobals = false
      , editBankName = newBank.name
      , editBankNumber = show n
      , editBankDescription = newBank.description
      , editReturnSwitch = newBank.returnSwitchIndex
      }

  -- Seed a new page from what the device reported. Labels only — a read brings
  -- back names and not messages — so the page arrives correct on the face of it
  -- and empty underneath, which is exactly the state the warning describes and
  -- exactly why every switch starts with no messages rather than a guess.
  AdoptBankFromDevice n -> do
    st <- H.get
    let names = case Array.find (\c -> c.bankNumber == n) (surveyCards st) of
          Just c -> c.observedNames
          Nothing -> []
        mDumped = Array.find (\nb -> nb.bankNumber == n) st.input.mc6DumpedBanks
        -- With a dump, the copy is exact: labels, long names, toggle mode and
        -- every message. Without one, labels are all there is, and the switch
        -- arrives silent — which is the state the panel's warning describes and
        -- the reason the two cases must not look alike.
        switches = case mDumped of
          Just nb -> Array.mapWithIndex (fromDumped nb) blanks
          Nothing -> Array.mapWithIndex
            (\i sw -> sw { label = fromMaybe "" (Array.index names i) }) blanks
        blanks = Array.replicate switchCount emptySwitch
        fromDumped nb i sw = case Array.find (\p -> p.presetNum == i) nb.presets of
          Just p ->
            { label: p.shortName
            , longName: p.longName
            , toToggle: p.toToggle
            , messages: p.messages
            }
          Nothing -> sw
        seeded = (emptyControlBank (bankTitle n) n) { switches = switches }
        bankTitle b = case Array.find (\c -> c.bankNumber == b) (surveyCards st) of
          Just c | c.name /= "" -> c.name
          _ -> "Bank " <> show b
        banks = Array.snoc st.input.controlBanks seeded
        newIdx = Array.length banks - 1
    H.raise (SaveControlBanks banks (Just newIdx))
    H.modify_ _
      { selectedBankIdx = newIdx
      , selectedBankNumber = Just n
      , selectedSwitchIdx = Nothing
      , viewingGlobals = false
      , editBankName = seeded.name
      , editBankNumber = show n
      , editBankDescription = seeded.description
      }

  ToggleElideUnknown -> H.modify_ \st -> st { elideUnknown = not st.elideUnknown }

  RequestRead -> H.raise ReadMC6

  RequestDeepRead -> H.raise DeepReadMC6
  RequestToggleSession -> H.raise ToggleMC6Session

  SetSwitchHolds idx boardId -> do
    st <- H.get
    for_ (selectedBank st) \bank ->
      if boardId == ""
        then H.raise (UnassignSwitch bank.mc6BankNumber idx)
        else H.raise (AssignBoard bank.mc6BankNumber idx boardId)

  OpenGlobals -> do
    commitBankProps
    H.modify_ _ { viewingGlobals = true, selectedSwitchIdx = Nothing }

  -- Reached both from the globals page and from "Edit global →" on a bank page,
  -- which is why it sets the surface as well as the slot: the second caller is
  -- standing somewhere else and the whole point of the jump is to move.
  OpenGlobalSlot idx -> do
    commitBankProps
    H.modify_ _ { viewingGlobals = true, selectedSwitchIdx = Just idx }

  -- Promote what is already on this switch. Building the thing once on a page
  -- where you can hear it and then saying "this is furniture" is the workflow
  -- that actually happens; an empty form you fill in from nothing is not.
  PromoteSwitch idx -> do
    st <- H.get
    for_ (selectedBank st) \bank ->
      for_ (Array.index bank.switches idx) \sw ->
        H.raise (SaveGlobalSwitches (Global.promote idx sw st.input.globalSwitches))

  -- The only way out of a global, and the whole way out: every page keeps a
  -- copy it owns, so the exception you wanted is now an ordinary edit on one
  -- page. Costs the link, which is the honest price and is paid once.
  DissolveGlobal idx -> do
    st <- H.get
    let r = Global.dissolve idx st.input.globalSwitches st.input.controlBanks
    H.modify_ _ { input = st.input { globalSwitches = r.globals, controlBanks = r.banks }
                , viewingGlobals = false
                }
    H.raise (SaveGlobalSwitches r.globals)
    H.raise (SaveControlBanks r.banks (Just st.selectedBankIdx))

  -- The undo for promoting. Writes nothing, because promoting wrote nothing:
  -- every page still holds whatever its own slot held before, and gets it back.
  DiscardGlobal idx -> do
    st <- H.get
    let globals = Global.discard idx st.input.globalSwitches
    H.modify_ _ { input = st.input { globalSwitches = globals } }
    H.raise (SaveGlobalSwitches globals)

  OpenStamp idx ->
    -- Opens with the page you are on already ticked: you are stamping *this*
    -- switch, so the page it came from is not a choice.
    H.modify_ \st -> st
      { stampSlot = Just idx
      , stampTargets = case st.selectedBankNumber of
          Just n -> [ n ]
          Nothing -> []
      }

  CloseStamp -> H.modify_ _ { stampSlot = Nothing, stampTargets = [] }

  ToggleStampBank n -> H.modify_ \st -> st
    { stampTargets = if Array.elem n st.stampTargets
        then Array.filter (_ /= n) st.stampTargets
        else Array.snoc st.stampTargets n
    }

  ToggleStampAll -> H.modify_ \st -> st
    { stampTargets =
        if Array.length st.stampTargets == Array.length st.input.controlBanks
          then []
          else map _.mc6BankNumber st.input.controlBanks
    }

  ApplyStamp -> do
    st <- H.get
    for_ st.stampSlot \slot ->
      for_ (selectedBank st) \bank ->
        for_ (Array.index bank.switches slot) \sw -> do
          let banks = Global.stampTo slot sw st.stampTargets st.input.controlBanks
          H.modify_ _ { input = st.input { controlBanks = banks }
                      , stampSlot = Nothing
                      , stampTargets = []
                      }
          H.raise (SaveControlBanks banks (Just st.selectedBankIdx))

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

emptyControlBank :: String -> Int -> ControlBank
emptyControlBank name bankNum =
  { id: "bank-" <> show bankNum
  , name
  , description: ""
  , mc6BankNumber: bankNum
  , returnSwitchIndex: 6
  , switches: Array.replicate switchCount emptySwitch
  }

-- | Modify a switch on whichever surface is open.
-- |
-- | The routing is by *surface*, not by what the slot happens to hold: on the
-- | globals page an edit is a global edit, on a bank page it is a page edit,
-- | and a bank page never offers the editor for a global slot in the first
-- | place. That is what makes the destination knowable before you type — the
-- | previous version decided per slot, so the same field meant different things
-- | on different switches of the same page.
modifySwitch :: forall m. MonadAff m => Int -> (ControlBankSwitch -> ControlBankSwitch) -> H.HalogenM State Action () Output m Unit
modifySwitch swIdx f = do
  st <- H.get
  if st.viewingGlobals
    then do
      -- Editing an empty slot here authors a global, which is why this upserts
      -- rather than requiring one to exist.
      let globals = Global.promote swIdx (f (globalSlotSwitch st swIdx)) st.input.globalSwitches
      H.modify_ _ { input = st.input { globalSwitches = globals } }
      H.raise (SaveGlobalSwitches globals)
    else case Array.index st.input.controlBanks st.selectedBankIdx of
      Nothing -> pure unit
      Just bank -> do
        let newSwitches = fromMaybe bank.switches (Array.modifyAt swIdx f bank.switches)
            newBank = bank { switches = newSwitches }
            newBanks = fromMaybe st.input.controlBanks (Array.updateAt st.selectedBankIdx newBank st.input.controlBanks)
        H.modify_ _ { input = st.input { controlBanks = newBanks } }

-- | What a globals slot currently holds, empty included.
globalSlotSwitch :: State -> Int -> ControlBankSwitch
globalSlotSwitch st slot =
  maybe emptySwitch Global.toSwitch (Global.globalAt st.input.globalSwitches slot)

-- | The page as it will actually reach the device: globals written in.
-- |
-- | Rendering goes through this and mutation goes through `selectedBank`, which
-- | is the whole trick — what you see is the compiled page, what you edit is
-- | whichever source owns that slot.
effectiveBank :: State -> Maybe ControlBank
effectiveBank st = Global.applyGlobals st.input.globalSwitches <$> selectedBank st

-- | Append messages to a switch, putting it into toggle mode if they need it.
-- |
-- | A CC toggle pair is two messages carrying Tg On and Tg Off, and the MC6
-- | alternates between them **only if the preset is in toggle mode**. Adding
-- | the pair without setting the flag produced a switch that looked correct in
-- | the editor and sent both messages on every press — 127 then 0 — so the
-- | pedal ended up back where it started and the switch appeared to do nothing.
-- | Silent, and exactly the failure this app exists to stop.
-- |
-- | Never clears the flag: a switch can carry several messages and one toggle
-- | pair among them is enough to need the mode.
addMessages :: forall m. MonadAff m => Int -> Boolean -> Array MC6Message -> H.HalogenM State Action () Output m Unit
addMessages swIdx needsToggle msgs =
  modifySwitch swIdx \sw -> sw
    { messages = reindexMessages (sw.messages <> msgs)
    , toToggle = sw.toToggle || needsToggle
    }

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
