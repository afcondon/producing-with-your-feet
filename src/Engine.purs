module Engine
  ( PedalState
  , EngineState
  , MidiConnections
  , View(..)
  , AppState
  , MC6Assignment
  , initEngineFromPedals
  , initAppState
  , getValue
  , getInfo
  , pedalState
  , pedalsOnChannel
  , defaultPedalState
  ) where

import Prelude

import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Config.Types (MidiRouting)
import Data.MC6.Global (GlobalSwitch)
import Data.MC6.ControlBank (ControlBank, exampleControlBank)
import Data.MC6.Dump as Dump
import Data.MC6.Settings as Settings
import Data.Looper.Banks as LooperBanks
import Data.MC6.Types (MC6NativeBank)
import Data.MC6.Wire as Wire
import Data.Array as Array
import Effect (Effect)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set as Set
import Halogen as H
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.LooperSocket (LooperState, SocketStatus)
import Data.Midi (CC, MidiValue)
import Data.Pedal (PedalDef, PedalId)
import Data.Preset (BoardPreset, PedalPreset, PresetId)
import Data.Tuple (Tuple(..))
import Foreign.WebMIDI (MIDIAccess, MIDIInput, MIDIOutput, MidiPort)

type PedalState =
  { channel :: Int
  , values :: Map CC MidiValue
  , info :: Map String Int
  }

type EngineState = Map PedalId PedalState

data View = GridView | DetailView PedalId | PedalView PedalId | OverviewView | BoardsView | ControlsView | LooperView | FilesView | ConnectView

derive instance Eq View

type MidiConnections =
  { access :: Maybe MIDIAccess
  , pedalOutput :: Maybe MIDIOutput
  , pedalOutputId :: Maybe String
  , twisterInput :: Maybe MIDIInput
  , twisterInputId :: Maybe String
  , twisterOutput :: Maybe MIDIOutput
  , twisterOutputId :: Maybe String
  , mc6Input :: Maybe MIDIInput
  , mc6InputId :: Maybe String
  , mc6Output :: Maybe MIDIOutput
  , mc6OutputId :: Maybe String
  , availableOutputs :: Array MidiPort
  , availableInputs :: Array MidiPort
  -- | Live subscriptions to the two inputs, so re-opening a port that came back
  -- | can tear down the old one first. Without this a reconnect would leave the
  -- | previous subscription in place and every footswitch press would arrive
  -- | twice — which for a looper means record-then-immediately-close.
  , mc6InputSub :: Maybe H.SubscriptionId
  , twisterInputSub :: Maybe H.SubscriptionId
  }

type MC6Assignment =
  { bankNumber :: Int
  , switchIndex :: Int
  , boardPresetId :: PresetId
  }

type AppState =
  { view :: View
  , engine :: EngineState
  , connections :: MidiConnections
  , cardOrder :: Array PedalId
  , hiddenPedals :: Array PedalId
  , focusPedalId :: Maybe PedalId
  , boardsActivePedal :: Maybe PedalId
  , overviewActivePedal :: Maybe PedalId
  , suppressTwister :: Boolean
  -- | What each of the Twister's lights was last told to be, keyed by the CC
  -- | that addresses it.
  -- |
  -- | **Not a belief about the device — a record of what we said.** The lights
  -- | themselves are computed fresh from the daemon's snapshot every poll; this
  -- | exists only so the app can send the ones that changed. Bank one's rings
  -- | are playheads and move every frame, and writing all sixty-four lights ten
  -- | times a second would put over a thousand messages a second on a wire that
  -- | also carries twelve pedals.
  -- |
  -- | Cleared whenever the device is dimmed, because what it was told is no
  -- | longer true of it.
  , twisterLit :: Map Int { ring :: Int, hue :: Int }
  -- | A turn that has arrived and is waiting to find out whether it was part of
  -- | a press. Keyed by the encoder's CC; the value is its newest position.
  -- |
  -- | **Because you cannot press one of these without turning it.** The
  -- | Midifighter's encoders rotate a little on the way down, so a press sends a
  -- | nudge as well — and the value under a loop's press is exactly the value
  -- | that nudge would move. See `Component.App.handleTwisterMsg`.
  , twisterPending :: Map Int Int
  -- | Encoders whose turns are being ignored because a press just landed on
  -- | them. Cleared on a timer.
  , twisterGuard :: Set Int
  -- | Which page the Twister last spoke *from* — an observation, read off the
  -- | wire. The bank travels in every encoder message, so this is what the
  -- | device believes. `Nothing` until it says something.
  , twisterHeardBank :: Maybe Int
  -- | Which page the app is **showing** — its own, and authoritative.
  -- |
  -- | **Two fields because there are two facts, and conflating them was the
  -- | mistake.** `Data.Twister` originally argued that the device owns the page
  -- | and the CC carries it, which is a fine rule for *reading* and no use at
  -- | all for getting back: it left the only way between pages on a side button
  -- | whose behaviour nobody here can verify, and Andrew reported the obvious
  -- | consequence — stuck on page 2 with no way home.
  -- |
  -- | So the app decides which table an encoder is read against, and the device
  -- | is merely *asked* to follow. If it cannot, paging still works; the two
  -- | facts simply differ, and the LED writes go to the block the device is
  -- | really showing (`twisterHeardBank`) carrying the content of the page the
  -- | app is on. Address from the device, content from the app.
  -- |
  -- | The device still wins when it *moves*: a change in the heard bank is an
  -- | event and is adopted. A heard bank that merely stays where it is is not,
  -- | which is what lets the app page a device parked on bank 1 for ever.
  , twisterPage :: Int
  , presets :: Array PedalPreset
  , boardPresets :: Array BoardPreset
  , registry :: PedalRegistry
  , configError :: Maybe String
  , mc6Banks :: Array MC6NativeBank
  , mc6BoardBankNum :: Int
  , mc6Assignments :: Array MC6Assignment
  -- | Which bank the MC6 says it is sitting on, from the `09 01` frame it
  -- | volunteers whenever that changes. The one piece of device state this app
  -- | does not have to believe — and what makes walking the banks checkable
  -- | rather than hopeful.
  , mc6CurrentBank :: Maybe Int
  -- | An editor session held open across many actions, rather than opened and
  -- | closed around each one.
  --
  -- | The device will not change bank for us at all without a session, so
  -- | anything that jumps banks while the board is being played needs one of
  -- | these. Holding it is only safe with "load preset data using switch press"
  -- | turned off, which is what `HoldMC6Session` does on the way in and undoes
  -- | on the way out: with that setting on, an open session blocks MIDI clock
  -- | and the MC6's own bank jump.
  --
  -- | `Wire.Open` is opaque and only `Wire.openSession` produces one, so this
  -- | field is the app's evidence that a session exists rather than a flag
  -- | asserting it.
  , mc6Held :: Maybe Wire.Open
  -- | Whether the device says it is in editor mode. `Nothing` until it has told
  -- | us — which is not the same as "off", and the difference is the whole
  -- | point: a session opened by Morningstar's editor in another tab is a thing
  -- | we can only learn about by being told.
  , mc6EditorMode :: Maybe Boolean
  -- | Removes the page-unload handler that releases a held session. Present
  -- | exactly when a session is held; a handler left installed after release
  -- | would disconnect a session that is already closed.
  , mc6UnloadGuard :: Maybe (Effect Unit)
  , controlBanks :: Array ControlBank
  , globalSwitches :: Array GlobalSwitch
  , activeControlBankIdx :: Maybe Int
  -- Folder backup (Chrome File System Access API → Infovore path)
  -- Result of the last manual MIDI test, shown on the MIDI page.
  -- Looper daemon (looper/ in this repo), over a socket. The app holds only
  -- what the daemon last reported; it never models the engine itself.
  , looper :: Maybe LooperState
  , looperStatus :: SocketStatus
  -- MC6 bank the generated looper transport is written to. Itajara's CCs are
  -- fixed by its pedal definition, so there is no base-CC to configure.
  , mc6LooperBankNum :: Int
  -- | First of the consecutive banks the six-loop machine occupies
  -- | (`Data.Looper.Banks`, one per `BankSlot`). One number rather than seven,
  -- | because "which bank is the speed bank" should be arithmetic and not a
  -- | setting that can be set inconsistently.
  , mc6LoopBankBase :: Int
  -- | How long the MC6 sat on a gesture before telling us, in milliseconds.
  -- |
  -- | **An estimate, and the one thing device-side recognition cost.** When the
  -- | app timed the switch edges it knew exactly when the foot went down, and
  -- | stamped every command with the gap so the daemon could reach back into the
  -- | pre-roll ring and un-do the delay. The device sends one message and no
  -- | timestamp, so that number now has to be reconstructed.
  -- |
  -- | It is reconstructible because the deferral is the device's own threshold,
  -- | not a variable delay: a tap cannot be emitted until the double-tap window
  -- | has passed without a second press, and a long press fires at its
  -- | threshold. Both are settings on the board.
  -- |
  -- | Both figures here are honest about their standing. The hold is the
  -- | device's own long-press time and is exact. The tap is **under** the truth
  -- | by however long the foot was on the switch, which nothing tells us — a
  -- | known, bounded, one-sided error, which is better than the zero it would
  -- | otherwise be and worse than the measurement it replaces.
  , looperDeferral :: { tapMs :: Number, holdMs :: Number }
  -- | The loop the config bank acts on — the last one a foot touched.
  -- | How old the newest snapshot is, in milliseconds, rounded so that it does
  -- | not re-render the page on every tick. Shown when it gets large, because a
  -- | frozen picture under a "connected" banner is the failure this whole
  -- | surface keeps meeting.
  , looperSnapshotAge :: Number
  , looperFocus :: Int
  -- | What the last footswitch press did, in words. Every press produces one,
  -- | including the refusals: a press that leaves no trace anywhere is the
  -- | thing the whole looper surface exists to prevent.
  , looperLastAction :: Maybe String
  -- | The `ackSeq` of the last thing the daemon said that we have shown.
  -- |
  -- | A counter rather than the text, because two identical refusals in a row
  -- | are two refusals: pressing the same dead switch twice should say so
  -- | twice, and comparing the sentences would swallow the second.
  , looperAckSeq :: Int
  -- | Where the gesture probe bank goes.
  -- |
  -- | **Below the family rather than above it.** It sat on 28 while the looper
  -- | occupied 22-27; the loop page made that seven banks, 22-28, and the probe
  -- | was standing on the last one. Twenty is empty on the device and there is
  -- | nothing above 28 to move to — 29 is Ableton Controls and the device stops
  -- | there.
  -- |
  -- | That reasoning was right and the conclusion was wrong: twenty was NOT
  -- | empty — the default control bank was already on it. Two places claimed
  -- | one bank for the second time, and again nothing but a read-back could
  -- | have said so.
  -- |
  -- | The table that was owed now exists: **`Data.MC6.Reserved`**, which
  -- | enumerates every claim from these numbers and checks it against the
  -- | user's control banks. `test/Main` runs it over these defaults, so a third
  -- | occurrence is a failing test rather than a bank you find with your foot.
  -- | Change a number here and read the map there.
  , mc6ProbeBankNum :: Int
  -- | Which face the Looper page is showing. The six-slot display is what the
  -- | board drives; the old transport is kept because it can drive the engine
  -- | by hand, which is how the six-slot display gets something to show.
  , looperShowsSlots :: Boolean
  -- | Which of the six looper banks the MC6 is *showing*.
  -- |
  -- | Not asked for and not remembered from a bank change we commanded: taken
  -- | from the presses themselves, because the switch namespace says which bank
  -- | every press came from. That is the whole point of encoding the bank in
  -- | the CC number, and it means the app cannot be out of step with the board
  -- | for longer than one press — including after a bank change the player made
  -- | with their foot, which nothing told the app about.
  -- |
  -- | It matters because G to L have no markings, so the screen is the only
  -- | thing that can say what they do — and saying it for the wrong bank is
  -- | worse than saying nothing.
  , looperBankShown :: Maybe LooperBanks.BankSlot
  -- Result of the last looper-bank programming run, shown on the Looper page.
  , looperProgramStatus :: Maybe String
  , midiTest :: Maybe String
  -- | What the MC6 said when last asked (`Data.MC6.Read`). Bank numbers are
  -- | wire numbers, i.e. 0-based; the editor shows them one higher.
  , mc6BankNames :: Map Int String
  , mc6BankSwitches :: Map Int (Array String)
  -- | Which whole-map sweep this is, counting from the first one this browser
  -- | ever ran. `Data.MC6.Stamp` writes it onto every bank it touches, so a
  -- | bank on the device can say which run put it there — the difference
  -- | between "still good from last time" and "written just now", which no
  -- | fixed marker could tell apart. Persisted (`Storage.loadSweepRun`),
  -- | because a reload between writing and reading must not make the whole
  -- | survey disagree.
  , sweepRun :: Int
  -- | When the device last told us the above. Stored with the reading, because
  -- | a persisted observation is only as good as its date and the alternative is
  -- | a map that looks like fresh truth forever.
  , mc6ReadAt :: Maybe String
  -- | Every preset the last dump returned, messages included. Held apart from
  -- | `mc6BankNames`/`mc6BankSwitches` because those are labels and this is
  -- | behaviour: one can be shown, only the other can be sent back.
  , mc6DumpedBanks :: Array MC6NativeBank
  -- | Raw dump frames as they arrive, before being gathered into banks.
  , mc6DumpedPresets :: Array Dump.DumpPreset
  -- | Everything the device says about itself that is not a preset, by `03 2x`
  -- | sub-code (`Data.MC6.Settings`). Volunteered on every connect, so this
  -- | fills itself without being asked — and it is the only place in the app
  -- | that knows facts like which channels the *device* considers spoken for.
  , mc6Settings :: Map Int Settings.Section
  -- | Every SysEx frame the device has sent, tallied by function code. Exists so
  -- | that "the request returned nothing" can be answered with what *did*
  -- | arrive, without needing a browser console to find out.
  , mc6FrameCounts :: Map String Int
  -- | Dump frames received, counted whether or not they decoded. Progress has to
  -- | be independent of understanding, or a decoder bug reads as a silent device.
  , mc6DumpFrames :: Int
  -- | Set by the device's own end-of-dump frame. The alternative — waiting for a
  -- | gap in the stream — is a guess, and it truncated a read once already.
  , mc6DumpDone :: Boolean
  -- | True while the device is being walked. A read is minutes long, so the
  -- | fact that one is running is state the whole UI needs.
  , mc6Reading :: Boolean
  , mc6ReadStatus :: Maybe String
  -- | Outcome of the last baseline sweep, shown on the pedal card that
  -- | triggered it. Separate from `midiTest` because that changes on every CC
  -- | from anywhere and would flicker noise into the card.
  , baselineStatus :: Maybe String
  -- Manual CC test on the MIDI page: channel and CC to poke at the rig.
  , testCh :: Int
  , testCC :: Int
  -- First MC6 bank the generated diagnostic banks are written to.
  , mc6DiagBankNum :: Int
  , backupFolderName :: Maybe String
  , backupLastSaveAt :: Maybe String
  , backupLastError :: Maybe String
  }

defaultPedalState :: PedalDef -> PedalState
defaultPedalState def =
  { channel: def.meta.defaultChannel
  , values: def.baseline
  , info: Map.empty
  }

initEngineFromPedals :: Array PedalDef -> EngineState
initEngineFromPedals pedals = Map.fromFoldable $
  map (\def -> Tuple def.meta.id (defaultPedalState def)) pedals

emptyRouting :: MidiRouting
emptyRouting =
  { pedalOutput: { match: "" }
  , twisterInput: { match: "" }
  , twisterOutput: { match: "" }
  , mc6Input: { match: "" }
  }

initAppState :: AppState
initAppState =
  { view: OverviewView
  , engine: Map.empty
  , registry: CRegistry.mkRegistry [] [] emptyRouting
  , connections:
      { access: Nothing
      , pedalOutput: Nothing
      , pedalOutputId: Nothing
      , twisterInput: Nothing
      , twisterInputId: Nothing
      , twisterOutput: Nothing
      , twisterOutputId: Nothing
      , mc6Input: Nothing
      , mc6InputId: Nothing
      , mc6Output: Nothing
      , mc6OutputId: Nothing
      , availableOutputs: []
      , availableInputs: []
      , mc6InputSub: Nothing
      , twisterInputSub: Nothing
      }
  , cardOrder: []
  , hiddenPedals: []
  , focusPedalId: Nothing
  , boardsActivePedal: Nothing
  , overviewActivePedal: Nothing
  , suppressTwister: false
  , twisterLit: Map.empty
  , twisterPending: Map.empty
  , twisterGuard: Set.empty
  , twisterHeardBank: Nothing
  , twisterPage: 0
  , presets: []
  , boardPresets: []
  , configError: Nothing
  , mc6Banks: []
  , mc6BoardBankNum: 1
  , mc6Assignments: []
  , mc6CurrentBank: Nothing
  , mc6Held: Nothing
  , mc6EditorMode: Nothing
  , mc6UnloadGuard: Nothing
  , controlBanks: [exampleControlBank]
  , globalSwitches: []
  , activeControlBankIdx: Just 0
  , looper: Nothing
  , looperStatus: { connected: false, everConnected: false, lastError: "", url: "" }
  , mc6LooperBankNum: 9
  -- 22-28 (seven slots, not the six this comment used to claim), just above the
  -- legacy transport bank. Wire numbers; the editor shows each one higher.
  -- `Data.MC6.Reserved` derives the seven from this base rather than restating
  -- them, so the range cannot be written down wrongly a second time.
  , mc6LoopBankBase: 2
  -- **Seven hundred, because that is what the device does.** Set in
  -- Morningstar's editor and confirmed against `03 21` offset 3, which moved
  -- from 2 to 4 when the setting went from 750 ms to 700 — so the byte is the
  -- right byte even though one data point gives no scale to read it by.
  --
  -- Two-fifty for the tap is the weaker number and the more important one, and
  -- it is the next thing to measure rather than to reason about: the double-tap
  -- window was bounded from above at 414 ms by the gesture probe (two presses
  -- that far apart read as two singles) and has never been pinned. A press log
  -- against a metronome would settle it in a minute.
  , looperDeferral: { tapMs: 250.0, holdMs: 700.0 }
  , looperSnapshotAge: 0.0
  , looperFocus: 0
  , looperLastAction: Nothing
  , looperAckSeq: 0
  , mc6ProbeBankNum: 10
  , looperShowsSlots: true
  , looperBankShown: Just LooperBanks.LoopBank
  , looperProgramStatus: Nothing
  , midiTest: Nothing
  , mc6BankNames: Map.empty
  , mc6BankSwitches: Map.empty
  , sweepRun: 0
  , mc6ReadAt: Nothing
  , mc6DumpedBanks: []
  , mc6DumpedPresets: []
  , mc6Settings: Map.empty
  , mc6FrameCounts: Map.empty
  , mc6DumpFrames: 0
  , mc6DumpDone: false
  , mc6Reading: false
  , mc6ReadStatus: Nothing
  , baselineStatus: Nothing
  , testCh: 3
  , testCC: 1
  , mc6DiagBankNum: 11
  , backupFolderName: Nothing
  , backupLastSaveAt: Nothing
  , backupLastError: Nothing
  }

getValue :: PedalId -> CC -> EngineState -> Maybe MidiValue
getValue pid ccNum engine = do
  ps <- Map.lookup pid engine
  Map.lookup ccNum ps.values

getInfo :: PedalId -> String -> EngineState -> Maybe Int
getInfo pid key engine = do
  ps <- Map.lookup pid engine
  Map.lookup key ps.info

pedalState :: PedalId -> EngineState -> Maybe PedalState
pedalState = Map.lookup

-- | Which pedals answer on this MIDI channel.
-- |
-- | Reads the engine rather than the registry's declared default, because a
-- | pedal's channel can be changed at runtime and callers of this are matching
-- | against what the hardware is actually doing.
-- |
-- | Returns an array because nothing forbids two pedals sharing a channel.
-- | Silently picking one would be a bug that only ever shows up on the rig,
-- | which is the worst place to find it.
pedalsOnChannel :: Int -> EngineState -> Array PedalId
pedalsOnChannel channel engine =
  Array.mapMaybe
    (\(Tuple pid ps) -> if ps.channel == channel then Just pid else Nothing)
    (Map.toUnfoldable engine)
