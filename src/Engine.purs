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
  , defaultPedalState
  ) where

import Prelude

import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Config.Types (MidiRouting)
import Data.MC6.ControlBank (ControlBank, exampleControlBank)
import Data.MC6.Types (MC6NativeBank)
import Data.Map (Map)
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
  , presets :: Array PedalPreset
  , boardPresets :: Array BoardPreset
  , registry :: PedalRegistry
  , configError :: Maybe String
  , mc6Banks :: Array MC6NativeBank
  , mc6BoardBankNum :: Int
  , mc6Assignments :: Array MC6Assignment
  , controlBanks :: Array ControlBank
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
  -- Result of the last looper-bank programming run, shown on the Looper page.
  , looperProgramStatus :: Maybe String
  , midiTest :: Maybe String
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
  , presets: []
  , boardPresets: []
  , configError: Nothing
  , mc6Banks: []
  , mc6BoardBankNum: 1
  , mc6Assignments: []
  , controlBanks: [exampleControlBank]
  , activeControlBankIdx: Just 0
  , looper: Nothing
  , looperStatus: { connected: false, everConnected: false, lastError: "", url: "" }
  , mc6LooperBankNum: 21
  , looperProgramStatus: Nothing
  , midiTest: Nothing
  , baselineStatus: Nothing
  , testCh: 3
  , testCC: 1
  , mc6DiagBankNum: 30
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
