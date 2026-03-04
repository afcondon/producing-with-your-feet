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
import Data.Array as Array
import Data.Loopy as Loopy
import Data.MC6.ControlBank (ControlBank, exampleControlBank)
import Data.MC6.Types (MC6NativeBank)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
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

data View = GridView | DetailView PedalId | PedalView PedalId | OverviewView | BoardsView | ControlsView | FilesView | DocsView | ConnectView

derive instance Eq View

type MidiConnections =
  { access :: Maybe MIDIAccess
  , pedalOutput :: Maybe MIDIOutput
  , pedalOutputId :: Maybe String
  , twisterInput :: Maybe MIDIInput
  , twisterInputId :: Maybe String
  , twisterOutput :: Maybe MIDIOutput
  , twisterOutputId :: Maybe String
  , loopyOutput :: Maybe MIDIOutput
  , loopyOutputId :: Maybe String
  , mc6Input :: Maybe MIDIInput
  , mc6InputId :: Maybe String
  , mc6Output :: Maybe MIDIOutput
  , mc6OutputId :: Maybe String
  , availableOutputs :: Array MidiPort
  , availableInputs :: Array MidiPort
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
  , suppressTwister :: Boolean
  , presets :: Array PedalPreset
  , boardPresets :: Array BoardPreset
  , registry :: PedalRegistry
  , configError :: Maybe String
  , loopyTwisterActive :: Boolean
  , loopySelectedLoop :: Int
  , loopyHeldEncoder :: Maybe Int
  , loopyLoopStates :: Array Loopy.LoopState
  , loopyClipSettings :: Array Loopy.ClipSettings
  , mc6Banks :: Array MC6NativeBank
  , mc6BoardBankNum :: Int
  , mc6Assignments :: Array MC6Assignment
  , controlBanks :: Array ControlBank
  , activeControlBankIdx :: Maybe Int
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
  , loopyOutput: { match: "" }
  , loopyChannel: 1
  , mc6Input: { match: "" }
  }

initAppState :: AppState
initAppState =
  { view: GridView
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
      , loopyOutput: Nothing
      , loopyOutputId: Nothing
      , mc6Input: Nothing
      , mc6InputId: Nothing
      , mc6Output: Nothing
      , mc6OutputId: Nothing
      , availableOutputs: []
      , availableInputs: []
      }
  , cardOrder: []
  , hiddenPedals: []
  , focusPedalId: Nothing
  , boardsActivePedal: Nothing
  , suppressTwister: false
  , presets: []
  , boardPresets: []
  , configError: Nothing
  , loopyTwisterActive: false
  , loopySelectedLoop: -1
  , loopyHeldEncoder: Nothing
  , loopyLoopStates: Array.replicate 8 Loopy.defaultLoopState
  , loopyClipSettings: Array.replicate 8 Loopy.defaultClipSettings
  , mc6Banks: []
  , mc6BoardBankNum: 1
  , mc6Assignments: []
  , controlBanks: [exampleControlBank]
  , activeControlBankIdx: Just 0
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
