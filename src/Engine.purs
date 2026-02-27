module Engine
  ( PedalState
  , EngineState
  , MidiConnections
  , View(..)
  , AppState
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
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue)
import Data.Pedal (PedalDef, PedalId)
import Data.Preset (BoardPreset, PedalPreset)
import Data.Tuple (Tuple(..))
import Foreign.WebMIDI (MIDIAccess, MIDIInput, MIDIOutput, MidiPort)

type PedalState =
  { channel :: Int
  , values :: Map CC MidiValue
  , info :: Map String Int
  }

type EngineState = Map PedalId PedalState

data View = GridView | DetailView PedalId | BoardsView | FilesView

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
  , availableOutputs :: Array MidiPort
  , availableInputs :: Array MidiPort
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
  , loopySelectedLoop: 0
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
