module Engine
  ( PedalState
  , EngineState
  , MidiConnections
  , View(..)
  , AppState
  , initEngine
  , initAppState
  , getValue
  , getInfo
  , pedalState
  , defaultPedalState
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue)
import Data.Pedal (PedalDef, PedalId)
import Data.Tuple (Tuple(..))
import Foreign.WebMIDI (MIDIAccess, MIDIInput, MIDIOutput, MidiPort)
import Pedals.Registry as Registry

type PedalState =
  { channel :: Int
  , values :: Map CC MidiValue
  , info :: Map String Int
  }

type EngineState = Map PedalId PedalState

data View = GridView | DetailView PedalId | BoardsView

derive instance Eq View

type MidiConnections =
  { access :: Maybe MIDIAccess
  , pedalOutput :: Maybe MIDIOutput
  , pedalOutputId :: Maybe String
  , twisterInput :: Maybe MIDIInput
  , twisterInputId :: Maybe String
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
  , focusPedalId :: Maybe PedalId
  }

defaultPedalState :: PedalDef -> PedalState
defaultPedalState def =
  { channel: def.meta.defaultChannel
  , values: def.baseline
  , info: Map.empty
  }

initEngine :: EngineState
initEngine = Map.fromFoldable $
  map (\def -> Tuple def.meta.id (defaultPedalState def)) Registry.pedals

initAppState :: AppState
initAppState =
  { view: GridView
  , engine: initEngine
  , connections:
      { access: Nothing
      , pedalOutput: Nothing
      , pedalOutputId: Nothing
      , twisterInput: Nothing
      , twisterInputId: Nothing
      , loopyOutput: Nothing
      , loopyOutputId: Nothing
      , availableOutputs: []
      , availableInputs: []
      }
  , cardOrder: map _.meta.id Registry.pedals
  , focusPedalId: Nothing
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
