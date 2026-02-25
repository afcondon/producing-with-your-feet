module Data.Preset
  ( PresetId
  , PedalPreset
  , BoardPresetEntry
  , BoardPreset
  ) where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Midi (CC, MidiValue, ProgramNumber)
import Data.Pedal (PedalId)
import Data.Pedal.Engage (EngageState)

type PresetId = String

type PedalPreset =
  { id :: PresetId
  , pedalId :: PedalId
  , name :: String
  , description :: String
  , notes :: String
  , values :: Map CC MidiValue
  , savedSlot :: Maybe ProgramNumber
  , created :: String
  , modified :: String
  }

type BoardPresetEntry =
  { presetId :: Maybe PresetId
  , engage :: EngageState
  }

type BoardPreset =
  { id :: PresetId
  , name :: String
  , description :: String
  , notes :: String
  , pedals :: Map PedalId BoardPresetEntry
  , created :: String
  , modified :: String
  }
