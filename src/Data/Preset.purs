module Data.Preset
  ( PresetId
  , PedalPreset
  , BoardPresetEntry
  , BoardPreset
  , isSlotRef
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, isJust)
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
  , info :: Map String Int
  , savedSlot :: Maybe ProgramNumber
  , created :: String
  , modified :: String
  }

-- | A *slot reference*: a preset that exists only in the pedal.
-- |
-- | Some of the best sounds on the board were dialled in years ago and saved
-- | to a numbered slot, and some pedals will not give them back — the two
-- | Meris units name their sixteen presets, but only inside the Meris editor
-- | and its own MIDI box. Requiring a captured value map before a slot can be
-- | used is friction with nothing on the other end of it: what a board or an
-- | MC6 switch actually needs is the number.
-- |
-- | So this is modelled as a preset with an empty value map rather than as its
-- | own type. It lands in the same library, the same board entries and the
-- | same Program Change path as a captured preset, and adds no codec on either
-- | side of the store. What it costs is that the two operations needing values
-- | — overwrite, and recall-by-streaming — have to check first.
isSlotRef :: PedalPreset -> Boolean
isSlotRef p = Map.isEmpty p.values && isJust p.savedSlot

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
