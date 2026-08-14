module Config.Types
  ( RigConfig
  , PedalEntry
  , MidiRouting
  , MidiMatch
  , SlotRange
  ) where

-- | Rig manifest types — decoded from config/rig.json

type MidiMatch = { match :: String }

type MidiRouting =
  { pedalOutput :: MidiMatch
  , twisterInput :: MidiMatch
  , twisterOutput :: MidiMatch
  , mc6Input :: MidiMatch
  }

type SlotRange = { start :: Int, count :: Int }

type PedalEntry =
  { file :: String
  , channel :: Int
  }

type RigConfig =
  { name :: String
  , storagePrefix :: String
  , pedals :: Array PedalEntry
  , midiRouting :: MidiRouting
  , slotRanges :: Array { brand :: String, range :: SlotRange }
  , controller :: String
  }
