module Config.Types
  ( RigConfig
  , PedalEntry
  , MidiRouting
  , MidiMatch
  , SlotRange
  , BrandSlots
  ) where

import Data.Maybe (Maybe)

-- | Rig manifest types — decoded from config/rig.json

type MidiMatch = { match :: String }

type MidiRouting =
  { pedalOutput :: MidiMatch
  , twisterInput :: MidiMatch
  , twisterOutput :: MidiMatch
  , mc6Input :: MidiMatch
  }

type SlotRange = { start :: Int, count :: Int }

-- | Where a brand's presets live, and which part of that this app has claimed.
-- |
-- | These are two different questions and conflating them cost us the factory
-- | presets: `range` is everything a Program Change can reach on the pedal, and
-- | is what the slot grid browses. `managed` is the house convention for where
-- | *we* save — on the Strymons, above 50, so that saving from this app never
-- | lands on a preset that shipped with the box. Constraining the browser to
-- | the managed region made half the pedal invisible.
-- |
-- | Nothing means the brand has no such convention, because there is nowhere
-- | safe: the sixteen Meris slots are all factory-named, so anything we save
-- | there overwrites something.
type BrandSlots =
  { range :: SlotRange
  , managed :: Maybe SlotRange
  }

type PedalEntry =
  { file :: String
  , channel :: Int
  }

type RigConfig =
  { name :: String
  , storagePrefix :: String
  , pedals :: Array PedalEntry
  , midiRouting :: MidiRouting
  , slotRanges :: Array { brand :: String, slots :: BrandSlots }
  , controller :: String
  }
