module Config.Registry
  ( PedalRegistry
  , mkRegistry
  , findPedal
  , registryPedals
  , slotRange
  ) where

import Prelude

import Config.Types (MidiRouting, SlotRange)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Pedal (PedalDef, PedalId)
import Data.Tuple (Tuple(..))

type PedalRegistry =
  { pedals :: Array PedalDef
  , pedalMap :: Map PedalId PedalDef
  , slotRanges :: Map String SlotRange
  , midiRouting :: MidiRouting
  }

mkRegistry
  :: Array PedalDef
  -> Array { brand :: String, range :: SlotRange }
  -> MidiRouting
  -> PedalRegistry
mkRegistry pedals slots routing =
  { pedals
  , pedalMap: Map.fromFoldable $ map (\d -> Tuple d.meta.id d) pedals
  , slotRanges: Map.fromFoldable $ map (\s -> Tuple s.brand s.range) slots
  , midiRouting: routing
  }

findPedal :: PedalRegistry -> PedalId -> Maybe PedalDef
findPedal reg pid = Map.lookup pid reg.pedalMap

registryPedals :: PedalRegistry -> Array PedalDef
registryPedals reg = reg.pedals

slotRange :: PedalRegistry -> String -> Maybe SlotRange
slotRange reg brand = Map.lookup brand reg.slotRanges
