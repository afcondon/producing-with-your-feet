module Config.Registry
  ( PedalRegistry
  , mkRegistry
  , findPedal
  , registryPedals
  , brandSlots
  ) where

import Prelude

import Config.Types (BrandSlots, MidiRouting)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Pedal (PedalDef, PedalId)
import Data.Tuple (Tuple(..))

type PedalRegistry =
  { pedals :: Array PedalDef
  , pedalMap :: Map PedalId PedalDef
  , slotRanges :: Map String BrandSlots
  , midiRouting :: MidiRouting
  }

mkRegistry
  :: Array PedalDef
  -> Array { brand :: String, slots :: BrandSlots }
  -> MidiRouting
  -> PedalRegistry
mkRegistry pedals slots routing =
  { pedals
  , pedalMap: Map.fromFoldable $ map (\d -> Tuple d.meta.id d) pedals
  , slotRanges: Map.fromFoldable $ map (\s -> Tuple s.brand s.slots) slots
  , midiRouting: routing
  }

findPedal :: PedalRegistry -> PedalId -> Maybe PedalDef
findPedal reg pid = Map.lookup pid reg.pedalMap

registryPedals :: PedalRegistry -> Array PedalDef
registryPedals reg = reg.pedals

-- | The browsable span for a brand, plus the sub-range this app saves into.
brandSlots :: PedalRegistry -> String -> Maybe BrandSlots
brandSlots reg brand = Map.lookup brand reg.slotRanges
