module Data.Pedal.Engage
  ( EngageConfig(..)
  , EngageState(..)
  , engageCCs
  ) where

import Prelude

import Data.Midi (CC)

-- | Which CCs toggle a pedal on/off
data EngageConfig
  = SingleEngage CC
  | DualEngage
      { a :: { cc :: CC, label :: String }
      , b :: { cc :: CC, label :: String }
      }

-- | Engage state for board presets
data EngageState
  = EngageOn
  | EngageOff
  | EngageA
  | EngageB
  | EngageNoChange

derive instance Eq EngageState

-- | Extract the CC number(s) from an EngageConfig
engageCCs :: EngageConfig -> Array CC
engageCCs (SingleEngage cc) = [cc]
engageCCs (DualEngage { a, b }) = [a.cc, b.cc]
