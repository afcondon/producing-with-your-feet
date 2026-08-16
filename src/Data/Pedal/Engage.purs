module Data.Pedal.Engage
  ( EngageConfig(..)
  , EngageState(..)
  , engageCCs
  , bypassCCs
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Midi (CC)

-- | Which CCs toggle a pedal on/off
-- |
-- | `both` on a dual pedal is a single message that takes the whole pedal out,
-- | where `a` and `b` would need one each. It matters because a board preset is
-- | compiled into an MC6 preset with a hard ceiling of sixteen messages, and
-- | four of the thirteen pedals are dual — costing two apiece for a bypass puts
-- | an all-twelve board over the limit (DESIGN-v2 §5).
-- |
-- | `Nothing` is the honest default rather than a gap to be filled in blindly.
-- | Our pedal JSON is a partial transcription of each pedal's MIDI
-- | implementation, and the two are not interchangeable sounds: MOOD's CC 55 is
-- | a *true* bypass that drops the relay and cuts trails, where turning both
-- | channels off leaves the pedal buffered and ringing. A pedal we have not
-- | checked stays correct-but-expensive.
data EngageConfig
  = SingleEngage CC
  | DualEngage
      { a :: { cc :: CC, label :: String }
      , b :: { cc :: CC, label :: String }
      , both :: Maybe CC
      }

-- | Engage state for board presets
data EngageState
  = EngageOn
  | EngageOff
  | EngageA
  | EngageB
  | EngageNoChange

derive instance Eq EngageState

-- | Every CC that engaging or bypassing this pedal can touch.
-- |
-- | Used to decide whether a saved preset already carries its own engage state,
-- | so `both` belongs here too — a preset that sets it is no less explicit
-- | about the pedal being on or off than one that sets `a` and `b`.
engageCCs :: EngageConfig -> Array CC
engageCCs (SingleEngage cc) = [cc]
engageCCs (DualEngage { a, b, both }) =
  [a.cc, b.cc] <> maybe [] Array.singleton both
  where
  maybe d f = case _ of
    Nothing -> d
    Just x -> f x

-- | The cheapest way to bypass this pedal, and what it costs in MC6 messages.
-- |
-- | One place so the compiler and the budget counter cannot disagree about the
-- | number — a board that reports 14/16 and then sends 16 would be worse than
-- | no counter at all.
bypassCCs :: EngageConfig -> Array CC
bypassCCs (SingleEngage cc) = [cc]
bypassCCs (DualEngage { a, b, both }) = case both of
  Just cc -> [cc]
  Nothing -> [a.cc, b.cc]
