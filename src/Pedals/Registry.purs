module Pedals.Registry (pedals, findPedal) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Pedal (PedalDef, PedalId)
import Pedals.Onward as Onward
import Pedals.Mood as Mood
import Pedals.Clean as Clean
import Pedals.Habit as Habit
import Pedals.Hedra as Hedra
import Pedals.Flint as Flint
import Pedals.Lex as Lex
import Pedals.Iridium as Iridium
import Pedals.Riverside as Riverside
import Pedals.Mercury7 as Mercury7
import Pedals.Brig as Brig
import Pedals.LostAndFound as LostAndFound

-- | All pedals in display order (matches JS tab bar order)
pedals :: Array PedalDef
pedals =
  [ Onward.pedal
  , Mood.pedal
  , Clean.pedal
  , Habit.pedal
  , Hedra.pedal
  , Flint.pedal
  , Lex.pedal
  , Iridium.pedal
  , Riverside.pedal
  , Mercury7.pedal
  , Brig.pedal
  , LostAndFound.pedal
  ]

findPedal :: PedalId -> Maybe PedalDef
findPedal pid = Array.find (\p -> p.meta.id == pid) pedals
