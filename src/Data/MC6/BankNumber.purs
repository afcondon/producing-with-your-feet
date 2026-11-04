-- | **Bank numbers as the MC6 shows them.**
-- |
-- | The device counts banks from 0 on the wire and from 1 on its display, and
-- | this app showed the wire's: "bank 16" here was "17" under your foot. Found
-- | 2026-11-04, when the Ambient page written to wire bank 16 could not be
-- | found by walking to 16. One number must not name two banks, so a person
-- | sees and types the device's number, and the code keeps the wire's.
-- |
-- | Everything that becomes text a person reads, or is read from what a person
-- | typed, goes through here. Nothing else adds or subtracts one.
module Data.MC6.BankNumber
  ( shown
  , fromShown
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe)
import Data.String (trim)

-- | A wire bank number, as the MC6's display shows it.
shown :: Int -> String
shown wire = show (wire + 1)

-- | What a person typed, as a wire bank number.
fromShown :: String -> Maybe Int
fromShown text = map (_ - 1) (Int.fromString (trim text))
