module Data.Twister
  ( TwisterEncoder(..)
  , TwisterButton(..)
  , TwisterMapping
  , TwisterMsg(..)
  , SideBtn(..)
  , parseTwisterMsg
  ) where

import Prelude

import Data.Array as Array
import Data.Int.Bits (and)
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue)

data TwisterEncoder
  = TwisterCC { cc :: CC, center :: Maybe MidiValue, options :: Maybe (Array MidiValue) }

data TwisterButton
  = TwisterToggle { cc :: CC }
  | TwisterMomentary { cc :: CC }
  | TwisterSet { cc :: CC, value :: MidiValue }

type TwisterMapping =
  { encoders :: Array (Maybe TwisterEncoder)
  , buttons :: Array (Maybe TwisterButton)
  }

data TwisterMsg
  = EncoderTurn Int Int        -- encoderIndex (0-15), value (0-127)
  | EncoderPress Int           -- encoderIndex (0-15)
  | EncoderRelease Int         -- encoderIndex (0-15)
  | SideButton SideBtn

data SideBtn = PrevPedal | NextPedal | RefreshLEDs

parseTwisterMsg :: Array Int -> Maybe TwisterMsg
parseTwisterMsg bytes = do
  status <- Array.index bytes 0
  cc <- Array.index bytes 1
  val <- Array.index bytes 2
  let channel = (and status 0x0F) + 1 -- 1-indexed
  case channel of
    1 -> Just (EncoderTurn cc val)
    2
      | val == 127 -> Just (EncoderPress cc)
      | otherwise -> Just (EncoderRelease cc)
    5 -> case cc of
      8 -> Just (SideButton PrevPedal)
      9 -> Just (SideButton NextPedal)
      10 -> Just (SideButton RefreshLEDs)
      _ -> Nothing
    _ -> Nothing
