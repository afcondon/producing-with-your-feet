module Data.Twister
  ( TwisterEncoder(..)
  , TwisterButton(..)
  , TwisterMapping
  ) where

import Data.Maybe (Maybe)
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
