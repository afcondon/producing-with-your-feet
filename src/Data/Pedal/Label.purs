module Data.Pedal.Label
  ( LabelSource(..)
  ) where

import Data.Map (Map)
import Data.Midi (CC, MidiValue)
import Data.Pedal.Modes (ModeChannel, ModeRole)

-- | How a control's label is determined
data LabelSource
  = Static String
  | ModeMap { cc :: CC, labels :: Map MidiValue String }
  | ChannelMode { channel :: ModeChannel, role :: ModeRole }
