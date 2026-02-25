module Data.Pedal.Modes
  ( ModeChannel(..)
  , ModeRole(..)
  , EffectVariant
  , EffectDef
  , ChannelDef
  , DualChannelModes
  ) where

import Prelude

import Data.Midi (CC)

data ModeChannel = LeftChannel | RightChannel

derive instance Eq ModeChannel
derive instance Ord ModeChannel

data ModeRole = TimeRole | ModifyRole | AltRole | HoldRole

derive instance Eq ModeRole

type EffectVariant =
  { name :: String
  , time :: String
  , modify :: String
  , alt :: String
  }

type EffectDef =
  { id :: String
  , label :: String
  , a :: EffectVariant
  , b :: EffectVariant
  , hold :: String
  }

type ChannelDef =
  { modeCC :: CC
  , swapCC :: CC
  , native :: Array String
  }

type DualChannelModes =
  { effects :: Array EffectDef
  , left :: ChannelDef
  , right :: ChannelDef
  }
