module Data.Pedal.Layout
  ( PedalLayout
  , GroupStyle
  , KnobDef
  , KnobLayer(..)
  , SegmentDef
  , FootswitchDef
  , DipBankDef
  , ConfigDef
  , ConfigControlType(..)
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Midi (CC)
import Data.Pedal.Label (LabelSource)

-- | A segment in a segmented knob: lo..hi defines the MIDI value range
-- | that activates this segment, send is the value emitted on click.
type SegmentDef = { lo :: Int, hi :: Int, send :: Int }

data KnobLayer
  = ContinuousKnob { center :: Maybe Int }
  | SegmentedKnob (Array SegmentDef)

derive instance Eq KnobLayer

data ConfigControlType = CfgToggle | CfgSlider

derive instance Eq ConfigControlType

type GroupStyle =
  { id :: String
  , label :: String
  , color :: String
  , mutedColor :: String
  }

type KnobDef =
  { col :: Int, row :: Int
  , group :: String
  , primaryCC :: CC
  , primaryLabel :: LabelSource
  , primaryLayer :: KnobLayer
  , hiddenCC :: Maybe CC
  , hiddenLabel :: Maybe LabelSource
  , hiddenLayer :: Maybe KnobLayer
  }

type FootswitchDef =
  { col :: Int, cc :: CC, label :: String
  , group :: String
  , ledCC :: Maybe CC
  , engagedColor :: String, ledColor :: String
  }

type DipBankDef =
  { label :: String
  , switches :: Array { cc :: CC, label :: String, index :: Int }
  }

type ConfigDef =
  { cc :: CC, label :: String
  , controlType :: ConfigControlType
  }

type PedalLayout =
  { groups :: Array GroupStyle
  , knobs :: Array KnobDef
  , footswitches :: Array FootswitchDef
  , dipBanks :: Array DipBankDef
  , config :: Array ConfigDef
  , columns :: Int
  , knobRows :: Int
  , viewBox :: { width :: Number, height :: Number }
  }
