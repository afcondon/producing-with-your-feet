module Data.Pedal
  ( PedalId(..)
  , PedalMeta
  , PedalDef
  , Annotation
  , SelectOption
  , RangeOption
  , ModeRangesMode
  , LabelSource(..)
  , Control(..)
  , Section
  , SectionLayout(..)
  ) where

import Prelude

import Color (Color)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Midi (CC, MidiValue)
import Data.Pedal.Engage (EngageConfig)
import Data.Pedal.Modes (DualChannelModes, ModeChannel, ModeRole)
import Data.Twister (TwisterMapping)

newtype PedalId = PedalId String

derive newtype instance Eq PedalId
derive newtype instance Ord PedalId
derive newtype instance Show PedalId

type PedalMeta =
  { id :: PedalId
  , name :: String
  , brand :: String
  , color :: Maybe Color
  , defaultChannel :: Int
  , saveInstructions :: Maybe String
  }

type Annotation = { position :: MidiValue, label :: String }

type SelectOption = { label :: String, value :: MidiValue, description :: Maybe String }

type RangeOption = { lo :: MidiValue, hi :: MidiValue, label :: String, description :: Maybe String }

type ModeRangesMode =
  { lo :: MidiValue
  , hi :: MidiValue
  , ranges :: Array (Array RangeOption)
  }

-- | How a control's label is determined
data LabelSource
  = Static String
  | ModeMap { cc :: CC, labels :: Map MidiValue String }
  | ChannelMode { channel :: ModeChannel, role :: ModeRole }

-- | Control ADT — one constructor per widget type
data Control
  = Slider
      { cc :: CC
      , label :: LabelSource
      , description :: Maybe String
      , annotations :: Array Annotation
      }
  | Toggle
      { cc :: CC
      , label :: String
      , onLabel :: String
      , offLabel :: String
      , description :: Maybe String
      , labelSource :: Maybe LabelSource
      }
  | Segmented
      { cc :: CC
      , label :: String
      , options :: Array SelectOption
      }
  | Dropdown
      { cc :: CC
      , label :: String
      , options :: Array SelectOption
      , description :: Maybe String
      }
  | Momentary
      { cc :: CC
      , label :: String
      , value :: MidiValue
      , description :: Maybe String
      }
  | PianoKey
      { cc :: CC
      , label :: String
      , options :: Array SelectOption
      , chromaticValue :: MidiValue
      }
  | RangeSelect
      { cc :: CC
      , label :: String
      , ranges :: Array RangeOption
      }
  | ModeRanges
      { cc :: CC
      , label :: String
      , modeCC :: CC
      , modes :: Array ModeRangesMode
      }
  | RadioGroup
      { label :: String
      , mapping :: Array { label :: String, values :: Map CC MidiValue }
      }
  | ModeRadio
      { label :: String
      , modeChannel :: ModeChannel
      }
  | InfoToggle
      { key :: String
      , label :: String
      , description :: Maybe String
      }

type Section =
  { name :: String
  , compact :: Boolean
  , collapsed :: Boolean
  , layout :: SectionLayout
  , description :: Maybe String
  , controls :: Array Control
  }

data SectionLayout = DefaultLayout | DipGrid | DualColumn

derive instance Eq SectionLayout

-- | The complete static definition of a pedal
type PedalDef =
  { meta :: PedalMeta
  , engage :: EngageConfig
  , baseline :: Map CC MidiValue
  , resetOrder :: Array CC
  , sections :: Array Section
  , twister :: Maybe TwisterMapping
  , modes :: Maybe DualChannelModes
  }
