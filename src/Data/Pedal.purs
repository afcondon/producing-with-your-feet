module Data.Pedal
  ( PedalId(..)
  , PedalMeta
  , PedalDef
  , Annotation
  , SelectOption
  , RangeOption
  , ModeRangesMode
  , RadioChoice
  , choiceIsSelected
  , choiceSends
  , module Data.Pedal.Label
  , Control(..)
  , Section
  , SectionLayout(..)
  ) where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue)
import Data.Pedal.Engage (EngageConfig)
import Data.Pedal.Label (LabelSource(..))
import Data.Pedal.Layout (PedalLayout)
import Data.Pedal.Modes (DualChannelModes, ModeChannel)
import Data.Tuple (Tuple(..))
import Data.Twister (TwisterMapping)

newtype PedalId = PedalId String

derive newtype instance Eq PedalId
derive newtype instance Ord PedalId
derive newtype instance Show PedalId

type PedalMeta =
  { id :: PedalId
  , name :: String
  , shortName :: String
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
      , mapping :: Array RadioChoice
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

-- | One choice in a RadioGroup. `values` are what the choice *is*: they are
-- | sent when it is chosen, and the choice shows as selected while the pedal
-- | holds all of them. `alsoSends` are sent when it is chosen and never checked
-- | afterwards: a companion control's starting point, which the player is
-- | expected to move. Habit's modifiers put Modify (CC 17) at the modifier's
-- | neutral point this way; were it in `values`, turning Modify would make the
-- | modifier stop showing as selected.
type RadioChoice =
  { label :: String
  , values :: Map CC MidiValue
  , alsoSends :: Map CC MidiValue
  }

-- | Selected while the pedal holds every one of the choice's `values`;
-- | `alsoSends` play no part.
choiceIsSelected :: RadioChoice -> Map CC MidiValue -> Boolean
choiceIsSelected choice held =
  Array.all (\(Tuple c v) -> Map.lookup c held == Just v) (Map.toUnfoldable choice.values)

-- | What choosing it sends, in order: what it is, then what it also sets.
-- | A CC in both is sent once, as `values` says.
choiceSends :: RadioChoice -> Array (Tuple CC MidiValue)
choiceSends choice =
  Map.toUnfoldable choice.values
    <> Map.toUnfoldable (Map.difference choice.alsoSends choice.values)

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
  , layout :: Maybe PedalLayout
  }
