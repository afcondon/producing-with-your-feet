module Component.Pedal.MoodLayout
  ( moodKnobs
  , moodFootswitches
  , moodDips
  , moodConfig
  , KnobPair
  , Footswitch
  , DIPBank(..)
  , DIPSwitch
  , ConfigType(..)
  , ConfigControl
  , Channel(..)
  , KnobType(..)
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeCC, unsafeMidiValue)
import Data.Pedal (LabelSource(..))
import Data.Tuple (Tuple(..))

data Channel = Wet | Shared | ML

derive instance Eq Channel

data KnobType = Continuous | Segmented

derive instance Eq KnobType

type KnobPair =
  { col :: Int
  , row :: Int
  , channel :: Channel
  , primaryCC :: CC
  , primaryLabel :: LabelSource
  , hiddenCC :: CC
  , hiddenLabel :: LabelSource
  , primaryType :: KnobType
  , hiddenType :: KnobType
  , primaryCenter :: Maybe Int
  , hiddenCenter :: Maybe Int
  }

type Footswitch =
  { col :: Int
  , cc :: CC
  , label :: String
  , channel :: Channel
  , ledCC :: Maybe CC
  , engagedColor :: String
  , ledColor :: String
  }

cc :: Int -> CC
cc = unsafeCC

mv :: Int -> MidiValue
mv = unsafeMidiValue

infixr 6 Tuple as /\

-- | 3x3 grid of knob pairs: columns (Wet, Shared, ML) x rows (A, B, C)
moodKnobs :: Array KnobPair
moodKnobs =
  -- Row A
  [ { col: 0, row: 0, channel: Wet
    , primaryCC: cc 14
    , primaryLabel: ModeMap { cc: cc 21, labels: Map.fromFoldable [ mv 0 /\ "Decay", mv 2 /\ "Delay Time", mv 3 /\ "Refresh Rate" ] }
    , hiddenCC: cc 24, hiddenLabel: Static "Stereo Width"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  , { col: 1, row: 0, channel: Shared
    , primaryCC: cc 15, primaryLabel: Static "Mix"
    , hiddenCC: cc 25, hiddenLabel: Static "Ramp Wave"
    , primaryType: Continuous, hiddenType: Segmented
    , primaryCenter: Just 64, hiddenCenter: Nothing }
  , { col: 2, row: 0, channel: ML
    , primaryCC: cc 16, primaryLabel: Static "Length"
    , hiddenCC: cc 26, hiddenLabel: Static "Fade"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  -- Row B
  , { col: 0, row: 1, channel: Wet
    , primaryCC: cc 17
    , primaryLabel: ModeMap { cc: cc 21, labels: Map.fromFoldable [ mv 0 /\ "Smear", mv 2 /\ "Feedback", mv 3 /\ "Speed" ] }
    , hiddenCC: cc 27, hiddenLabel: Static "Tone"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  , { col: 1, row: 1, channel: Shared
    , primaryCC: cc 18, primaryLabel: Static "Clock"
    , hiddenCC: cc 28, hiddenLabel: Static "Level Bal"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Just 64, hiddenCenter: Just 64 }
  , { col: 2, row: 1, channel: ML
    , primaryCC: cc 19
    , primaryLabel: ModeMap { cc: cc 23, labels: Map.fromFoldable [ mv 0 /\ "Sensitivity", mv 2 /\ "Speed/Dir", mv 3 /\ "Amount" ] }
    , hiddenCC: cc 29, hiddenLabel: Static "Direct ML"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  -- Row C (toggle switches)
  , { col: 0, row: 2, channel: Wet
    , primaryCC: cc 21, primaryLabel: Static "Rev / Dly / Slip"
    , hiddenCC: cc 31, hiddenLabel: Static "ML>W / \x2014 / W>ML"
    , primaryType: Segmented, hiddenType: Segmented
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  , { col: 1, row: 2, channel: Shared
    , primaryCC: cc 22, primaryLabel: Static "IN / ML+IN / ML"
    , hiddenCC: cc 32, hiddenLabel: Static "Wet / Both / ML"
    , primaryType: Segmented, hiddenType: Segmented
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  , { col: 2, row: 2, channel: ML
    , primaryCC: cc 23, primaryLabel: Static "Env / Tape / Str"
    , hiddenCC: cc 33, hiddenLabel: Static "Half / Full"
    , primaryType: Segmented, hiddenType: Segmented
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  ]

-- | Row D: footswitches
-- Wet: off=gray, engaged=red, freeze(cc105)=green
-- ML:  off=gray, playback=green, overdub(cc106)=red
moodFootswitches :: Array Footswitch
moodFootswitches =
  [ { col: 0, cc: cc 103, label: "Wet", channel: Wet
    , ledCC: Just (cc 105), engagedColor: "#c75050", ledColor: "#50a060" }
  , { col: 2, cc: cc 102, label: "ML", channel: ML
    , ledCC: Just (cc 106), engagedColor: "#50a060", ledColor: "#c75050" }
  ]

-- | DIP switches (two banks of 8)
data DIPBank = Ramping | Customize

derive instance Eq DIPBank

type DIPSwitch = { cc :: CC, label :: String, bank :: DIPBank, index :: Int }

moodDips :: Array DIPSwitch
moodDips =
  [ { cc: cc 61, label: "Time",     bank: Ramping,   index: 0 }
  , { cc: cc 62, label: "Modify L", bank: Ramping,   index: 1 }
  , { cc: cc 63, label: "Clock",    bank: Ramping,   index: 2 }
  , { cc: cc 64, label: "Modify R", bank: Ramping,   index: 3 }
  , { cc: cc 65, label: "Length",   bank: Ramping,   index: 4 }
  , { cc: cc 66, label: "Bounce",   bank: Ramping,   index: 5 }
  , { cc: cc 67, label: "Sweep",    bank: Ramping,   index: 6 }
  , { cc: cc 68, label: "Polarity", bank: Ramping,   index: 7 }
  , { cc: cc 71, label: "Classic",  bank: Customize, index: 0 }
  , { cc: cc 72, label: "Miso",     bank: Customize, index: 1 }
  , { cc: cc 73, label: "Spread",   bank: Customize, index: 2 }
  , { cc: cc 74, label: "Dry Kill", bank: Customize, index: 3 }
  , { cc: cc 75, label: "Trails",   bank: Customize, index: 4 }
  , { cc: cc 76, label: "Latch",    bank: Customize, index: 5 }
  , { cc: cc 77, label: "No Dub",   bank: Customize, index: 6 }
  , { cc: cc 78, label: "Smooth",   bank: Customize, index: 7 }
  ]

-- | Config controls (toggles + one slider)
data ConfigType = ConfigToggle | ConfigSlider

derive instance Eq ConfigType

type ConfigControl = { cc :: CC, label :: String, controlType :: ConfigType }

moodConfig :: Array ConfigControl
moodConfig =
  [ { cc: cc 107, label: "Tap",       controlType: ConfigToggle }
  , { cc: cc 52,  label: "Ramp",      controlType: ConfigToggle }
  , { cc: cc 20,  label: "Speed",     controlType: ConfigSlider }
  , { cc: cc 51,  label: "Clk Fol",   controlType: ConfigToggle }
  , { cc: cc 55,  label: "Bypass",    controlType: ConfigToggle }
  ]
