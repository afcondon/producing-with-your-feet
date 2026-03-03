module Component.Pedal.MoodLayout
  ( moodKnobs
  , moodFootswitches
  , KnobPair
  , Footswitch
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
moodFootswitches :: Array Footswitch
moodFootswitches =
  [ { col: 0, cc: cc 103, label: "Wet", channel: Wet }
  , { col: 2, cc: cc 102, label: "ML", channel: ML }
  ]
