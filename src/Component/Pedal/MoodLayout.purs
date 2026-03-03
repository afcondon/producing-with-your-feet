module Component.Pedal.MoodLayout
  ( moodKnobs
  , moodFootswitches
  , KnobPair
  , Footswitch
  , Channel(..)
  , KnobType(..)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Midi (CC, unsafeCC)

data Channel = Wet | Shared | ML

derive instance Eq Channel

data KnobType = Continuous | Segmented

derive instance Eq KnobType

type KnobPair =
  { col :: Int
  , row :: Int
  , channel :: Channel
  , primaryCC :: CC
  , primaryLabel :: String
  , hiddenCC :: CC
  , hiddenLabel :: String
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

-- | 3×3 grid of knob pairs: columns (Wet, Shared, ML) × rows (A, B, C)
moodKnobs :: Array KnobPair
moodKnobs =
  -- Row A
  [ { col: 0, row: 0, channel: Wet
    , primaryCC: cc 14, primaryLabel: "Time"
    , hiddenCC: cc 24, hiddenLabel: "Stereo Width"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  , { col: 1, row: 0, channel: Shared
    , primaryCC: cc 15, primaryLabel: "Mix"
    , hiddenCC: cc 25, hiddenLabel: "Ramp Wave"
    , primaryType: Continuous, hiddenType: Segmented
    , primaryCenter: Just 64, hiddenCenter: Nothing }
  , { col: 2, row: 0, channel: ML
    , primaryCC: cc 16, primaryLabel: "Length"
    , hiddenCC: cc 26, hiddenLabel: "Fade"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  -- Row B
  , { col: 0, row: 1, channel: Wet
    , primaryCC: cc 17, primaryLabel: "Modify"
    , hiddenCC: cc 27, hiddenLabel: "Tone"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  , { col: 1, row: 1, channel: Shared
    , primaryCC: cc 18, primaryLabel: "Clock"
    , hiddenCC: cc 28, hiddenLabel: "Level Bal"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Just 64, hiddenCenter: Just 64 }
  , { col: 2, row: 1, channel: ML
    , primaryCC: cc 19, primaryLabel: "Modify"
    , hiddenCC: cc 29, hiddenLabel: "Direct ML"
    , primaryType: Continuous, hiddenType: Continuous
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  -- Row C (toggle switches)
  , { col: 0, row: 2, channel: Wet
    , primaryCC: cc 21, primaryLabel: "Wet Mode"
    , hiddenCC: cc 31, hiddenLabel: "Sync"
    , primaryType: Segmented, hiddenType: Segmented
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  , { col: 1, row: 2, channel: Shared
    , primaryCC: cc 22, primaryLabel: "Routing"
    , hiddenCC: cc 32, hiddenLabel: "Spread Solo"
    , primaryType: Segmented, hiddenType: Segmented
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  , { col: 2, row: 2, channel: ML
    , primaryCC: cc 23, primaryLabel: "ML Mode"
    , hiddenCC: cc 33, hiddenLabel: "Buffer Len"
    , primaryType: Segmented, hiddenType: Segmented
    , primaryCenter: Nothing, hiddenCenter: Nothing }
  ]

-- | Row D: footswitches
moodFootswitches :: Array Footswitch
moodFootswitches =
  [ { col: 0, cc: cc 103, label: "Wet", channel: Wet }
  , { col: 2, cc: cc 102, label: "ML", channel: ML }
  ]
