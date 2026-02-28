module Data.Loopy
  ( LoopyAction(..)
  , LoopyActionDef
  , LoopIndex(..)
  , LoopColor
  , LoopGroup
  , LoopState
  , LoopyParam(..)
  , LoopyTwisterConfig
  , CountInMode(..)
  , CountOutMode(..)
  , RecordEndAction(..)
  , BeatQuantPreset(..)
  , ClipSettings
  , actions
  , groups
  , selectCC
  , volumeCC
  , clearCC
  , speedCC
  , loopCount
  , paramCC
  , paramLabel
  , loopToEncoder
  , encoderToLoop
  , recordAndMixConfig
  , defaultLoopState
  , defaultClipSettings
  , countInLabel
  , countOutLabel
  , recordEndLabel
  , beatQuantLabel
  , ShiftDef
  , paramShift
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Midi (CC, unsafeCC)

data LoopyAction
  = LoopyRecord
  | LoopyPlayStop
  | LoopyClear
  | LoopyMute
  | LoopySolo
  | LoopyMultiply
  | LoopyDivide
  | LoopyPrev
  | LoopyNext

derive instance Eq LoopyAction

type LoopyActionDef = { action :: LoopyAction, cc :: CC, label :: String }

newtype LoopIndex = LoopIndex Int

derive newtype instance Eq LoopIndex
derive newtype instance Ord LoopIndex

type LoopColor =
  { label :: String
  , color :: String
  , twisterHue :: Int
  }

type LoopGroup =
  { color :: LoopColor
  , loopA :: LoopIndex
  , loopB :: LoopIndex
  }

-- | Shift function available when holding a loop encoder
type ShiftDef = { label :: String, action :: LoopyAction, cc :: CC }

-- | Parameter type for Twister bottom row (encoders 8-15)
data LoopyParam
  = LoopyParamContinuous { label :: String, cc :: CC, shift :: Maybe ShiftDef }
  | LoopyParamToggle { label :: String, cc :: CC, shift :: Maybe ShiftDef }
  | LoopyParamMomentary { label :: String, cc :: CC, action :: LoopyAction, shift :: Maybe ShiftDef }

loopCount :: Int
loopCount = 8

-- | Map loop index (0-7) to Twister encoder index (0-7)
-- Twister row 1 (enc 0-3): A loops (0,2,4,6)
-- Twister row 2 (enc 4-7): B loops (1,3,5,7)
-- loop 0→enc 0, loop 1→enc 4, loop 2→enc 1, loop 3→enc 5, etc.
loopToEncoder :: Int -> Int
loopToEncoder loopIdx =
  let group = loopIdx / 2    -- which color group (0-3)
      row   = loopIdx `mod` 2 -- 0=A (top), 1=B (bottom)
  in row * 4 + group

-- | Map Twister encoder index (0-7) back to loop index (0-7)
-- enc 0→loop 0, enc 1→loop 2, enc 4→loop 1, enc 5→loop 3, etc.
encoderToLoop :: Int -> Int
encoderToLoop encIdx =
  let group = encIdx `mod` 4  -- which column (0-3)
      row   = encIdx / 4      -- 0=top, 1=bottom
  in group * 2 + row

type LoopState =
  { volume :: Int, speed :: Int, muted :: Boolean, soloed :: Boolean, cleared :: Boolean }

defaultLoopState :: LoopState
defaultLoopState = { volume: 100, speed: 64, muted: false, soloed: false, cleared: false }

clearCC :: CC
clearCC = unsafeCC 22

speedCC :: CC
speedCC = unsafeCC 53

-- | Extract shift definition from a LoopyParam
paramShift :: LoopyParam -> Maybe ShiftDef
paramShift = case _ of
  LoopyParamContinuous r -> r.shift
  LoopyParamToggle r -> r.shift
  LoopyParamMomentary r -> r.shift

selectCC :: LoopIndex -> CC
selectCC (LoopIndex i) = unsafeCC (30 + i)

-- | Volume CC per loop: CC 40-47 on channel 16
volumeCC :: LoopIndex -> CC
volumeCC (LoopIndex i) = unsafeCC (40 + i)

-- | Extract CC from a LoopyParam
paramCC :: LoopyParam -> CC
paramCC = case _ of
  LoopyParamContinuous r -> r.cc
  LoopyParamToggle r -> r.cc
  LoopyParamMomentary r -> r.cc

-- | Extract label from a LoopyParam
paramLabel :: LoopyParam -> String
paramLabel = case _ of
  LoopyParamContinuous r -> r.label
  LoopyParamToggle r -> r.label
  LoopyParamMomentary r -> r.label

-- | A named Twister configuration for the bottom 8 encoders
type LoopyTwisterConfig =
  { name :: String
  , params :: Array LoopyParam       -- 8 entries for encoders 8-15
  , paramHue :: Int                  -- MFT hue for the parameter encoders
  }

actions :: Array LoopyActionDef
actions =
  [ { action: LoopyRecord,   cc: unsafeCC 20, label: "Record" }
  , { action: LoopyPlayStop, cc: unsafeCC 21, label: "Play" }
  , { action: LoopyClear,    cc: unsafeCC 22, label: "Clear" }
  , { action: LoopyMute,     cc: unsafeCC 23, label: "Mute" }
  , { action: LoopySolo,     cc: unsafeCC 24, label: "Solo" }
  , { action: LoopyMultiply, cc: unsafeCC 25, label: "\x00d72" }
  , { action: LoopyDivide,   cc: unsafeCC 26, label: "\x00f72" }
  , { action: LoopyPrev,     cc: unsafeCC 27, label: "\x25c4" }
  , { action: LoopyNext,     cc: unsafeCC 28, label: "\x25ba" }
  ]

groups :: Array LoopGroup
groups =
  [ { color: { label: "Orange", color: "#e87830", twisterHue: 75 }, loopA: LoopIndex 0, loopB: LoopIndex 1 }
  , { color: { label: "Yellow", color: "#e8c830", twisterHue: 63 }, loopA: LoopIndex 2, loopB: LoopIndex 3 }
  , { color: { label: "Lime",   color: "#70c830", twisterHue: 45 }, loopA: LoopIndex 4, loopB: LoopIndex 5 }
  , { color: { label: "Blue",   color: "#30a0d8", twisterHue: 10 }, loopA: LoopIndex 6, loopB: LoopIndex 7 }
  ]

-- | "Record & Mix" — transport actions on row 3, editing tools on row 4
-- Row 3: Record, Play, Mute, Solo (momentary actions on selected loop)
-- Row 4: Speed, Reverse, Fade In, Fade Out (continuous/toggle)
recordAndMixConfig :: LoopyTwisterConfig
recordAndMixConfig =
  { name: "Record & Mix"
  , paramHue: 42
  , params:
      -- Row 3: transport (all momentary — use existing action CCs)
      [ LoopyParamMomentary  { label: "Rec",   cc: unsafeCC 20, action: LoopyRecord,   shift: Nothing }
      , LoopyParamMomentary  { label: "Play",  cc: unsafeCC 21, action: LoopyPlayStop, shift: Nothing }
      , LoopyParamMomentary  { label: "Mute",  cc: unsafeCC 23, action: LoopyMute,     shift: Just { label: "Clear", action: LoopyClear, cc: unsafeCC 22 } }
      , LoopyParamMomentary  { label: "Solo",  cc: unsafeCC 24, action: LoopySolo,     shift: Nothing }
      -- Row 4: editing (continuous rotation + toggle press)
      , LoopyParamContinuous { label: "Speed", cc: unsafeCC 53, shift: Just { label: "\x00d72", action: LoopyMultiply, cc: unsafeCC 25 } }
      , LoopyParamToggle     { label: "Rvrs",  cc: unsafeCC 56, shift: Just { label: "\x00f72", action: LoopyDivide,   cc: unsafeCC 26 } }
      , LoopyParamContinuous { label: "FadeI", cc: unsafeCC 54, shift: Just { label: "\x25c4",  action: LoopyPrev,     cc: unsafeCC 27 } }
      , LoopyParamContinuous { label: "FadeO", cc: unsafeCC 55, shift: Just { label: "\x25ba",  action: LoopyNext,     cc: unsafeCC 28 } }
      ]
  }

-- | Clip Settings — recording mode configuration per loop

data CountInMode = CountInNone | CountInMaster | CountInLoop

derive instance Eq CountInMode

data CountOutMode = CountOutNone | CountOutMaster | CountOutLoop

derive instance Eq CountOutMode

data RecordEndAction = EndPlay | EndStop | EndOverdub

derive instance Eq RecordEndAction

data BeatQuantPreset
  = BeatQuantOff
  | BeatQuant16Tight | BeatQuant16Med | BeatQuant16Loose
  | BeatQuant32Tight | BeatQuant32Med | BeatQuant32Loose

derive instance Eq BeatQuantPreset

type ClipSettings =
  { countIn         :: CountInMode
  , countOut        :: CountOutMode
  , autoCountOut    :: Boolean
  , recordEndAction :: RecordEndAction
  , overdubFeedback :: Number
  , beatQuant       :: BeatQuantPreset
  , phaseLocked     :: Boolean
  , loop            :: Boolean
  , threshold       :: Boolean
  , intro           :: Boolean
  , tail            :: Boolean
  , retrospective   :: Boolean
  }

defaultClipSettings :: ClipSettings
defaultClipSettings =
  { countIn: CountInMaster
  , countOut: CountOutMaster
  , autoCountOut: true
  , recordEndAction: EndPlay
  , overdubFeedback: 1.0
  , beatQuant: BeatQuantOff
  , phaseLocked: true
  , loop: true
  , threshold: false
  , intro: false
  , tail: false
  , retrospective: false
  }

countInLabel :: CountInMode -> String
countInLabel = case _ of
  CountInNone -> "None"
  CountInMaster -> "Master"
  CountInLoop -> "Loop"

countOutLabel :: CountOutMode -> String
countOutLabel = case _ of
  CountOutNone -> "None"
  CountOutMaster -> "Master"
  CountOutLoop -> "Loop"

recordEndLabel :: RecordEndAction -> String
recordEndLabel = case _ of
  EndPlay -> "Play"
  EndStop -> "Stop"
  EndOverdub -> "Overdub"

beatQuantLabel :: BeatQuantPreset -> String
beatQuantLabel = case _ of
  BeatQuantOff -> "Off"
  BeatQuant16Tight -> "16th Tight"
  BeatQuant16Med -> "16th Med"
  BeatQuant16Loose -> "16th Loose"
  BeatQuant32Tight -> "32nd Tight"
  BeatQuant32Med -> "32nd Med"
  BeatQuant32Loose -> "32nd Loose"
