module Data.Loopy
  ( LoopyAction(..)
  , LoopyActionDef
  , LoopIndex(..)
  , LoopColor
  , LoopGroup
  , LoopPhase(..)
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
  , panCC
  , overdubCC
  , phaseLockCC
  , thresholdCC
  , loopCount
  , paramCC
  , paramLabel
  , loopToEncoder
  , encoderToLoop
  , loopConfigBank
  , defaultLoopState
  , defaultClipSettings
  , countInLabel
  , countOutLabel
  , recordEndLabel
  , beatQuantLabel
  , ShiftDef
  , paramShift
  , transition
  , expectedResolution
  , ccToAction
  , phaseLabel
  , hasContent
  , isRecording
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Midi (CC, unCC, unsafeCC)

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
  | LoopyPlay
  | LoopyStop
  | LoopyPlayImmediate
  | LoopyStopImmediate
  | LoopyUndo
  | LoopyRedo
  | LoopyTapTempo
  | LoopyRetroRecord

derive instance Eq LoopyAction

-- | Phase model for LoopyPro loop state
data LoopPhase
  = PhaseEmpty        -- No audio content
  | PhaseArmed        -- Waiting to record (count-in or threshold pending)
  | PhaseRecording    -- Actively recording initial content
  | PhasePlaying      -- Has content, producing audio
  | PhaseStopped      -- Has content, not playing
  | PhaseOverdubbing  -- Recording over existing content
  | PhaseMuted        -- Has content, output silenced (phase-locked: playhead advancing)

derive instance Eq LoopPhase

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
  { phase :: LoopPhase
  , volume :: Int
  , speed :: Int
  , soloed :: Boolean
  }

defaultLoopState :: LoopState
defaultLoopState = { phase: PhaseEmpty, volume: 100, speed: 64, soloed: false }

clearCC :: CC
clearCC = unsafeCC 22

speedCC :: CC
speedCC = unsafeCC 53

-- | Pan CC per loop: CC 70-77 on channel 16
panCC :: LoopIndex -> CC
panCC (LoopIndex i) = unsafeCC (70 + i)

-- | Overdub feedback (continuous, selected loop): CC 80
overdubCC :: CC
overdubCC = unsafeCC 80

-- | Phase lock toggle (selected loop): CC 81
phaseLockCC :: CC
phaseLockCC = unsafeCC 81

-- | Threshold toggle (selected loop): CC 82
thresholdCC :: CC
thresholdCC = unsafeCC 82

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
  [ { action: LoopyRecord,        cc: unsafeCC 20, label: "Record" }
  , { action: LoopyPlayStop,      cc: unsafeCC 21, label: "Play/Stop" }
  , { action: LoopyClear,         cc: unsafeCC 22, label: "Clear" }
  , { action: LoopyMute,          cc: unsafeCC 23, label: "Mute" }
  , { action: LoopySolo,          cc: unsafeCC 24, label: "Solo" }
  , { action: LoopyMultiply,      cc: unsafeCC 25, label: "\x00d72" }
  , { action: LoopyDivide,        cc: unsafeCC 26, label: "\x00f72" }
  , { action: LoopyPrev,          cc: unsafeCC 27, label: "\x25c4" }
  , { action: LoopyNext,          cc: unsafeCC 28, label: "\x25ba" }
  , { action: LoopyUndo,          cc: unsafeCC 57, label: "Undo" }
  , { action: LoopyRedo,          cc: unsafeCC 58, label: "Redo" }
  , { action: LoopyPlay,          cc: unsafeCC 59, label: "Play" }
  , { action: LoopyStop,          cc: unsafeCC 60, label: "Stop" }
  , { action: LoopyPlayImmediate, cc: unsafeCC 61, label: "Play!" }
  , { action: LoopyStopImmediate, cc: unsafeCC 62, label: "Stop!" }
  , { action: LoopyTapTempo,     cc: unsafeCC 83, label: "Tap" }
  , { action: LoopyRetroRecord,  cc: unsafeCC 84, label: "Retro" }
  ]

groups :: Array LoopGroup
groups =
  [ { color: { label: "Orange", color: "#e87830", twisterHue: 75 }, loopA: LoopIndex 0, loopB: LoopIndex 1 }
  , { color: { label: "Yellow", color: "#e8c830", twisterHue: 63 }, loopA: LoopIndex 2, loopB: LoopIndex 3 }
  , { color: { label: "Lime",   color: "#70c830", twisterHue: 45 }, loopA: LoopIndex 4, loopB: LoopIndex 5 }
  , { color: { label: "Blue",   color: "#30a0d8", twisterHue: 10 }, loopA: LoopIndex 6, loopB: LoopIndex 7 }
  ]

-- | "Loop Config" — per-loop parameters when a loop is selected
-- Enc 8: Volume (per-loop CC 40+loop), Enc 9: Speed (CC 53)
-- Enc 10: Overdub (CC 80), Enc 11: Reverse toggle (CC 56)
-- Enc 12: Fade In + Phase toggle, Enc 13: Fade Out + Threshold toggle
-- Enc 14: Multiply ×2, Enc 15: Divide ÷2
loopConfigBank :: LoopyTwisterConfig
loopConfigBank =
  { name: "Loop Config"
  , paramHue: 42
  , params:
      [ LoopyParamContinuous { label: "Vol",  cc: unsafeCC 40, shift: Nothing }
      , LoopyParamContinuous { label: "Spd",  cc: unsafeCC 53, shift: Nothing }
      , LoopyParamContinuous { label: "OD",   cc: unsafeCC 80, shift: Nothing }
      , LoopyParamToggle     { label: "Rvrs", cc: unsafeCC 56, shift: Nothing }
      , LoopyParamContinuous { label: "FdI",  cc: unsafeCC 54, shift: Just { label: "Phase", action: LoopyRecord, cc: unsafeCC 81 } }
      , LoopyParamContinuous { label: "FdO",  cc: unsafeCC 55, shift: Just { label: "Thresh", action: LoopyRecord, cc: unsafeCC 82 } }
      , LoopyParamMomentary  { label: "\x00d72", cc: unsafeCC 25, action: LoopyMultiply, shift: Nothing }
      , LoopyParamMomentary  { label: "\x00f72", cc: unsafeCC 26, action: LoopyDivide,   shift: Nothing }
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

-- | Determine the result phase when recording ends, based on clip settings
recordEndPhase :: ClipSettings -> LoopPhase
recordEndPhase cs = case cs.recordEndAction of
  EndPlay -> PhasePlaying
  EndStop -> PhaseStopped
  EndOverdub -> PhaseOverdubbing

-- | Whether a count-in is configured (armed before recording)
hasCountIn :: ClipSettings -> Boolean
hasCountIn cs = cs.countIn /= CountInNone || cs.threshold

-- | Pure phase transition function
transition :: ClipSettings -> LoopPhase -> LoopyAction -> LoopPhase
transition cs phase action = case phase, action of
  -- Record action
  PhaseEmpty,      LoopyRecord -> if hasCountIn cs then PhaseArmed else PhaseRecording
  PhaseArmed,      LoopyRecord -> PhaseEmpty  -- cancel
  PhaseRecording,  LoopyRecord -> if cs.autoCountOut then PhaseRecording else recordEndPhase cs
  PhasePlaying,    LoopyRecord -> PhaseOverdubbing
  PhaseStopped,    LoopyRecord -> PhaseOverdubbing
  PhaseOverdubbing,LoopyRecord -> PhasePlaying  -- end overdub
  PhaseMuted,      LoopyRecord -> PhaseOverdubbing  -- unmute + overdub

  -- PlayStop action
  PhaseEmpty,      LoopyPlayStop -> PhaseEmpty  -- no-op
  PhaseArmed,      LoopyPlayStop -> PhaseArmed  -- no-op
  PhaseRecording,  LoopyPlayStop -> recordEndPhase cs
  PhasePlaying,    LoopyPlayStop -> if cs.phaseLocked then PhaseMuted else PhaseStopped
  PhaseStopped,    LoopyPlayStop -> PhasePlaying
  PhaseOverdubbing,LoopyPlayStop -> PhasePlaying  -- end overdub + keep playing
  PhaseMuted,      LoopyPlayStop -> PhasePlaying  -- unmute

  -- Clear always -> Empty
  _,               LoopyClear -> PhaseEmpty

  -- Mute toggle
  PhasePlaying,    LoopyMute -> PhaseMuted
  PhaseMuted,      LoopyMute -> PhasePlaying
  _,               LoopyMute -> phase  -- no-op for other phases

  -- Stop
  PhasePlaying,    LoopyStop -> if cs.phaseLocked then PhaseMuted else PhaseStopped
  PhaseOverdubbing,LoopyStop -> if cs.phaseLocked then PhaseMuted else PhaseStopped
  _,               LoopyStop -> phase

  -- StopImmediate (same logic, no count-out)
  PhasePlaying,    LoopyStopImmediate -> if cs.phaseLocked then PhaseMuted else PhaseStopped
  PhaseOverdubbing,LoopyStopImmediate -> if cs.phaseLocked then PhaseMuted else PhaseStopped
  _,               LoopyStopImmediate -> phase

  -- Play
  PhaseStopped,    LoopyPlay -> PhasePlaying
  PhaseMuted,      LoopyPlay -> PhasePlaying
  _,               LoopyPlay -> phase

  -- PlayImmediate (same logic, no count-in)
  PhaseStopped,    LoopyPlayImmediate -> PhasePlaying
  PhaseMuted,      LoopyPlayImmediate -> PhasePlaying
  _,               LoopyPlayImmediate -> phase

  -- Phase-neutral actions
  _,               LoopySolo -> phase
  _,               LoopyMultiply -> phase
  _,               LoopyDivide -> phase
  _,               LoopyUndo -> phase
  _,               LoopyRedo -> phase
  _,               LoopyPrev -> phase
  _,               LoopyNext -> phase
  _,               LoopyTapTempo -> phase
  _,               LoopyRetroRecord -> phase

-- | For auto-transitioning phases, what phase comes next
expectedResolution :: ClipSettings -> LoopPhase -> Maybe LoopPhase
expectedResolution cs = case _ of
  PhaseArmed -> Just PhaseRecording
  PhaseRecording | cs.autoCountOut -> Just (recordEndPhase cs)
  _ -> Nothing

-- | Inverse lookup: CC number to LoopyAction
ccToAction :: CC -> Maybe LoopyAction
ccToAction cc = map _.action $ Array.find (\a -> unCC a.cc == unCC cc) actions

-- | Display label for a phase
phaseLabel :: LoopPhase -> String
phaseLabel = case _ of
  PhaseEmpty -> ""
  PhaseArmed -> "Arm"
  PhaseRecording -> "Rec"
  PhasePlaying -> ""
  PhaseStopped -> "Stop"
  PhaseOverdubbing -> "OD"
  PhaseMuted -> "M"

-- | Whether a phase has recorded content
hasContent :: LoopPhase -> Boolean
hasContent = case _ of
  PhaseEmpty -> false
  PhaseArmed -> false
  _ -> true

-- | Whether a phase involves recording
isRecording :: LoopPhase -> Boolean
isRecording = case _ of
  PhaseRecording -> true
  PhaseOverdubbing -> true
  _ -> false
