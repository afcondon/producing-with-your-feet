module Data.Loopy
  ( LoopyAction(..)
  , LoopyActionDef
  , LoopIndex(..)
  , LoopColor
  , LoopGroup
  , LoopyParam(..)
  , LoopyTwisterConfig
  , actions
  , groups
  , selectCC
  , volumeCC
  , loopCount
  , paramCC
  , paramLabel
  , loopToEncoder
  , encoderToLoop
  , recordAndMixConfig
  ) where

import Prelude

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

-- | Parameter type for Twister bottom row (encoders 8-15)
data LoopyParam
  = LoopyParamContinuous { label :: String, cc :: CC }
  | LoopyParamToggle { label :: String, cc :: CC }
  | LoopyParamMomentary { label :: String, cc :: CC }

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
      [ LoopyParamMomentary  { label: "Rec",   cc: unsafeCC 20 }
      , LoopyParamMomentary  { label: "Play",  cc: unsafeCC 21 }
      , LoopyParamMomentary  { label: "Mute",  cc: unsafeCC 23 }
      , LoopyParamMomentary  { label: "Solo",  cc: unsafeCC 24 }
      -- Row 4: editing (continuous rotation + toggle press)
      , LoopyParamContinuous { label: "Speed", cc: unsafeCC 53 }
      , LoopyParamToggle     { label: "Rvrs",  cc: unsafeCC 56 }
      , LoopyParamContinuous { label: "FadeI", cc: unsafeCC 54 }
      , LoopyParamContinuous { label: "FadeO", cc: unsafeCC 55 }
      ]
  }
