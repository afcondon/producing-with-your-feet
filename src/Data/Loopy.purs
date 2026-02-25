module Data.Loopy
  ( LoopyAction(..)
  , LoopyActionDef
  , LoopIndex(..)
  , LoopColor
  , LoopGroup
  , actions
  , groups
  , selectCC
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
  }

type LoopGroup =
  { color :: LoopColor
  , loopA :: LoopIndex
  , loopB :: LoopIndex
  }

selectCC :: LoopIndex -> CC
selectCC (LoopIndex i) = unsafeCC (30 + i)

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
  [ { color: { label: "Orange",  color: "#e87830" }, loopA: LoopIndex 0, loopB: LoopIndex 1 }
  , { color: { label: "Yellow",  color: "#e8c830" }, loopA: LoopIndex 2, loopB: LoopIndex 3 }
  , { color: { label: "Lime",    color: "#70c830" }, loopA: LoopIndex 4, loopB: LoopIndex 5 }
  , { color: { label: "Blue",    color: "#30a0d8" }, loopA: LoopIndex 6, loopB: LoopIndex 7 }
  , { color: { label: "Magenta", color: "#d040a0" }, loopA: LoopIndex 8, loopB: LoopIndex 9 }
  ]
