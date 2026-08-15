-- | Itajara — the looper, as a pedal on the board.
-- |
-- | The daemon holds the audio; this is the face it wears among the other
-- | twelve. Deliberately not the whole control surface: a real pedal's front
-- | panel is not its MIDI implementation chart either. The deep work — every
-- | layer, every source, the send — lives on the Looper page, and everything
-- | is reachable from the MC6 regardless of what appears here.
-- |
-- | Five columns because the footswitch row gets `columns - 1` distinct
-- | positions, and four gestures is the transport worth having underfoot.
module Pedals.Itajara (pedal) where

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, unsafeCC)
import Data.Pedal (PedalDef, PedalId(..), LabelSource(..))
import Data.Pedal.Engage (EngageConfig(..))
import Data.Pedal.Layout (ConfigControlType(..), KnobLayer(..), PedalLayout)
import Color (fromHexString)

cc :: Int -> CC
cc = unsafeCC

pedal :: PedalDef
pedal =
  { meta:
      { id: PedalId "itajara"
      , name: "Itajara"
      , shortName: "Ij"
      , brand: "Hylograph"
      , color: fromHexString "#3a7d6c"
      , defaultChannel: 13
      , saveInstructions: Nothing
      }
  , engage: SingleEngage (cc 7)
  , baseline: Map.empty
  , resetOrder: []
  , twister: Nothing
  , modes: Nothing
  , sections: []
  , layout: Just layout
  }

-- | Only the layout is read from here — `mergeLayout` in `Component.App` takes
-- | this field and leaves everything else to the JSON, which is why the rest of
-- | the record above is empty rather than duplicated.
layout :: PedalLayout
layout =
  { groups:
      [ { id: "loop",   label: "Loop",  color: "#3a7d6c", mutedColor: "#8fb5ad" }
      , { id: "source", label: "In",    color: "#5a8fa8", mutedColor: "#a3c2d0" }
      , { id: "layer",  label: "Layer", color: "#a87a4a", mutedColor: "#cdb494" }
      ]
  , knobs:
      -- Top row: what you hear.
      [ { col: 0, row: 0
        , group: "loop"
        , primaryCC: cc 80, primaryLabel: Static "Loop"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing, hiddenLayer: Nothing
        }
      , { col: 2, row: 0
        , group: "source"
        , primaryCC: cc 20, primaryLabel: Static "Source"
        , primaryLayer: SegmentedKnob sourceSegments
        , hiddenCC: Nothing, hiddenLabel: Nothing, hiddenLayer: Nothing
        }
      , { col: 4, row: 0
        , group: "source"
        , primaryCC: cc 23, primaryLabel: Static "Monitor"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing, hiddenLayer: Nothing
        }
      -- Bottom row: pick a layer, then place it. The pointer is what makes
      -- eight layers reachable from three knobs.
      , { col: 0, row: 1
        , group: "layer"
        , primaryCC: cc 60, primaryLabel: Static "Layer"
        , primaryLayer: SegmentedKnob layerSegments
        , hiddenCC: Nothing, hiddenLabel: Nothing, hiddenLayer: Nothing
        }
      , { col: 2, row: 1
        , group: "layer"
        , primaryCC: cc 65, primaryLabel: Static "Pan"
        , primaryLayer: ContinuousKnob { center: Just 64 }
        , hiddenCC: Nothing, hiddenLabel: Nothing, hiddenLayer: Nothing
        }
      , { col: 4, row: 1
        , group: "layer"
        , primaryCC: cc 66, primaryLabel: Static "Width"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing, hiddenLayer: Nothing
        }
      ]
  , footswitches:
      [ { col: 0, cc: cc 1, label: "Rec", group: "loop"
        , ledCC: Nothing, engagedColor: "#c0392b", ledColor: "#c0392b" }
      , { col: 1, cc: cc 2, label: "Mult", group: "loop"
        , ledCC: Nothing, engagedColor: "#3a7d6c", ledColor: "#3a7d6c" }
      , { col: 2, cc: cc 5, label: "Take", group: "loop"
        , ledCC: Nothing, engagedColor: "#3a7d6c", ledColor: "#3a7d6c" }
      , { col: 3, cc: cc 3, label: "Undo", group: "layer"
        , ledCC: Nothing, engagedColor: "#a87a4a", ledColor: "#a87a4a" }
      ]
  , dipBanks: []
  , config:
      [ { cc: cc 81, label: "Click",   controlType: CfgToggle }
      , { cc: cc 83, label: "Monitor", controlType: CfgToggle }
      , { cc: cc 8,  label: "Reverse", controlType: CfgToggle }
      , { cc: cc 9,  label: "Half",    controlType: CfgToggle }
      ]
  , columns: 5
  , knobRows: 2
  , viewBox: { width: 480.0, height: 370.0 }
  }

-- | Eight equal segments over the CC range, one per layer. `send` is the value
-- | that lands the pointer squarely in the middle of its own band, so a click
-- | round-trips rather than drifting toward a boundary.
layerSegments :: Array { lo :: Int, hi :: Int, send :: Int }
layerSegments =
  [ { lo: 0,   hi: 15,  send: 8 }
  , { lo: 16,  hi: 31,  send: 24 }
  , { lo: 32,  hi: 47,  send: 40 }
  , { lo: 48,  hi: 63,  send: 56 }
  , { lo: 64,  hi: 79,  send: 72 }
  , { lo: 80,  hi: 95,  send: 88 }
  , { lo: 96,  hi: 111, send: 104 }
  , { lo: 112, hi: 127, send: 120 }
  ]

-- | Matches the `Record source` options in `config/pedals/itajara.json`. The
-- | two must agree; the JSON is the one the Detail view reads.
sourceSegments :: Array { lo :: Int, hi :: Int, send :: Int }
sourceSegments =
  [ { lo: 0,   hi: 15,  send: 0 }
  , { lo: 16,  hi: 31,  send: 16 }
  , { lo: 32,  hi: 47,  send: 32 }
  , { lo: 48,  hi: 63,  send: 48 }
  , { lo: 64,  hi: 79,  send: 64 }
  , { lo: 80,  hi: 95,  send: 80 }
  , { lo: 96,  hi: 111, send: 96 }
  , { lo: 112, hi: 127, send: 112 }
  ]
