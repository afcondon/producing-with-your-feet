module Pedals.Habit (pedal) where

import Color (fromHexString)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeCC, unsafeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), PedalDef, PedalId(..), SectionLayout(..))
import Data.Pedal.Engage (EngageConfig(..))
import Data.Tuple (Tuple(..))
import Data.Twister (TwisterButton(..), TwisterEncoder(..))

cc :: Int -> CC
cc = unsafeCC

mv :: Int -> MidiValue
mv = unsafeMidiValue

ann :: Int -> String -> Annotation
ann pos label = { position: mv pos, label }

infixr 6 Tuple as /\

pedal :: PedalDef
pedal =
  { meta:
      { id: PedalId "habit"
      , name: "Habit"
      , brand: "Chase Bliss"
      , color: fromHexString "#d4a017"
      , defaultChannel: 16
      , saveInstructions: Just "Hold both footswitches until LEDs blink, then release."
      }
  , engage: SingleEngage (cc 102)
  , baseline: Map.fromFoldable
      [ cc 14 /\ mv 64, cc 15 /\ mv 64, cc 16 /\ mv 64, cc 17 /\ mv 64
      , cc 18 /\ mv 0, cc 19 /\ mv 0, cc 20 /\ mv 64
      , cc 21 /\ mv 1, cc 22 /\ mv 2, cc 23 /\ mv 2
      , cc 24 /\ mv 0, cc 25 /\ mv 0
      , cc 51 /\ mv 0, cc 102 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { encoders:
          [ Just (TwisterCC { cc: cc 14, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 15, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 16, center: Nothing, options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 17, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 18, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 19, center: Nothing, options: Nothing })
          , Nothing
          , Nothing, Nothing, Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          ]
      , buttons:
          [ Nothing, Nothing, Nothing
          , Just (TwisterSet { cc: cc 23, value: mv 1 })
          , Nothing
          , Just (TwisterSet { cc: cc 18, value: mv 0 })
          , Just (TwisterSet { cc: cc 19, value: mv 0 })
          , Just (TwisterSet { cc: cc 23, value: mv 2 })
          , Just (TwisterSet { cc: cc 22, value: mv 1 })
          , Just (TwisterSet { cc: cc 22, value: mv 2 })
          , Just (TwisterSet { cc: cc 22, value: mv 3 })
          , Just (TwisterSet { cc: cc 23, value: mv 3 })
          , Just (TwisterSet { cc: cc 21, value: mv 1 })
          , Just (TwisterSet { cc: cc 21, value: mv 2 })
          , Just (TwisterSet { cc: cc 21, value: mv 3 })
          , Just (TwisterSet { cc: cc 17, value: mv 64 })
          ]
      }
  , modes: Nothing
  , sections:
      [ { name: "Engage", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 102, label: "Bypass", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 24, label: "Loop", onLabel: "On", offLabel: "Off", description: Just "Freeze echo, play overtop", labelSource: Nothing }
            , Toggle { cc: cc 25, label: "Scan Hold", onLabel: "On", offLabel: "Off", description: Just "Momentary auto-scanning", labelSource: Nothing }
            , Momentary { cc: cc 26, label: "Clear Memory", value: mv 127, description: Nothing }
            , Momentary { cc: cc 93, label: "Tap Tempo", value: mv 127, description: Nothing }
            ]
        }
      , { name: "Main Controls", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 14, label: Static "Volume", description: Just "Echo wet level"
              , annotations: [ ann 0 "Silent", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 15, label: Static "Repeats", description: Just "Echo repeats. Max = infinite loop."
              , annotations: [ ann 0 "Single", ann 64 "Mid", ann 127 "Infinite" ] }
            , Slider { cc: cc 16, label: Static "Size", description: Just "Delay time (50ms to 60s)"
              , annotations: [ ann 0 "50ms", ann 64 "Mid", ann 127 "60s" ] }
            , Slider { cc: cc 17, label: Static "Modify", description: Just "Modifier depth. Noon = neutral."
              , annotations: [ ann 0 "CCW", ann 64 "Neutral", ann 127 "CW" ] }
            , Slider { cc: cc 18, label: Static "Spread", description: Just "Secondary echo / multi-tap distance"
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 19, label: Static "Scan", description: Just "Auto: scan frequency. Manual: memory position."
              , annotations: [ ann 0 "Off/Now", ann 64 "Mid", ann 127 "Max/Past" ] }
            ]
        }
      , { name: "Modifier", compact: true, collapsed: false, layout: DefaultLayout
        , description: Just "Select modifier. MOD knob controls the selected modifier."
        , controls:
            [ RadioGroup { label: "Modifier", mapping:
                [ { label: "Stepped Speed", values: Map.fromFoldable [ cc 22 /\ mv 1, cc 21 /\ mv 1 ] }
                , { label: "Stability",     values: Map.fromFoldable [ cc 22 /\ mv 1, cc 21 /\ mv 2 ] }
                , { label: "Trimmer",       values: Map.fromFoldable [ cc 22 /\ mv 1, cc 21 /\ mv 3 ] }
                , { label: "Off",           values: Map.fromFoldable [ cc 22 /\ mv 2, cc 21 /\ mv 1 ] }
                , { label: "Smooth Speed",  values: Map.fromFoldable [ cc 22 /\ mv 3, cc 21 /\ mv 1 ] }
                , { label: "Filter",        values: Map.fromFoldable [ cc 22 /\ mv 3, cc 21 /\ mv 2 ] }
                , { label: "Dropper",       values: Map.fromFoldable [ cc 22 /\ mv 3, cc 21 /\ mv 3 ] }
                ] }
            , Segmented { cc: cc 23, label: "Mode", options:
                [ { label: "In", value: mv 1, description: Just "Accumulating: modifier stacks on each echo" }
                , { label: "Out", value: mv 2, description: Just "Consistent: each echo sounds the same" }
                , { label: "Feed", value: mv 3, description: Just "Output re-recorded to memory" }
                ] }
            ]
        }
      , { name: "DIP Switches", compact: true, collapsed: true, layout: DipGrid
        , description: Just "Physical DIP switches \x2014 not MIDI controllable. Mark to match your pedal."
        , controls:
            [ InfoToggle { key: "dip:repeats", label: "Repeats", description: Nothing }
            , InfoToggle { key: "dip:size", label: "Size", description: Nothing }
            , InfoToggle { key: "dip:modify", label: "Modify", description: Nothing }
            , InfoToggle { key: "dip:spread", label: "Spread", description: Nothing }
            , InfoToggle { key: "dip:scan", label: "Scan", description: Nothing }
            , InfoToggle { key: "dip:bounce", label: "Bounce", description: Nothing }
            , InfoToggle { key: "dip:random", label: "Random", description: Nothing }
            , InfoToggle { key: "dip:shape", label: "Shape", description: Nothing }
            , InfoToggle { key: "dip:manual", label: "Manual", description: Nothing }
            , InfoToggle { key: "dip:collect", label: "Collect", description: Nothing }
            , InfoToggle { key: "dip:dry_kill", label: "Dry Kill", description: Nothing }
            , InfoToggle { key: "dip:always", label: "Always", description: Nothing }
            , InfoToggle { key: "dip:latch", label: "Latch", description: Nothing }
            , InfoToggle { key: "dip:wipe", label: "Wipe", description: Nothing }
            , InfoToggle { key: "dip:sweep", label: "Sweep", description: Nothing }
            , InfoToggle { key: "dip:polarity", label: "Polarity", description: Nothing }
            ]
        }
      , { name: "Other", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 51, label: "MIDI Clock Follow", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Slider { cc: cc 20, label: Static "Ramp Speed", description: Nothing
              , annotations: [ ann 0 "Slow", ann 127 "Fast" ] }
            ]
        }
      ]
  }
