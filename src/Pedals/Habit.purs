module Pedals.Habit (pedal) where

import Color (fromHexString)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeCC, unsafeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), PedalDef, PedalId(..), RadioChoice, SectionLayout(..))
import Data.Pedal.Engage (EngageConfig(..))
import Data.Pedal.Layout (ConfigControlType(..), KnobLayer(..), PedalLayout)
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
      , shortName: "Ht"
      , brand: "Chase Bliss"
      , color: fromHexString "#d4a017"
      , defaultChannel: 15
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
      { hue: 12
      , encoders:
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
  , layout: Just habitLayout
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
            , Slider { cc: cc 17, label: Static "Modify", description: Just "Modifier depth. Its ends are labelled for the modifier chosen."
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
                [ modifier "Stepped Speed" 1 1 (Just steppedSpeedOneX)
                    [ ann 0 "Rev 4x", ann 64 "Stop", ann steppedSpeedOneX "1x", ann 127 "Fwd 4x" ]
                , modifier "Stability"     1 2 (Just 64)
                    [ ann 0 "Max, no noise", ann 64 "Neutral", ann 127 "Max, tape noise" ]
                , modifier "Trimmer"       1 3 (Just 64)
                    [ ann 0 "Cuts ends", ann 64 "Neutral", ann 127 "Cuts starts" ]
                , modifier "Off"           2 1 Nothing
                    [ ann 64 "Modifier off: no effect" ]
                , modifier "Smooth Speed"  3 1 (Just smoothSpeedOneX)
                    [ ann 0 "Rev 2x", ann 64 "Stop", ann smoothSpeedOneX "1x", ann 127 "Fwd 2x" ]
                , modifier "Filter"        3 2 (Just 64)
                    [ ann 0 "Low-pass", ann 64 "Neutral", ann 127 "High-pass" ]
                , modifier "Dropper"       3 3 (Just 64)
                    [ ann 0 "Random", ann 64 "Neutral", ann 127 "Patterned" ]
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

-- | Habit 3-way toggle: values 1, 2, 3
habit3 :: Array { lo :: Int, hi :: Int, send :: Int }
habit3 = [ { lo: 0, hi: 1, send: 1 }, { lo: 2, hi: 2, send: 2 }, { lo: 3, hi: 127, send: 3 } ]

-- | Donut view layout. Gold = echo parameters, dark = modifiers.
-- | No hidden layer (Habit has no alt knobs). DIP switches are not MIDI-controllable.
habitLayout :: PedalLayout
habitLayout =
  { groups:
      [ { id: "echo", label: "Echo", color: "#d4a017", mutedColor: "#e8d498" }
      , { id: "modifier", label: "Modifier", color: "#5a4a3a", mutedColor: "#b8b0a0" }
      ]
  , knobs:
      -- Row A (gold — echo parameters)
      [ { col: 0, row: 0, group: "echo"
        , primaryCC: cc 14, primaryLabel: Static "Level"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      , { col: 1, row: 0, group: "echo"
        , primaryCC: cc 15, primaryLabel: Static "Repeats"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      , { col: 2, row: 0, group: "echo"
        , primaryCC: cc 16, primaryLabel: Static "Size"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      -- Row B (dark — modifiers)
      , { col: 0, row: 1, group: "modifier"
        , primaryCC: cc 17, primaryLabel: Static "Modify"
        , primaryLayer: ContinuousKnob { center: Just 64 }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      , { col: 1, row: 1, group: "modifier"
        , primaryCC: cc 18, primaryLabel: Static "Spread"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      , { col: 2, row: 1, group: "modifier"
        , primaryCC: cc 19, primaryLabel: Static "Scan"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      -- Row C (toggle switches — modifier matrix + mode)
      , { col: 0, row: 2, group: "modifier"
        , primaryCC: cc 21, primaryLabel: Static "1 / 2 / 3"
        , primaryLayer: SegmentedKnob habit3
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      , { col: 1, row: 2, group: "modifier"
        , primaryCC: cc 22, primaryLabel: Static "L / Off / R"
        , primaryLayer: SegmentedKnob habit3
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      , { col: 2, row: 2, group: "echo"
        , primaryCC: cc 23, primaryLabel: Static "In / Out / Feed"
        , primaryLayer: SegmentedKnob habit3
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      ]
  , footswitches:
      [ { col: 0, cc: cc 24, label: "Tap/Hold", group: "modifier"
        , ledCC: Nothing, engagedColor: "#d4a017", ledColor: "#d4a017" }
      , { col: 2, cc: cc 102, label: "Bypass", group: "echo"
        , ledCC: Nothing, engagedColor: "#d4a017", ledColor: "#d4a017" }
      ]
  , dipBanks: []  -- Habit DIPs are not MIDI-controllable (InfoToggle only)
  , config:
      [ { cc: cc 51, label: "Clk Fol", controlType: CfgToggle }
      , { cc: cc 20, label: "Speed",   controlType: CfgSlider }
      ]
  , columns: 3
  , knobRows: 3
  , viewBox: { width: 320.0, height: 470.0 }
  }

-- | A modifier is a position of the left toggle (CC 21) and the centre toggle
-- | (CC 22), plus, where it has one, the Modify value (CC 17) at which it does
-- | nothing. Choosing it puts Modify there, so you start from neutral and turn
-- | away; Modify is not part of what the choice *is* (see `RadioChoice`).
-- | Off has no neutral: Modify does nothing, so it is left alone.
-- | `sides` are what Modify's ends mean under this modifier (manual p. 16-17:
-- | Stability is noise-free counter-clockwise and adds tape noise clockwise).
modifier :: String -> Int -> Int -> Maybe Int -> Array Annotation -> RadioChoice
modifier label bank number neutral sides =
  { label
  , values: Map.fromFoldable [ Tuple (cc 22) (mv bank), Tuple (cc 21) (mv number) ]
  , alsoSends: case neutral of
      Just n -> Map.singleton (cc 17) (mv n)
      Nothing -> Map.empty
  , annotates: Map.singleton (cc 17) sides
  }

-- | Stability, Trimmer, Filter and Dropper are neutral at the midpoint (AC,
-- | 2026-11-08). The two speeds are not: their midpoint is *stopped*, 0 and 127
-- | the extremes of reverse and forward (Stepped -4x..4x, Smooth -2x..2x), so
-- | neutral is 1x forward. On a linear scale that is 64 + 63.5/4 and
-- | 64 + 63.5/2. Stepped is quantised and its steps may not be linear: both
-- | values are to be checked by ear (echo pitch unchanged).
steppedSpeedOneX :: Int
steppedSpeedOneX = 80

smoothSpeedOneX :: Int
smoothSpeedOneX = 96
