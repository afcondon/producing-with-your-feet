module Pedals.Mood (pedal) where

import Color (fromHexString)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeCC, unsafeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), PedalDef, PedalId(..), RangeOption, SectionLayout(..))
import Data.Pedal.Layout (ConfigControlType(..), KnobLayer(..), PedalLayout)
import Data.Pedal.Engage (EngageConfig(..))
import Data.Tuple (Tuple(..))
import Data.Twister (TwisterButton(..), TwisterEncoder(..))

cc :: Int -> CC
cc = unsafeCC

mv :: Int -> MidiValue
mv = unsafeMidiValue

ann :: Int -> String -> Annotation
ann pos label = { position: mv pos, label }

rng :: Int -> Int -> String -> RangeOption
rng lo hi label = { lo: mv lo, hi: mv hi, label, description: Nothing }

infixr 6 Tuple as /\

pedal :: PedalDef
pedal =
  { meta:
      { id: PedalId "mood"
      , name: "MOOD"
      , shortName: "MD"
      , brand: "Chase Bliss"
      , color: fromHexString "#7b4f8a"
      , defaultChannel: 3
      , saveInstructions: Just "Hold both footswitches until LEDs blink, then release."
      }
  , engage: DualEngage
      { a: { cc: cc 102, label: "ML" }
      , b: { cc: cc 103, label: "Wet" }
      }
  , baseline: Map.fromFoldable
      [ cc 14 /\ mv 64, cc 15 /\ mv 64, cc 16 /\ mv 64, cc 17 /\ mv 64
      , cc 18 /\ mv 64, cc 19 /\ mv 64, cc 20 /\ mv 64
      , cc 21 /\ mv 0, cc 22 /\ mv 0, cc 23 /\ mv 0
      , cc 24 /\ mv 64, cc 25 /\ mv 55, cc 26 /\ mv 127, cc 27 /\ mv 64
      , cc 28 /\ mv 64, cc 29 /\ mv 0
      , cc 31 /\ mv 2, cc 32 /\ mv 2, cc 33 /\ mv 0
      , cc 51 /\ mv 0, cc 52 /\ mv 0, cc 53 /\ mv 5, cc 54 /\ mv 5, cc 55 /\ mv 0
      , cc 61 /\ mv 0, cc 62 /\ mv 0, cc 63 /\ mv 0, cc 64 /\ mv 0, cc 65 /\ mv 0
      , cc 66 /\ mv 0, cc 67 /\ mv 0, cc 68 /\ mv 0
      , cc 71 /\ mv 0, cc 72 /\ mv 0, cc 73 /\ mv 0, cc 74 /\ mv 0
      , cc 75 /\ mv 0, cc 76 /\ mv 0, cc 77 /\ mv 0, cc 78 /\ mv 0
      , cc 100 /\ mv 0, cc 102 /\ mv 0, cc 103 /\ mv 0
      , cc 105 /\ mv 0, cc 106 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { hue: 95
      , encoders:
          [ Just (TwisterCC { cc: cc 14, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 15, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 16, center: Nothing, options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 17, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 18, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 19, center: Nothing, options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 24, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 25, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 26, center: Nothing, options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 27, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 28, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 29, center: Nothing, options: Nothing })
          , Nothing
          ]
      , buttons:
          [ Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 102 })
          , Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 103 })
          , Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 105 })
          , Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 106 })
          ]
      }
  , modes: Nothing
  , layout: Just moodLayout
  , sections:
      [ { name: "Channels", compact: true, collapsed: false, layout: DualColumn, description: Nothing
        , controls:
            -- Left footswitch (Wet Channel)
            [ Toggle { cc: cc 103, label: "Wet Channel", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 105, label: "Wet Freeze", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Momentary { cc: cc 107, label: "Tap Tempo", value: mv 127, description: Nothing }
            -- Right footswitch (Micro-Looper)
            , Toggle { cc: cc 102, label: "Micro-Looper", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 106, label: "ML Overdub", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            ]
        }
      , { name: "Mix & Routing", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 15, label: Static "Mix", description: Just "Wet/dry balance (ramp speed when ramping)"
              , annotations: [ ann 0 "Dry", ann 64 "Even", ann 127 "Wet" ] }
            , Slider { cc: cc 18, label: Static "Clock", description: Just "Sample rate (2k-64k). Resolution/quality tradeoff."
              , annotations: [ ann 0 "Lo-fi", ann 64 "Mid", ann 127 "Hi-fi" ] }
            , Segmented { cc: cc 22, label: "Routing", options:
                [ { label: "IN", value: mv 0, description: Just "Wet processes input only" }
                , { label: "ML+IN", value: mv 2, description: Just "Input + micro-looper" }
                , { label: "ML", value: mv 3, description: Just "Micro-looper only" }
                ] }
            ]
        }
      , { name: "Micro-Looper", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 23, label: "Mode", options:
                [ { label: "Env", value: mv 0, description: Just "Audio-controlled looper" }
                , { label: "Tape", value: mv 2, description: Just "Tape-style looper" }
                , { label: "Stretch", value: mv 3, description: Just "Time-stretching looper" }
                ] }
            , Slider { cc: cc 16, label: Static "Length", description: Just "Micro-Looper slice/loop length"
              , annotations: [ ann 0 "Short", ann 64 "Mid", ann 127 "Long" ] }
            , Slider { cc: cc 19
              , label: ModeMap { cc: cc 23, labels: Map.fromFoldable [ mv 0 /\ "Sensitivity", mv 2 /\ "Speed/Dir", mv 3 /\ "Amount" ] }
              , description: Nothing
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            ]
        }
      , { name: "Wet Channel", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 21, label: "Mode", options:
                [ { label: "Reverb", value: mv 0, description: Just "Shimmer-y, smearable reverb" }
                , { label: "Delay", value: mv 2, description: Just "Clock-rate delay with feedback" }
                , { label: "Slip", value: mv 3, description: Just "Granular time slip" }
                ] }
            , Slider { cc: cc 14
              , label: ModeMap { cc: cc 21, labels: Map.fromFoldable [ mv 0 /\ "Decay", mv 2 /\ "Delay Time", mv 3 /\ "Refresh Rate" ] }
              , description: Nothing
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 17
              , label: ModeMap { cc: cc 21, labels: Map.fromFoldable [ mv 0 /\ "Smear", mv 2 /\ "Feedback", mv 3 /\ "Speed" ] }
              , description: Nothing
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            ]
        }
      , { name: "Hidden Options", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 24, label: Static "Stereo Width", description: Just "Panning width when Spread is on"
              , annotations: [ ann 0 "Narrow", ann 127 "Wide" ] }
            , RangeSelect { cc: cc 25, label: "Ramp Waveform", ranges:
                [ rng 0 7 "Triangle"
                , rng 8 35 "Square"
                , rng 36 68 "Sine"
                , rng 69 104 "Random"
                , rng 105 127 "Smooth"
                ] }
            , Slider { cc: cc 26, label: Static "Fade", description: Just "Loop fade during overdub"
              , annotations: [ ann 0 "Fast fade", ann 127 "No fade" ] }
            , Slider { cc: cc 27, label: Static "Tone", description: Just "Hi-cut filter for Wet Channel"
              , annotations: [ ann 0 "Dark", ann 127 "Bright" ] }
            , Slider { cc: cc 28, label: Static "Level Balance", description: Just "Relative loudness of channels"
              , annotations: [ ann 0 "ML loud", ann 64 "Even", ann 127 "Wet loud" ] }
            , Slider { cc: cc 29, label: Static "Direct ML", description: Just "Clean micro-loop blended into Wet"
              , annotations: [ ann 0 "None", ann 127 "Full" ] }
            , Segmented { cc: cc 31, label: "Sync", options:
                [ { label: "ML>Wet", value: mv 0, description: Just "ML synced to Wet" }
                , { label: "None", value: mv 2, description: Nothing }
                , { label: "Wet>ML", value: mv 3, description: Just "Wet synced to ML" }
                ] }
            , Segmented { cc: cc 32, label: "Spread Solo", options:
                [ { label: "Wet", value: mv 0, description: Nothing }
                , { label: "Both", value: mv 2, description: Nothing }
                , { label: "ML", value: mv 3, description: Nothing }
                ] }
            , Segmented { cc: cc 33, label: "Buffer Length", options:
                [ { label: "Half", value: mv 0, description: Just "MKI-style shorter" }
                , { label: "Full", value: mv 127, description: Nothing }
                ] }
            ]
        }
      , { name: "DIP Switches", compact: true, collapsed: true, layout: DipGrid
        , description: Just "Physical DIP switches \x2014 left bank (ramping), right bank (customize)"
        , controls:
            [ Toggle { cc: cc 61, label: "Time", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 62, label: "Modify L", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 63, label: "Clock", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 64, label: "Modify R", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 65, label: "Length", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 66, label: "Bounce", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 67, label: "Sweep", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 68, label: "Polarity", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 71, label: "Classic", onLabel: "On", offLabel: "Off", description: Just "MKI-style: noisy, deteriorating, resonant", labelSource: Nothing }
            , Toggle { cc: cc 72, label: "Miso", onLabel: "On", offLabel: "Off", description: Just "Mono in, stereo out", labelSource: Nothing }
            , Toggle { cc: cc 73, label: "Spread", onLabel: "On", offLabel: "Off", description: Just "Stereo processing", labelSource: Nothing }
            , Toggle { cc: cc 74, label: "Dry Kill", onLabel: "On", offLabel: "Off", description: Just "Remove dry signal", labelSource: Nothing }
            , Toggle { cc: cc 75, label: "Trails", onLabel: "On", offLabel: "Off", description: Just "Wet fades on bypass", labelSource: Nothing }
            , Toggle { cc: cc 76, label: "Latch", onLabel: "On", offLabel: "Off", description: Just "Hold becomes latch", labelSource: Nothing }
            , Toggle { cc: cc 77, label: "No Dub", onLabel: "On", offLabel: "Off", description: Just "Disable ML feedback/overdub", labelSource: Nothing }
            , Toggle { cc: cc 78, label: "Smooth", onLabel: "On", offLabel: "Off", description: Just "Smooth Clock knob (no stepping)", labelSource: Nothing }
            ]
        }
      , { name: "Ramping", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 52, label: "Ramp Active", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Slider { cc: cc 20, label: Static "Ramp Speed", description: Nothing
              , annotations: [ ann 0 "Slow", ann 127 "Fast" ] }
            ]
        }
      , { name: "Other", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 51, label: "MIDI Clock Follow", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Dropdown { cc: cc 53, label: "Wet Clock Div", description: Nothing, options:
                [ { label: "32nd", value: mv 0, description: Nothing }, { label: "16th", value: mv 1, description: Nothing }
                , { label: "8th trip", value: mv 2, description: Nothing }, { label: "8th", value: mv 3, description: Nothing }
                , { label: "Dot 8th", value: mv 4, description: Nothing }, { label: "Quarter", value: mv 5, description: Nothing }
                , { label: "Half", value: mv 6, description: Nothing }, { label: "Whole", value: mv 7, description: Nothing }
                ] }
            , Dropdown { cc: cc 54, label: "ML Clock Div", description: Nothing, options:
                [ { label: "32nd", value: mv 0, description: Nothing }, { label: "16th", value: mv 1, description: Nothing }
                , { label: "8th trip", value: mv 2, description: Nothing }, { label: "8th", value: mv 3, description: Nothing }
                , { label: "Dot 8th", value: mv 4, description: Nothing }, { label: "Quarter", value: mv 5, description: Nothing }
                , { label: "Half", value: mv 6, description: Nothing }, { label: "Whole", value: mv 7, description: Nothing }
                ] }
            , Toggle { cc: cc 55, label: "True Bypass", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Slider { cc: cc 100, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Min", ann 127 "Max" ] }
            ]
        }
      ]
  }

moodLayout :: PedalLayout
moodLayout =
  { groups:
      [ { id: "wet", label: "Wet", color: "#7b4f8a", mutedColor: "#d4c0dc" }
      , { id: "shared", label: "Shared", color: "#666", mutedColor: "#ccc" }
      , { id: "ml", label: "ML", color: "#4a7b6a", mutedColor: "#b8d4c8" }
      ]
  , knobs:
      -- Row A
      [ { col: 0, row: 0, group: "wet"
        , primaryCC: cc 14
        , primaryLabel: ModeMap { cc: cc 21, labels: Map.fromFoldable [ mv 0 /\ "Decay", mv 2 /\ "Delay Time", mv 3 /\ "Refresh Rate" ] }
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Just (cc 24), hiddenLabel: Just (Static "Stereo Width")
        , hiddenLayer: Just (ContinuousKnob { center: Nothing }) }
      , { col: 1, row: 0, group: "shared"
        , primaryCC: cc 15, primaryLabel: Static "Mix"
        , primaryLayer: ContinuousKnob { center: Just 64 }
        , hiddenCC: Just (cc 25), hiddenLabel: Just (Static "Ramp Wave")
        , hiddenLayer: Just SegmentedKnob }
      , { col: 2, row: 0, group: "ml"
        , primaryCC: cc 16, primaryLabel: Static "Length"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Just (cc 26), hiddenLabel: Just (Static "Fade")
        , hiddenLayer: Just (ContinuousKnob { center: Nothing }) }
      -- Row B
      , { col: 0, row: 1, group: "wet"
        , primaryCC: cc 17
        , primaryLabel: ModeMap { cc: cc 21, labels: Map.fromFoldable [ mv 0 /\ "Smear", mv 2 /\ "Feedback", mv 3 /\ "Speed" ] }
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Just (cc 27), hiddenLabel: Just (Static "Tone")
        , hiddenLayer: Just (ContinuousKnob { center: Nothing }) }
      , { col: 1, row: 1, group: "shared"
        , primaryCC: cc 18, primaryLabel: Static "Clock"
        , primaryLayer: ContinuousKnob { center: Just 64 }
        , hiddenCC: Just (cc 28), hiddenLabel: Just (Static "Level Bal")
        , hiddenLayer: Just (ContinuousKnob { center: Just 64 }) }
      , { col: 2, row: 1, group: "ml"
        , primaryCC: cc 19
        , primaryLabel: ModeMap { cc: cc 23, labels: Map.fromFoldable [ mv 0 /\ "Sensitivity", mv 2 /\ "Speed/Dir", mv 3 /\ "Amount" ] }
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Just (cc 29), hiddenLabel: Just (Static "Direct ML")
        , hiddenLayer: Just (ContinuousKnob { center: Nothing }) }
      -- Row C (toggle switches)
      , { col: 0, row: 2, group: "wet"
        , primaryCC: cc 21, primaryLabel: Static "Rev / Dly / Slip"
        , primaryLayer: SegmentedKnob
        , hiddenCC: Just (cc 31), hiddenLabel: Just (Static "ML>W / \x2014 / W>ML")
        , hiddenLayer: Just SegmentedKnob }
      , { col: 1, row: 2, group: "shared"
        , primaryCC: cc 22, primaryLabel: Static "IN / ML+IN / ML"
        , primaryLayer: SegmentedKnob
        , hiddenCC: Just (cc 32), hiddenLabel: Just (Static "Wet / Both / ML")
        , hiddenLayer: Just SegmentedKnob }
      , { col: 2, row: 2, group: "ml"
        , primaryCC: cc 23, primaryLabel: Static "Env / Tape / Str"
        , primaryLayer: SegmentedKnob
        , hiddenCC: Just (cc 33), hiddenLabel: Just (Static "Half / Full")
        , hiddenLayer: Just SegmentedKnob }
      ]
  , footswitches:
      [ { col: 0, cc: cc 103, label: "Wet", group: "wet"
        , ledCC: Just (cc 105), engagedColor: "#c75050", ledColor: "#50a060" }
      , { col: 2, cc: cc 102, label: "ML", group: "ml"
        , ledCC: Just (cc 106), engagedColor: "#50a060", ledColor: "#c75050" }
      ]
  , dipBanks:
      [ { label: "Ramping"
        , switches:
            [ { cc: cc 61, label: "Time",     index: 0 }
            , { cc: cc 62, label: "Modify L", index: 1 }
            , { cc: cc 63, label: "Clock",    index: 2 }
            , { cc: cc 64, label: "Modify R", index: 3 }
            , { cc: cc 65, label: "Length",   index: 4 }
            , { cc: cc 66, label: "Bounce",   index: 5 }
            , { cc: cc 67, label: "Sweep",    index: 6 }
            , { cc: cc 68, label: "Polarity", index: 7 }
            ]
        }
      , { label: "Customize"
        , switches:
            [ { cc: cc 71, label: "Classic",  index: 0 }
            , { cc: cc 72, label: "Miso",     index: 1 }
            , { cc: cc 73, label: "Spread",   index: 2 }
            , { cc: cc 74, label: "Dry Kill", index: 3 }
            , { cc: cc 75, label: "Trails",   index: 4 }
            , { cc: cc 76, label: "Latch",    index: 5 }
            , { cc: cc 77, label: "No Dub",   index: 6 }
            , { cc: cc 78, label: "Smooth",   index: 7 }
            ]
        }
      ]
  , config:
      [ { cc: cc 107, label: "Tap",     controlType: CfgToggle }
      , { cc: cc 52,  label: "Ramp",    controlType: CfgToggle }
      , { cc: cc 20,  label: "Speed",   controlType: CfgSlider }
      , { cc: cc 51,  label: "Clk Fol", controlType: CfgToggle }
      , { cc: cc 55,  label: "Bypass",  controlType: CfgToggle }
      ]
  , columns: 3
  , knobRows: 3
  , viewBox: { width: 320.0, height: 470.0 }
  }
