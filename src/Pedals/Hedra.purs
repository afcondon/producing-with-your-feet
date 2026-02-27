module Pedals.Hedra (pedal) where

import Color (fromHexString)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeCC, unsafeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), PedalDef, PedalId(..), RangeOption, SectionLayout(..), ModeRangesMode)
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

-- Pitch voice interval tables (mode-dependent)

diatonicIntervals :: Array (Array RangeOption)
diatonicIntervals =
  [ [ rng 0 0 "Off", rng 1 7 "-2Oct", rng 8 11 "-1Oct", rng 116 119 "+1Oct", rng 120 127 "+2Oct" ]
  , [ rng 12 19 "-7", rng 20 27 "-6", rng 28 35 "-5", rng 36 43 "-4"
    , rng 44 51 "-3", rng 52 59 "-2", rng 60 67 "U"
    , rng 68 75 "+2", rng 76 83 "+3", rng 84 91 "+4"
    , rng 92 99 "+5", rng 100 107 "+6", rng 108 115 "+7"
    ]
  ]

pentatonicIntervals :: Array (Array RangeOption)
pentatonicIntervals =
  [ [ rng 0 0 "Off", rng 1 19 "-2Oct", rng 20 27 "-1Oct", rng 100 107 "+1Oct", rng 108 127 "+2Oct" ]
  , [ rng 28 35 "-5", rng 36 43 "-4", rng 44 51 "-3", rng 52 59 "-2"
    , rng 60 67 "U"
    , rng 68 75 "+2", rng 76 83 "+3", rng 84 91 "+4", rng 92 99 "+5"
    ]
  ]

pitchVoiceModes :: Array ModeRangesMode
pitchVoiceModes =
  [ { lo: mv 0, hi: mv 97, ranges: diatonicIntervals }
  , { lo: mv 98, hi: mv 127, ranges: pentatonicIntervals }
  ]

pedal :: PedalDef
pedal =
  { meta:
      { id: PedalId "hedra"
      , name: "Hedra"
      , shortName: "Ha"
      , brand: "Meris"
      , color: fromHexString "#8a8f94"
      , defaultChannel: 5
      , saveInstructions: Just "Hold ALT and tap the preset footswitch."
      }
  , engage: SingleEngage (cc 14)
  , baseline: Map.fromFoldable
      [ cc 4 /\ mv 0, cc 9 /\ mv 0, cc 14 /\ mv 0
      , cc 15 /\ mv 64, cc 16 /\ mv 0, cc 17 /\ mv 0, cc 18 /\ mv 64
      , cc 19 /\ mv 56, cc 20 /\ mv 56, cc 21 /\ mv 56
      , cc 22 /\ mv 0, cc 23 /\ mv 64, cc 24 /\ mv 0
      , cc 25 /\ mv 125, cc 26 /\ mv 125, cc 27 /\ mv 125
      , cc 28 /\ mv 0, cc 29 /\ mv 0, cc 30 /\ mv 0, cc 31 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { hue: 127
      , encoders:
          [ Just (TwisterCC { cc: cc 19, center: Just (mv 56), options: Nothing })
          , Just (TwisterCC { cc: cc 20, center: Just (mv 56), options: Nothing })
          , Just (TwisterCC { cc: cc 21, center: Just (mv 56), options: Nothing })
          , Just (TwisterCC { cc: cc 15, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 18, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 24, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 16, center: Nothing, options: Just [ mv 0, mv 7, mv 18, mv 31, mv 42, mv 53, mv 62, mv 72, mv 82, mv 92, mv 103, mv 114, mv 123 ] })
          , Just (TwisterCC { cc: cc 22, center: Nothing, options: Just [ mv 5, mv 24, mv 48, mv 68, mv 88, mv 108, mv 123 ] })
          , Just (TwisterCC { cc: cc 25, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 26, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 27, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 29, center: Nothing, options: Just [ mv 15, mv 47, mv 79, mv 111 ] })
          , Just (TwisterCC { cc: cc 17, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 23, center: Just (mv 64), options: Nothing })
          , Nothing, Nothing
          ]
      , buttons:
          [ Just (TwisterToggle { cc: cc 14 })
          , Just (TwisterToggle { cc: cc 9 })
          , Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          , Just (TwisterMomentary { cc: cc 28 })
          , Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 30 })
          , Just (TwisterToggle { cc: cc 31 })
          , Nothing, Nothing
          ]
      }
  , modes: Nothing
  , sections:
      [ { name: "Engage", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 14, label: "Bypass", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 9, label: "Half Speed", onLabel: "On", offLabel: "Off", description: Just "Delay runs at half speed", labelSource: Nothing }
            , Momentary { cc: cc 28, label: "Tap Tempo", value: mv 127, description: Nothing }
            ]
        }
      , { name: "Pitch Voices", compact: true, collapsed: false, layout: DefaultLayout
        , description: Just "Three independent pitch-shifted delay voices"
        , controls:
            [ ModeRanges { cc: cc 19, label: "Pitch 1", modeCC: cc 22, modes: pitchVoiceModes }
            , ModeRanges { cc: cc 20, label: "Pitch 2", modeCC: cc 22, modes: pitchVoiceModes }
            , ModeRanges { cc: cc 21, label: "Pitch 3", modeCC: cc 22, modes: pitchVoiceModes }
            ]
        }
      , { name: "Key & Scale", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ PianoKey { cc: cc 16, label: "Key"
              , options:
                  [ { label: "C", value: mv 0, description: Nothing }
                  , { label: "D\x266D", value: mv 7, description: Nothing }
                  , { label: "D", value: mv 18, description: Nothing }
                  , { label: "E\x266D", value: mv 31, description: Nothing }
                  , { label: "E", value: mv 42, description: Nothing }
                  , { label: "F", value: mv 53, description: Nothing }
                  , { label: "G\x266D", value: mv 62, description: Nothing }
                  , { label: "G", value: mv 72, description: Nothing }
                  , { label: "A\x266D", value: mv 82, description: Nothing }
                  , { label: "A", value: mv 92, description: Nothing }
                  , { label: "B\x266D", value: mv 103, description: Nothing }
                  , { label: "B", value: mv 114, description: Nothing }
                  ]
              , chromaticValue: mv 123
              }
            , RangeSelect { cc: cc 22, label: "Scale", ranges:
                [ rng 0 11 "Major"
                , rng 12 37 "Minor"
                , rng 38 58 "Mel Min"
                , rng 59 78 "Harm Min"
                , rng 79 97 "Dbl Harm"
                , rng 98 119 "Lyd Pent"
                , rng 120 127 "Min Pent"
                ] }
            ]
        }
      , { name: "Delay", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 15, label: Static "Tempo", description: Just "Delay time (10ms intervals)"
              , annotations: [ ann 0 "Short", ann 64 "Mid", ann 127 "Long" ] }
            , Slider { cc: cc 18, label: Static "Mix", description: Just "Dry/wet balance"
              , annotations: [ ann 0 "Dry", ann 64 "Even", ann 127 "Wet" ] }
            , Slider { cc: cc 24, label: Static "Feedback", description: Just "Delay feedback amount"
              , annotations: [ ann 0 "None", ann 64 "Mid", ann 127 "Max" ] }
            , RangeSelect { cc: cc 29, label: "Delay Mode", ranges:
                [ rng 0 31 "Series+Pitch Fdbk"
                , rng 32 63 "Series"
                , rng 64 95 "Dual+Cross Fdbk"
                , rng 96 127 "Dual"
                ] }
            ]
        }
      , { name: "Time Divisions", compact: true, collapsed: true, layout: DefaultLayout
        , description: Just "Rhythmic subdivision per voice (fraction of tempo)"
        , controls:
            [ Slider { cc: cc 25, label: Static "Div 1", description: Just "Voice 1 time division"
              , annotations: [ ann 0 "Off", ann 59 "1/2", ann 92 "Dot 8th", ann 125 "Quarter" ] }
            , Slider { cc: cc 26, label: Static "Div 2", description: Just "Voice 2 time division"
              , annotations: [ ann 0 "Off", ann 59 "1/2", ann 92 "Dot 8th", ann 125 "Quarter" ] }
            , Slider { cc: cc 27, label: Static "Div 3", description: Just "Voice 3 time division"
              , annotations: [ ann 0 "Off", ann 59 "1/2", ann 92 "Dot 8th", ann 125 "Quarter" ] }
            ]
        }
      , { name: "Other", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 17, label: Static "Micro Tune", description: Just "Detune all voices slightly"
              , annotations: [ ann 0 "None", ann 127 "Max detune" ] }
            , Slider { cc: cc 23, label: Static "Pitch Correct / Glide"
              , description: Just "Low: correction mode. High: glide amount."
              , annotations: [ ann 0 "No correct", ann 64 "Strict", ann 127 "Max glide" ] }
            , Toggle { cc: cc 30, label: "Pitch Smoothing", onLabel: "On", offLabel: "Off", description: Just "Smooth pitch knob transitions", labelSource: Nothing }
            , Toggle { cc: cc 31, label: "Volume Swell", onLabel: "On", offLabel: "Off", description: Just "Auto swell on pick attack", labelSource: Nothing }
            , Slider { cc: cc 4, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Heel", ann 127 "Toe" ] }
            ]
        }
      ]
  }
