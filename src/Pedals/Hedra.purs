module Pedals.Hedra (pedal) where

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
      { id: PedalId "hedra"
      , name: "Hedra"
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
      { encoders:
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
            [ Slider { cc: cc 19, label: Static "Pitch 1", description: Just "Interval for voice 1"
              , annotations: [ ann 0 "-2 Oct", ann 56 "Unison", ann 116 "+1 Oct", ann 127 "+2 Oct" ] }
            , Slider { cc: cc 20, label: Static "Pitch 2", description: Just "Interval for voice 2"
              , annotations: [ ann 0 "-2 Oct", ann 56 "Unison", ann 116 "+1 Oct", ann 127 "+2 Oct" ] }
            , Slider { cc: cc 21, label: Static "Pitch 3", description: Just "Interval for voice 3"
              , annotations: [ ann 0 "-2 Oct", ann 56 "Unison", ann 116 "+1 Oct", ann 127 "+2 Oct" ] }
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
            , Segmented { cc: cc 22, label: "Scale", options:
                [ { label: "Major", value: mv 5, description: Nothing }
                , { label: "Minor", value: mv 24, description: Nothing }
                , { label: "Mel Min", value: mv 48, description: Nothing }
                , { label: "Harm Min", value: mv 68, description: Nothing }
                , { label: "Dbl Harm", value: mv 88, description: Nothing }
                , { label: "Lyd Pent", value: mv 108, description: Nothing }
                , { label: "Min Pent", value: mv 123, description: Nothing }
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
            , Dropdown { cc: cc 29, label: "Delay Mode", description: Nothing, options:
                [ { label: "Series+Pitch Fdbk", value: mv 15, description: Nothing }
                , { label: "Series", value: mv 47, description: Nothing }
                , { label: "Dual+Cross Fdbk", value: mv 79, description: Nothing }
                , { label: "Dual", value: mv 111, description: Nothing }
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
