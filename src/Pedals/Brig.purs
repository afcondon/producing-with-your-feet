module Pedals.Brig (pedal) where

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
      { id: PedalId "brig"
      , name: "Brig"
      , brand: "Strymon"
      , color: fromHexString "#2d6a4f"
      , defaultChannel: 14
      , saveInstructions: Just "Hold the footswitch for 3 seconds until LED flashes."
      }
  , engage: SingleEngage (cc 102)
  , baseline: Map.fromFoldable
      [ cc 11 /\ mv 1, cc 12 /\ mv 64, cc 13 /\ mv 64
      , cc 14 /\ mv 64, cc 15 /\ mv 0, cc 16 /\ mv 64
      , cc 17 /\ mv 1, cc 102 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { encoders:
          [ Just (TwisterCC { cc: cc 11, center: Nothing, options: Just [ mv 1, mv 2, mv 3 ] })
          , Just (TwisterCC { cc: cc 12, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 13, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 14, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 15, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 16, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 17, center: Nothing, options: Just [ mv 0, mv 1, mv 2, mv 3 ] })
          , Nothing
          , Nothing, Nothing, Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          ]
      , buttons:
          [ Just (TwisterToggle { cc: cc 102 })
          , Just (TwisterToggle { cc: cc 97 })
          , Nothing, Nothing
          , Just (TwisterMomentary { cc: cc 93 })
          , Nothing, Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          ]
      }
  , modes: Nothing
  , sections:
      [ { name: "Engage", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 102, label: "Bypass", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 97, label: "Infinite", onLabel: "On", offLabel: "Off", description: Just "Hold repeats indefinitely", labelSource: Nothing }
            , Momentary { cc: cc 93, label: "Tap Tempo", value: mv 127, description: Nothing }
            ]
        }
      , { name: "Controls", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 11, label: "Voice", options:
                [ { label: "3205", value: mv 1, description: Just "MN3205 chip: warm, dark" }
                , { label: "3005", value: mv 2, description: Just "MN3005 chip: clear, bright" }
                , { label: "Multi", value: mv 3, description: Just "Multi-head: rhythmic patterns" }
                ] }
            , Slider { cc: cc 12, label: Static "Time", description: Just "Delay time"
              , annotations: [ ann 0 "Short", ann 64 "Mid", ann 127 "Long" ] }
            , Slider { cc: cc 13, label: Static "Filter", description: Just "EQ, noise, bucket loss of repeats"
              , annotations: [ ann 0 "Dark", ann 64 "Mid", ann 127 "Bright" ] }
            , Slider { cc: cc 14, label: Static "Repeats", description: Just "Feedback. High = oscillation."
              , annotations: [ ann 0 "Single", ann 64 "Mid", ann 127 "Runaway" ] }
            , Slider { cc: cc 15, label: Static "Mod", description: Just "LFO modulation of delay time"
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 16, label: Static "Mix", description: Just "Wet/dry balance"
              , annotations: [ ann 0 "Dry", ann 64 "Even", ann 127 "Wet" ] }
            ]
        }
      , { name: "Settings", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Dropdown { cc: cc 17, label: "Tap Division", description: Nothing, options:
                [ { label: "Triplet", value: mv 0, description: Nothing }
                , { label: "Eighth", value: mv 1, description: Nothing }
                , { label: "Dotted 8th", value: mv 2, description: Nothing }
                , { label: "Quarter", value: mv 3, description: Nothing }
                ] }
            , Slider { cc: cc 100, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Heel", ann 127 "Toe" ] }
            ]
        }
      ]
  }
