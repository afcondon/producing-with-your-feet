module Pedals.Riverside (pedal) where

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
      { id: PedalId "riverside"
      , name: "Riverside"
      , brand: "Strymon"
      , color: fromHexString "#b8860b"
      , defaultChannel: 11
      , saveInstructions: Just "Hold the footswitch for 3 seconds until LED flashes."
      }
  , engage: SingleEngage (cc 102)
  , baseline: Map.fromFoldable
      [ cc 7 /\ mv 127
      , cc 12 /\ mv 64, cc 13 /\ mv 64, cc 14 /\ mv 64, cc 15 /\ mv 64, cc 16 /\ mv 64
      , cc 17 /\ mv 0, cc 18 /\ mv 0, cc 19 /\ mv 2, cc 20 /\ mv 2, cc 21 /\ mv 2, cc 22 /\ mv 0
      , cc 60 /\ mv 0, cc 102 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { encoders:
          [ Just (TwisterCC { cc: cc 12, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 13, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 14, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 15, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 16, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 17, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 19, center: Nothing, options: Just [ mv 2, mv 1 ] })
          , Just (TwisterCC { cc: cc 21, center: Nothing, options: Just [ mv 1, mv 2, mv 3 ] })
          , Just (TwisterCC { cc: cc 20, center: Nothing, options: Just [ mv 2, mv 1 ] })
          , Just (TwisterCC { cc: cc 22, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 7, center: Nothing, options: Nothing })
          , Nothing
          , Nothing, Nothing, Nothing, Nothing
          ]
      , buttons:
          [ Just (TwisterToggle { cc: cc 102 })
          , Just (TwisterToggle { cc: cc 18 })
          , Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          ]
      }
  , modes: Nothing
  , sections:
      [ { name: "Engage", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 102, label: "Bypass", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 18, label: "Boost", onLabel: "On", offLabel: "Off", description: Just "Post-drive analog boost (+6dB)", labelSource: Nothing }
            ]
        }
      , { name: "Controls", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 12, label: Static "Level", description: Just "Output volume when engaged"
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 13, label: Static "Drive", description: Just "Gain/overdrive amount"
              , annotations: [ ann 0 "Clean", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 14, label: Static "Bass", description: Just "Active shelving low EQ"
              , annotations: [ ann 0 "Cut", ann 64 "Flat", ann 127 "Boost" ] }
            , Slider { cc: cc 15, label: Static "Mid", description: Just "Parametric mid boost/cut"
              , annotations: [ ann 0 "Cut", ann 64 "Flat", ann 127 "Boost" ] }
            , Slider { cc: cc 16, label: Static "Treble", description: Just "Active shelf high EQ"
              , annotations: [ ann 0 "Cut", ann 64 "Flat", ann 127 "Boost" ] }
            , Slider { cc: cc 17, label: Static "Boost Level", description: Just "Post-drive boost amount"
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "+6dB" ] }
            ]
        }
      , { name: "Character", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 19, label: "Gain", options:
                [ { label: "Low", value: mv 2, description: Just "Lower gain structure" }
                , { label: "High", value: mv 1, description: Just "Higher gain structure" }
                ] }
            , Segmented { cc: cc 20, label: "Push", options:
                [ { label: "Norm", value: mv 2, description: Nothing }
                , { label: "Mid", value: mv 1, description: Just "Mid-band EQ push" }
                ] }
            , Segmented { cc: cc 21, label: "Presence", options:
                [ { label: "+", value: mv 1, description: Just "Enhanced top end" }
                , { label: "-", value: mv 2, description: Just "Reduced top end" }
                , { label: "Mid", value: mv 3, description: Just "Center position" }
                ] }
            ]
        }
      , { name: "Other", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 22, label: Static "Noise Gate", description: Just "Gate threshold"
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 7, label: Static "Volume Pedal", description: Nothing
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Full" ] }
            , Toggle { cc: cc 60, label: "MIDI Expression", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Slider { cc: cc 100, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Heel", ann 127 "Toe" ] }
            ]
        }
      ]
  }
