module Pedals.Iridium (pedal) where

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
      { id: PedalId "iridium"
      , name: "Iridium"
      , brand: "Strymon"
      , color: Nothing
      , defaultChannel: 10
      , saveInstructions: Just "Hold the footswitch for 3 seconds until LED flashes."
      }
  , engage: SingleEngage (cc 102)
  , baseline: Map.fromFoldable
      [ cc 7 /\ mv 127, cc 9 /\ mv 0
      , cc 12 /\ mv 100, cc 13 /\ mv 64, cc 14 /\ mv 64, cc 15 /\ mv 64, cc 16 /\ mv 64
      , cc 17 /\ mv 0, cc 18 /\ mv 1, cc 19 /\ mv 1, cc 20 /\ mv 0, cc 21 /\ mv 0
      , cc 60 /\ mv 0, cc 102 /\ mv 127
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
          , Just (TwisterCC { cc: cc 19, center: Nothing, options: Just [ mv 1, mv 2, mv 3 ] })
          , Just (TwisterCC { cc: cc 18, center: Nothing, options: Just [ mv 1, mv 2, mv 3 ] })
          , Just (TwisterCC { cc: cc 20, center: Nothing, options: Just [ mv 0, mv 1, mv 2, mv 3, mv 4, mv 5, mv 6, mv 7, mv 8 ] })
          , Just (TwisterCC { cc: cc 7, center: Nothing, options: Nothing })
          , Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          ]
      , buttons:
          [ Just (TwisterToggle { cc: cc 102 })
          , Just (TwisterToggle { cc: cc 21 })
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
            , Toggle { cc: cc 21, label: "Amp Disable", onLabel: "On", offLabel: "Off", description: Just "Bypass amp modeling (cab/room/EQ remain)", labelSource: Nothing }
            ]
        }
      , { name: "Amp & Cab", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 19, label: "Amp", options:
                [ { label: "Round", value: mv 1, description: Just "Fender Deluxe Reverb" }
                , { label: "Chime", value: mv 2, description: Just "Vox AC30TB" }
                , { label: "Punch", value: mv 3, description: Just "Marshall Plexi" }
                ] }
            , Dropdown { cc: cc 20, label: "Cabinet", description: Nothing, options:
                [ { label: "Round A (1x12 Deluxe)", value: mv 0, description: Nothing }
                , { label: "Round B (1x12 Blues Jr)", value: mv 1, description: Nothing }
                , { label: "Round C (2x10 Vibrolux)", value: mv 2, description: Nothing }
                , { label: "Chime A (2x12 AC30 Blue)", value: mv 3, description: Nothing }
                , { label: "Chime B (1x12 AlNiCo)", value: mv 4, description: Nothing }
                , { label: "Chime C (4x12 Mesa)", value: mv 5, description: Nothing }
                , { label: "Punch A (4x12 G12M-25)", value: mv 6, description: Nothing }
                , { label: "Punch B (2x12 V30)", value: mv 7, description: Nothing }
                , { label: "Punch C (8x12 Marshall)", value: mv 8, description: Nothing }
                ] }
            ]
        }
      , { name: "Controls", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 12, label: Static "Level", description: Just "Output level"
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 13, label: Static "Drive", description: Just "Gain/overdrive amount"
              , annotations: [ ann 0 "Clean", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 14, label: Static "Bass", description: Nothing
              , annotations: [ ann 0 "Cut", ann 64 "Flat", ann 127 "Boost" ] }
            , Slider { cc: cc 15, label: Static "Mid", description: Just "Tone cut on Chime amp"
              , annotations: [ ann 0 "Cut", ann 64 "Flat", ann 127 "Boost" ] }
            , Slider { cc: cc 16, label: Static "Treble", description: Nothing
              , annotations: [ ann 0 "Cut", ann 64 "Flat", ann 127 "Boost" ] }
            ]
        }
      , { name: "Room", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 17, label: Static "Room Level", description: Nothing
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Max" ] }
            , Segmented { cc: cc 18, label: "Room Size", options:
                [ { label: "Small", value: mv 1, description: Nothing }
                , { label: "Medium", value: mv 2, description: Nothing }
                , { label: "Large", value: mv 3, description: Nothing }
                ] }
            ]
        }
      , { name: "Volume & Expression", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 7, label: Static "Volume Pedal", description: Nothing
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Full" ] }
            , Segmented { cc: cc 9, label: "Vol Pre/Post", options:
                [ { label: "Pre", value: mv 0, description: Just "Before amp/cab" }
                , { label: "Post", value: mv 127, description: Just "After amp/cab" }
                ] }
            , Toggle { cc: cc 60, label: "MIDI Expression", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Slider { cc: cc 100, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Heel", ann 127 "Toe" ] }
            ]
        }
      ]
  }
