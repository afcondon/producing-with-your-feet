module Pedals.Lex (pedal) where

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
      { id: PedalId "lex"
      , name: "Lex"
      , shortName: "Lx"
      , brand: "Strymon"
      , color: fromHexString "#8b4513"
      , defaultChannel: 8
      , saveInstructions: Just "Hold the footswitch for 3 seconds until LED flashes."
      }
  , engage: SingleEngage (cc 102)
  , baseline: Map.fromFoldable
      [ cc 0 /\ mv 0, cc 11 /\ mv 1, cc 12 /\ mv 64, cc 13 /\ mv 64
      , cc 14 /\ mv 64, cc 15 /\ mv 64
      , cc 16 /\ mv 1, cc 17 /\ mv 64, cc 18 /\ mv 64, cc 19 /\ mv 64
      , cc 20 /\ mv 0, cc 21 /\ mv 0, cc 22 /\ mv 0
      , cc 23 /\ mv 64, cc 24 /\ mv 64
      , cc 25 /\ mv 0, cc 60 /\ mv 0, cc 63 /\ mv 0, cc 102 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { hue: 8
      , encoders:
          [ Just (TwisterCC { cc: cc 12, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 17, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 19, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 18, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 14, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 15, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 11, center: Nothing, options: Just [ mv 1, mv 2 ] })
          , Just (TwisterCC { cc: cc 16, center: Nothing, options: Just [ mv 1, mv 2, mv 3 ] })
          , Just (TwisterCC { cc: cc 23, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 24, center: Nothing, options: Nothing })
          , Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          ]
      , buttons:
          [ Just (TwisterToggle { cc: cc 102 })
          , Just (TwisterToggle { cc: cc 22 })
          , Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          , Just (TwisterMomentary { cc: cc 93 })
          , Nothing, Nothing
          , Just (TwisterMomentary { cc: cc 97 })
          , Nothing, Nothing, Nothing, Nothing
          ]
      }
  , modes: Nothing
  , sections:
      [ { name: "Bypass", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 102, label: "On", onLabel: "On", offLabel: "Bypass", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 22, label: "Slow/Fast", onLabel: "Fast", offLabel: "Slow", description: Just "Toggle rotor speed", labelSource: Nothing }
            ]
        }
      , { name: "Rotary", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 11, label: "Mic", options:
                [ { label: "Front", value: mv 1, description: Nothing }
                , { label: "Rear", value: mv 2, description: Nothing }
                ] }
            , Slider { cc: cc 12, label: Static "Speed", description: Just "Rotor speed for current Slow/Fast mode"
              , annotations: [ ann 0 "Slow", ann 64 "Mid", ann 127 "Fast" ] }
            , Slider { cc: cc 13, label: Static "Speed (Full Range)", description: Just "Full 0-127 speed control"
              , annotations: [ ann 0 "Slow", ann 64 "Mid", ann 127 "Fast" ] }
            , Slider { cc: cc 14, label: Static "Mic Distance", description: Just "Stereo mic distance from cabinet"
              , annotations: [ ann 0 "Pronounced", ann 64 "Mid", ann 127 "Ambient" ] }
            , Slider { cc: cc 15, label: Static "Horn Level", description: Just "High-frequency horn volume"
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Segmented { cc: cc 16, label: "Ramp", options:
                [ { label: "Slow", value: mv 1, description: Just "Slow acceleration" }
                , { label: "Med", value: mv 2, description: Just "Medium acceleration" }
                , { label: "Fast", value: mv 3, description: Just "Fast acceleration" }
                ] }
            ]
        }
      , { name: "Amp", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 17, label: Static "Volume", description: Just "+/- 6dB boost/cut"
              , annotations: [ ann 0 "-6dB", ann 64 "Unity", ann 127 "+6dB" ] }
            , Slider { cc: cc 19, label: Static "Preamp Drive", description: Just "Tube preamp drive"
              , annotations: [ ann 0 "Clean", ann 64 "Mid", ann 127 "Driven" ] }
            , Slider { cc: cc 18, label: Static "Dry", description: Just "Dry signal blend"
              , annotations: [ ann 0 "Off", ann 64 "50/50", ann 127 "Full" ] }
            ]
        }
      , { name: "Settings", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 20, label: "Bi-Amp Mode", options:
                [ { label: "Stereo", value: mv 0, description: Nothing }
                , { label: "Bi-Amp", value: mv 1, description: Nothing }
                ] }
            , Segmented { cc: cc 21, label: "Cab Filter", options:
                [ { label: "Guitar Amp", value: mv 0, description: Nothing }
                , { label: "Full Range", value: mv 1, description: Nothing }
                ] }
            , Slider { cc: cc 23, label: Static "Slow Speed", description: Just "Speed in Slow mode"
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 24, label: Static "Fast Speed", description: Just "Speed in Fast mode"
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Dropdown { cc: cc 25, label: "Clock Tempo Mult/Div", description: Nothing, options:
                [ { label: "x4", value: mv 0, description: Nothing }
                , { label: "x3", value: mv 1, description: Nothing }
                , { label: "x2", value: mv 2, description: Nothing }
                , { label: "x1", value: mv 3, description: Nothing }
                , { label: "1/2", value: mv 4, description: Nothing }
                , { label: "1/3", value: mv 5, description: Nothing }
                , { label: "1/4", value: mv 6, description: Nothing }
                ] }
            , Toggle { cc: cc 60, label: "MIDI Expression", onLabel: "On", offLabel: "Off", description: Just "Respond to CC#100", labelSource: Nothing }
            , Toggle { cc: cc 63, label: "MIDI Clock", onLabel: "On", offLabel: "Off", description: Just "Sync to incoming clock", labelSource: Nothing }
            , Slider { cc: cc 100, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Heel", ann 127 "Toe" ] }
            , Momentary { cc: cc 93, label: "Tap", value: mv 127, description: Nothing }
            , Slider { cc: cc 97, label: Static "Brake", description: Just "Hold to engage brake"
              , annotations: [ ann 0 "Release", ann 127 "Hold" ] }
            , Segmented { cc: cc 0, label: "Bank Select", options:
                [ { label: "Bank 1", value: mv 0, description: Nothing }
                , { label: "Bank 2", value: mv 1, description: Nothing }
                , { label: "Bank 3", value: mv 2, description: Nothing }
                ] }
            ]
        }
      ]
  }
