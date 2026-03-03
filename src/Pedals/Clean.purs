module Pedals.Clean (pedal) where

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
      { id: PedalId "clean"
      , name: "Clean"
      , shortName: "Cl"
      , brand: "Chase Bliss"
      , color: fromHexString "#2a9d8f"
      , defaultChannel: 4
      , saveInstructions: Just "Hold both footswitches until LEDs blink, then release."
      }
  , engage: SingleEngage (cc 102)
  , baseline: Map.fromFoldable
      [ cc 14 /\ mv 64, cc 15 /\ mv 64, cc 16 /\ mv 64, cc 17 /\ mv 64
      , cc 18 /\ mv 64, cc 19 /\ mv 64, cc 20 /\ mv 64
      , cc 21 /\ mv 0, cc 22 /\ mv 2, cc 23 /\ mv 2
      , cc 24 /\ mv 64, cc 25 /\ mv 64, cc 26 /\ mv 64, cc 27 /\ mv 64
      , cc 28 /\ mv 64, cc 29 /\ mv 64
      , cc 31 /\ mv 0, cc 32 /\ mv 0, cc 33 /\ mv 2
      , cc 52 /\ mv 0
      , cc 61 /\ mv 0, cc 62 /\ mv 0, cc 63 /\ mv 0, cc 64 /\ mv 0, cc 65 /\ mv 0
      , cc 66 /\ mv 0, cc 67 /\ mv 0, cc 68 /\ mv 0
      , cc 71 /\ mv 0, cc 72 /\ mv 0, cc 73 /\ mv 0, cc 74 /\ mv 0
      , cc 75 /\ mv 0, cc 76 /\ mv 0, cc 77 /\ mv 0, cc 78 /\ mv 0
      , cc 100 /\ mv 0, cc 102 /\ mv 0, cc 103 /\ mv 0
      , cc 105 /\ mv 0, cc 106 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { hue: 60
      , encoders:
          [ Just (TwisterCC { cc: cc 14, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 15, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 16, center: Nothing, options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 17, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 18, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 19, center: Nothing, options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 24, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 25, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 26, center: Nothing, options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 27, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 28, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 29, center: Nothing, options: Nothing })
          , Nothing
          ]
      , buttons:
          [ Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 102 })
          , Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 103 })
          , Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 106 })
          , Nothing, Nothing, Nothing
          , Nothing
          ]
      }
  , modes: Nothing
  , layout: Nothing
  , sections:
      [ { name: "Engage", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 102, label: "Bypass", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 103, label: "Swell", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 106, label: "Dynamics Max", onLabel: "On", offLabel: "Off", description: Just "Maximize sag effect", labelSource: Nothing }
            ]
        }
      , { name: "Main Controls", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 14, label: Static "Dynamics", description: Just "Compression amount: compress > limit > sag"
              , annotations: [ ann 0 "Light", ann 64 "Limit", ann 127 "Sag" ] }
            , Slider { cc: cc 15, label: Static "Sensitivity", description: Just "Dynamic threshold (ramp speed when ramping)"
              , annotations: [ ann 0 "Low", ann 64 "Mid", ann 127 "High" ] }
            , Slider { cc: cc 16, label: Static "Wet", description: Just "Compressed signal level"
              , annotations: [ ann 0 "Off", ann 64 "Unity", ann 127 "Boost" ] }
            , Slider { cc: cc 17, label: Static "Attack", description: Just "Compression onset (0.5ms to 300ms)"
              , annotations: [ ann 0 "Fast", ann 64 "Mid", ann 127 "Slow" ] }
            , Slider { cc: cc 18, label: Static "EQ", description: Just "Dynamic one-knob EQ"
              , annotations: [ ann 0 "Cut highs", ann 64 "Flat", ann 127 "Cut lows" ] }
            , Slider { cc: cc 19, label: Static "Dry", description: Just "Unprocessed signal level (parallel blend)"
              , annotations: [ ann 0 "Off", ann 64 "Unity", ann 127 "Boost" ] }
            ]
        }
      , { name: "Character", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 21, label: "Release", options:
                [ { label: "Fast", value: mv 0, description: Just "50ms release" }
                , { label: "User", value: mv 2, description: Just "Custom (set in Hidden Options)" }
                , { label: "Slow", value: mv 3, description: Just "1.5s release" }
                ] }
            , Segmented { cc: cc 22, label: "EQ Mode", options:
                [ { label: "Shifty", value: mv 0, description: Just "EQ shifts on playing, sweeps back on stop" }
                , { label: "Manual", value: mv 2, description: Just "Classic fixed EQ" }
                , { label: "Modulated", value: mv 3, description: Just "EQ modulates when playing" }
                ] }
            , Segmented { cc: cc 23, label: "Physics", options:
                [ { label: "Wobbly", value: mv 0, description: Just "Subtle spring-like movement" }
                , { label: "Off", value: mv 2, description: Nothing }
                , { label: "Twitchy", value: mv 3, description: Just "Unstable, erratic movement" }
                ] }
            ]
        }
      , { name: "Hidden Options", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 24, label: Static "Gate Release", description: Just "Noise gate re-engage speed"
              , annotations: [ ann 0 "Fast", ann 127 "Slow" ] }
            , Slider { cc: cc 25, label: Static "Gate Sensitivity", description: Just "Noise gate threshold"
              , annotations: [ ann 0 "Low", ann 127 "High" ] }
            , Slider { cc: cc 26, label: Static "Swell In", description: Just "Time to reach full volume"
              , annotations: [ ann 0 "Fast", ann 127 "Slow" ] }
            , Slider { cc: cc 27, label: Static "User Release", description: Just "Custom release for User position"
              , annotations: [ ann 0 "50ms", ann 64 "500ms", ann 127 "1s" ] }
            , Slider { cc: cc 28, label: Static "Balance Filter", description: Just "Filter bass from envelope follower"
              , annotations: [ ann 0 "Full range", ann 127 "Highs only" ] }
            , Slider { cc: cc 29, label: Static "Swell Out", description: Just "Time to fade to silence"
              , annotations: [ ann 0 "Fast", ann 127 "Slow" ] }
            , Segmented { cc: cc 31, label: "Envelope", options:
                [ { label: "Analog", value: mv 0, description: Just "Classic envelope follower" }
                , { label: "Combo", value: mv 2, description: Just "Analog attack, adaptive release" }
                , { label: "Adaptive", value: mv 3, description: Just "Dynamically adjusts to playing" }
                ] }
            , Segmented { cc: cc 32, label: "Shifty Sub", options:
                [ { label: "ASR", value: mv 0, description: Just "Attack-sustain-release shifting" }
                , { label: "ENV", value: mv 127, description: Just "Envelope-following shifting" }
                ] }
            , Segmented { cc: cc 33, label: "Spread Route", options:
                [ { label: "EQ", value: mv 0, description: Just "Stereo from EQ only" }
                , { label: "Both", value: mv 2, description: Nothing }
                , { label: "Vol/Comp", value: mv 3, description: Just "Stereo from dynamics" }
                ] }
            ]
        }
      , { name: "DIP Switches", compact: true, collapsed: true, layout: DipGrid
        , description: Just "Physical DIP switches \x2014 left bank (ramping), right bank (customize)"
        , controls:
            [ Toggle { cc: cc 61, label: "Dynamics", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 62, label: "Attack", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 63, label: "EQ", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 64, label: "Dry", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 65, label: "Wet", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 66, label: "Bounce", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 67, label: "Sweep", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 68, label: "Polarity", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 71, label: "Miso", onLabel: "On", offLabel: "Off", description: Just "Mono in, stereo out", labelSource: Nothing }
            , Toggle { cc: cc 72, label: "Spread", onLabel: "On", offLabel: "Off", description: Just "Independent L/R dynamics", labelSource: Nothing }
            , Toggle { cc: cc 73, label: "Latch", onLabel: "On", offLabel: "Off", description: Just "Hold becomes latch", labelSource: Nothing }
            , Toggle { cc: cc 74, label: "Sidechain", onLabel: "On", offLabel: "Off", description: Just "External signal triggers compression", labelSource: Nothing }
            , Toggle { cc: cc 75, label: "Noise Gate", onLabel: "On", offLabel: "Off", description: Just "Mute input when not playing", labelSource: Nothing }
            , Toggle { cc: cc 76, label: "Motion", onLabel: "On", offLabel: "Off", description: Just "Modulate compression amount", labelSource: Nothing }
            , Toggle { cc: cc 77, label: "Swell Aux", onLabel: "On", offLabel: "Off", description: Just "Manual swell per press", labelSource: Nothing }
            , Toggle { cc: cc 78, label: "Dusty", onLabel: "On", offLabel: "Off", description: Just "Clipping/overdrive circuit", labelSource: Nothing }
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
            [ Slider { cc: cc 100, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Min", ann 127 "Max" ] }
            ]
        }
      ]
  }
