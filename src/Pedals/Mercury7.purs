module Pedals.Mercury7 (pedal) where

import Color (fromHexString)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeCC, unsafeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), PedalDef, PedalId(..), RangeOption, SectionLayout(..))
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
      { id: PedalId "mercury7"
      , name: "Mercury7"
      , shortName: "M7"
      , brand: "Meris"
      , color: fromHexString "#2a5a8a"
      , defaultChannel: 12
      , saveInstructions: Just "Hold ALT and tap the preset footswitch."
      }
  , engage: SingleEngage (cc 14)
  , baseline: Map.fromFoldable
      [ cc 4 /\ mv 0, cc 14 /\ mv 0
      , cc 16 /\ mv 80, cc 17 /\ mv 12, cc 18 /\ mv 77
      , cc 19 /\ mv 127, cc 20 /\ mv 0, cc 21 /\ mv 127
      , cc 22 /\ mv 0, cc 23 /\ mv 21, cc 24 /\ mv 88
      , cc 25 /\ mv 127, cc 26 /\ mv 63, cc 27 /\ mv 0
      , cc 29 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { hue: 80
      , encoders:
          [ Just (TwisterCC { cc: cc 16, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 17, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 18, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 20, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 19, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 21, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 24, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 29, center: Nothing, options: Just [ mv 0, mv 127 ] })
          , Just (TwisterCC { cc: cc 22, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 23, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 25, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 26, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 27, center: Nothing, options: Nothing })
          , Nothing, Nothing, Nothing
          ]
      , buttons:
          [ Just (TwisterToggle { cc: cc 14 })
          , Nothing, Nothing
          , Just (TwisterToggle { cc: cc 28 })
          , Nothing, Nothing, Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          ]
      }
  , modes: Nothing
  , layout: Nothing
  , sections:
      [ { name: "Engage", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 14, label: "Bypass", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 28, label: "Swell", onLabel: "On", offLabel: "Off"
              , description: Just "Auto swell \x2014 hold ALT to max decay sustain", labelSource: Nothing }
            , RangeSelect { cc: cc 29, label: "Algorithm", ranges:
                [ rng 0 63 "Ultraplate"
                , rng 64 127 "Cathedra"
                ] }
            ]
        }
      , { name: "Reverb", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 16, label: Static "Space Decay", description: Just "Decay energy of the reverb space"
              , annotations: [ ann 0 "Short", ann 64 "Mid", ann 127 "Infinite" ] }
            , Slider { cc: cc 17, label: Static "Modulate", description: Just "Overall modulation depth"
              , annotations: [ ann 0 "None", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 18, label: Static "Mix", description: Just "Dry/wet balance"
              , annotations: [ ann 0 "Dry", ann 64 "Even", ann 127 "Wet" ] }
            ]
        }
      , { name: "Tone", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 19, label: Static "Lo Frequency", description: Just "Low frequency behavior"
              , annotations: [ ann 0 "Attenuated", ann 64 "Mid", ann 127 "Extended" ] }
            , Slider { cc: cc 21, label: Static "Hi Frequency", description: Just "High frequency absorption"
              , annotations: [ ann 0 "Attenuated", ann 64 "Mid", ann 127 "Extended" ] }
            ]
        }
      , { name: "Pitch", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 20, label: Static "Pitch Vector", description: Just "Intra-tank pitch interval"
              , annotations: [ ann 0 "-Oct", ann 32 "Down", ann 64 "Off", ann 96 "Up", ann 127 "+Oct" ] }
            , Slider { cc: cc 24, label: Static "Pitch Vector Mix", description: Just "Pitch-shifted vs normal reflections"
              , annotations: [ ann 0 "Pitch", ann 64 "Mid", ann 127 "Normal" ] }
            ]
        }
      , { name: "Alt Functions", compact: true, collapsed: true, layout: DefaultLayout
        , description: Just "Hold ALT to access these on the pedal"
        , controls:
            [ Slider { cc: cc 22, label: Static "Predelay", description: Just "Time before reverb onset"
              , annotations: [ ann 0 "None", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 23, label: Static "Mod Speed", description: Just "Dominant modulation speed"
              , annotations: [ ann 0 "Slow", ann 64 "Mid", ann 127 "Fast" ] }
            , Slider { cc: cc 25, label: Static "Density", description: Just "Initial echo density"
              , annotations: [ ann 0 "Sparse", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 26, label: Static "Attack Time", description: Just "Swell envelope attack"
              , annotations: [ ann 0 "Fast", ann 64 "~600ms", ann 127 "Slow" ] }
            , Slider { cc: cc 27, label: Static "Vibrato Depth", description: Just "Vibrato added to reverb input"
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Max" ] }
            ]
        }
      , { name: "Other", compact: false, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 4, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Heel", ann 127 "Toe" ] }
            ]
        }
      ]
  }
