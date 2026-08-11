module Pedals.Flint (pedal) where

import Color (fromHexString)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeCC, unsafeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), PedalDef, PedalId(..), SectionLayout(..))
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

infixr 6 Tuple as /\

pedal :: PedalDef
pedal =
  { meta:
      { id: PedalId "flint"
      , name: "Flint"
      , shortName: "Fl"
      , brand: "Strymon"
      , color: fromHexString "#a85a50"
      , defaultChannel: 7
      , saveInstructions: Just "Hold the footswitch for 3 seconds until LED flashes."
      }
  , engage: DualEngage
      { a: { cc: cc 10, label: "Trem" }
      , b: { cc: cc 16, label: "Verb" }
      }
  , baseline: Map.fromFoldable
      [ cc 0 /\ mv 0, cc 10 /\ mv 127, cc 11 /\ mv 1, cc 12 /\ mv 64
      , cc 13 /\ mv 64, cc 14 /\ mv 64, cc 15 /\ mv 64
      , cc 16 /\ mv 127, cc 17 /\ mv 1, cc 18 /\ mv 64, cc 19 /\ mv 64
      , cc 20 /\ mv 64, cc 21 /\ mv 0, cc 22 /\ mv 64
      , cc 23 /\ mv 0, cc 25 /\ mv 3
      , cc 60 /\ mv 0, cc 63 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { hue: 127
      , encoders:
          [ Just (TwisterCC { cc: cc 11, center: Nothing, options: Just [ mv 1, mv 2, mv 3 ] })
          , Just (TwisterCC { cc: cc 12, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 13, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 14, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 17, center: Nothing, options: Just [ mv 1, mv 2, mv 3 ] })
          , Just (TwisterCC { cc: cc 18, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 19, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 20, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 15, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 21, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 22, center: Just (mv 64), options: Nothing })
          , Nothing
          , Nothing, Nothing, Nothing, Nothing
          ]
      , buttons:
          [ Just (TwisterToggle { cc: cc 23 })
          , Nothing, Nothing, Nothing
          , Nothing, Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 10 })
          , Just (TwisterToggle { cc: cc 16 })
          , Just (TwisterToggle { cc: cc 33 })
          , Just (TwisterMomentary { cc: cc 93 })
          , Nothing, Nothing, Nothing, Nothing
          ]
      }
  , modes: Nothing
  , layout: Just flintLayout
  , sections:
      [ { name: "Channels", compact: true, collapsed: false, layout: DualColumn, description: Nothing
        , controls:
            -- Left footswitch (Tremolo)
            [ Toggle { cc: cc 10, label: "Tremolo", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 33, label: "Both", onLabel: "On", offLabel: "Off", description: Just "Master bypass for both engines", labelSource: Nothing }
            -- Right footswitch (Reverb)
            , Toggle { cc: cc 16, label: "Reverb", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 23, label: "Effect Order", onLabel: "Trm > Rvb", offLabel: "Rvb > Trm", description: Nothing, labelSource: Nothing }
            ]
        }
      , { name: "Tremolo", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 11, label: "Type", options:
                [ { label: "'61 Harm", value: mv 1, description: Just "Harmonic tremolo" }
                , { label: "'63 Tube", value: mv 2, description: Just "Tube bias tremolo" }
                , { label: "'65 Photo", value: mv 3, description: Just "Photo-cell tremolo" }
                ] }
            , Slider { cc: cc 12, label: Static "Intensity", description: Just "Tremolo depth"
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 13, label: Static "Speed", description: Just "LFO rate"
              , annotations: [ ann 0 "Slow", ann 64 "Mid", ann 127 "Fast" ] }
            , Slider { cc: cc 14, label: Static "Tap Subdivision", description: Nothing
              , annotations: [ ann 0 "1/16", ann 42 "Triplet", ann 84 "1/8", ann 127 "1/4" ] }
            , Slider { cc: cc 15, label: Static "Boost/Cut", description: Just "Volume when tremolo engaged"
              , annotations: [ ann 0 "-3dB", ann 64 "Unity", ann 127 "+3dB" ] }
            ]
        }
      , { name: "Reverb", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Segmented { cc: cc 17, label: "Type", options:
                [ { label: "'60s Spring", value: mv 1, description: Just "Spring tank" }
                , { label: "'70s Plate", value: mv 2, description: Just "Electronic plate" }
                , { label: "'80s Hall", value: mv 3, description: Just "Digital hall" }
                ] }
            , Slider { cc: cc 18, label: Static "Mix", description: Just "Reverb wet/dry"
              , annotations: [ ann 0 "Dry", ann 64 "50/50", ann 127 "Wet" ] }
            , Slider { cc: cc 19, label: Static "Color", description: Just "Reverb tone"
              , annotations: [ ann 0 "Dark", ann 64 "Mid", ann 127 "Bright" ] }
            , Slider { cc: cc 20, label: Static "Decay", description: Just "Reverb tail length"
              , annotations: [ ann 0 "Short", ann 64 "Mid", ann 127 "Infinite" ] }
            , Slider { cc: cc 21, label: Static "Pre-Delay", description: Nothing
              , annotations: [ ann 0 "None", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 22, label: Static "Boost/Cut", description: Just "Volume when reverb engaged"
              , annotations: [ ann 0 "-3dB", ann 64 "Unity", ann 127 "+3dB" ] }
            ]
        }
      , { name: "Settings", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 100, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Heel", ann 127 "Toe" ] }
            , Segmented { cc: cc 0, label: "Bank Select", options:
                [ { label: "Bank 1", value: mv 0, description: Nothing }
                , { label: "Bank 2", value: mv 1, description: Nothing }
                , { label: "Bank 3", value: mv 2, description: Nothing }
                ] }
            , Dropdown { cc: cc 25, label: "Clock Division", description: Nothing, options:
                [ { label: "x1/4", value: mv 0, description: Nothing }
                , { label: "x1/3", value: mv 1, description: Nothing }
                , { label: "x1/2", value: mv 2, description: Nothing }
                , { label: "x1", value: mv 3, description: Nothing }
                , { label: "x2", value: mv 4, description: Nothing }
                , { label: "x3", value: mv 5, description: Nothing }
                , { label: "x4", value: mv 6, description: Nothing }
                ] }
            , Toggle { cc: cc 63, label: "MIDI Clock", onLabel: "On", offLabel: "Off", description: Just "Sync to incoming clock", labelSource: Nothing }
            , Toggle { cc: cc 60, label: "MIDI Expression", onLabel: "On", offLabel: "Off", description: Just "Respond to CC#100", labelSource: Nothing }
            , Momentary { cc: cc 93, label: "Tap Tempo", value: mv 127, description: Nothing }
            , Momentary { cc: cc 27, label: "Footswitch A", value: mv 127, description: Nothing }
            , Momentary { cc: cc 28, label: "Footswitch B", value: mv 127, description: Nothing }
            ]
        }
      ]
  }

-- | Donut view layout. Tremolo (left) + reverb (right) dual-section.
-- | 5 knobs in 3×2 grid (one empty cell). Toggles (type selectors) in config.
flintLayout :: PedalLayout
flintLayout =
  { groups:
      [ { id: "trem", label: "Tremolo", color: "#a85a50", mutedColor: "#d8b8b0" }
      , { id: "verb", label: "Reverb", color: "#6a5a50", mutedColor: "#c8c0b8" }
      ]
  , knobs:
      -- Row A (main controls)
      [ { col: 0, row: 0, group: "trem"
        , primaryCC: cc 12, primaryLabel: Static "Intensity"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      , { col: 1, row: 0, group: "verb"
        , primaryCC: cc 20, primaryLabel: Static "Decay"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      , { col: 2, row: 0, group: "verb"
        , primaryCC: cc 18, primaryLabel: Static "Mix"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      -- Row B (secondary)
      , { col: 0, row: 1, group: "trem"
        , primaryCC: cc 13, primaryLabel: Static "Speed"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      , { col: 2, row: 1, group: "verb"
        , primaryCC: cc 19, primaryLabel: Static "Color"
        , primaryLayer: ContinuousKnob { center: Nothing }
        , hiddenCC: Nothing, hiddenLabel: Nothing
        , hiddenLayer: Nothing }
      ]
  , footswitches:
      [ { col: 0, cc: cc 10, label: "Trem", group: "trem"
        , ledCC: Nothing, engagedColor: "#a85a50", ledColor: "#a85a50" }
      , { col: 2, cc: cc 16, label: "Verb", group: "verb"
        , ledCC: Nothing, engagedColor: "#6a5a50", ledColor: "#6a5a50" }
      ]
  , dipBanks: []
  , config:
      [ { cc: cc 11, label: "Trem",   controlType: CfgToggle }
      , { cc: cc 17, label: "Verb",   controlType: CfgToggle }
      , { cc: cc 23, label: "Order",  controlType: CfgToggle }
      , { cc: cc 63, label: "Clock",  controlType: CfgToggle }
      ]
  , columns: 3
  , knobRows: 2
  , viewBox: { width: 320.0, height: 370.0 }
  }
