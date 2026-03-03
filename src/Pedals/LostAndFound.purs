module Pedals.LostAndFound (pedal) where

import Color (fromHexString)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeCC, unsafeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), PedalDef, PedalId(..), SectionLayout(..))
import Data.Pedal.Engage (EngageConfig(..))
import Data.Pedal.Modes (DualChannelModes, ModeChannel(..), ModeRole(..))
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
      { id: PedalId "lostandfound"
      , name: "Lost + Found"
      , shortName: "LF"
      , brand: "Chase Bliss"
      , color: fromHexString "#5a8a3c"
      , defaultChannel: 6
      , saveInstructions: Just "Hold both footswitches until LEDs blink, then release."
      }
  , engage: DualEngage
      { a: { cc: cc 102, label: "Right" }
      , b: { cc: cc 103, label: "Left" }
      }
  , baseline: Map.fromFoldable
      [ cc 14 /\ mv 64, cc 15 /\ mv 64, cc 16 /\ mv 64
      , cc 17 /\ mv 64, cc 18 /\ mv 64, cc 19 /\ mv 64
      , cc 20 /\ mv 64
      , cc 21 /\ mv 0, cc 22 /\ mv 2, cc 23 /\ mv 0
      , cc 24 /\ mv 64, cc 25 /\ mv 0, cc 26 /\ mv 64
      , cc 27 /\ mv 64, cc 28 /\ mv 0, cc 29 /\ mv 64, cc 30 /\ mv 100
      , cc 32 /\ mv 2
      , cc 51 /\ mv 0, cc 52 /\ mv 0, cc 53 /\ mv 6, cc 54 /\ mv 6
      , cc 57 /\ mv 0
      , cc 61 /\ mv 0, cc 62 /\ mv 0, cc 63 /\ mv 0, cc 64 /\ mv 0, cc 65 /\ mv 0
      , cc 66 /\ mv 0, cc 67 /\ mv 0, cc 68 /\ mv 0
      , cc 71 /\ mv 0, cc 72 /\ mv 0, cc 73 /\ mv 0, cc 74 /\ mv 0
      , cc 75 /\ mv 0, cc 76 /\ mv 0, cc 77 /\ mv 0, cc 78 /\ mv 0
      , cc 100 /\ mv 0
      , cc 102 /\ mv 0, cc 103 /\ mv 0
      , cc 105 /\ mv 0, cc 106 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just
      { hue: 40
      , encoders:
          [ Just (TwisterCC { cc: cc 14, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 15, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 16, center: Nothing, options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 17, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 18, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 19, center: Just (mv 64), options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 24, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 25, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 26, center: Nothing, options: Nothing })
          , Nothing
          , Just (TwisterCC { cc: cc 27, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 28, center: Nothing, options: Nothing })
          , Just (TwisterCC { cc: cc 29, center: Just (mv 64), options: Nothing })
          , Just (TwisterCC { cc: cc 30, center: Nothing, options: Nothing })
          ]
      , buttons:
          [ Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 103 })
          , Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 102 })
          , Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 105 })
          , Nothing, Nothing, Nothing
          , Just (TwisterToggle { cc: cc 106 })
          ]
      }
  , modes: Just modes
  , layout: Nothing
  , sections:
      [ { name: "Channels", compact: true, collapsed: false, layout: DualColumn, description: Nothing
        , controls:
            -- Left footswitch
            [ Toggle { cc: cc 103, label: "Left", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 105, label: "L Hold", onLabel: "On", offLabel: "Off", description: Nothing
              , labelSource: Just (ChannelMode { channel: LeftChannel, role: HoldRole }) }
            -- Right footswitch
            , Toggle { cc: cc 102, label: "Right", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 106, label: "R Hold", onLabel: "On", offLabel: "Off", description: Nothing
              , labelSource: Just (ChannelMode { channel: RightChannel, role: HoldRole }) }
            ]
        }
      , { name: "Mix & Routing", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 15, label: Static "Mix", description: Just "Wet/dry balance"
              , annotations: [ ann 0 "Dry", ann 64 "Even", ann 127 "Wet" ] }
            , Slider { cc: cc 18, label: Static "Blend", description: Just "Balance between L and R channels"
              , annotations: [ ann 0 "Left", ann 64 "Even", ann 127 "Right" ] }
            , Segmented { cc: cc 22, label: "Routing", options:
                [ { label: "L\x2009>\x2009R", value: mv 0, description: Just "Series: Left into Right" }
                , { label: "L\x2009+\x2009R", value: mv 2, description: Just "Parallel" }
                , { label: "L\x2009<\x2009R", value: mv 3, description: Just "Series: Right into Left" }
                ] }
            ]
        }
      , { name: "Left Channel", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ ModeRadio { modeChannel: LeftChannel, label: "Left Effect" }
            , Slider { cc: cc 14, label: ChannelMode { channel: LeftChannel, role: TimeRole }
              , description: Nothing
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 17, label: ChannelMode { channel: LeftChannel, role: ModifyRole }
              , description: Nothing
              , annotations: [ ann 0 "A", ann 64 "Neutral", ann 127 "B" ] }
            , Slider { cc: cc 24, label: ChannelMode { channel: LeftChannel, role: AltRole }
              , description: Nothing
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 27, label: Static "L EQ", description: Just "Left channel tone"
              , annotations: [ ann 0 "Dark", ann 64 "Flat", ann 127 "Thin" ] }
            ]
        }
      , { name: "Right Channel", compact: true, collapsed: false, layout: DefaultLayout, description: Nothing
        , controls:
            [ ModeRadio { modeChannel: RightChannel, label: "Right Effect" }
            , Slider { cc: cc 16, label: ChannelMode { channel: RightChannel, role: TimeRole }
              , description: Nothing
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 19, label: ChannelMode { channel: RightChannel, role: ModifyRole }
              , description: Nothing
              , annotations: [ ann 0 "A", ann 64 "Neutral", ann 127 "B" ] }
            , Slider { cc: cc 26, label: ChannelMode { channel: RightChannel, role: AltRole }
              , description: Nothing
              , annotations: [ ann 0 "Min", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 29, label: Static "R EQ", description: Just "Right channel tone"
              , annotations: [ ann 0 "Dark", ann 64 "Flat", ann 127 "Thin" ] }
            ]
        }
      , { name: "Hidden Options", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Slider { cc: cc 28, label: Static "Glue", description: Just "Compressor / saturator"
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Max" ] }
            , Slider { cc: cc 25, label: Static "Spill", description: Just "In series routing, apply second effect to dry signal too"
              , annotations: [ ann 0 "None", ann 127 "Full" ] }
            , Slider { cc: cc 30, label: Static "Master Wet Vol", description: Nothing
              , annotations: [ ann 0 "Off", ann 64 "Mid", ann 127 "Max" ] }
            , Segmented { cc: cc 32, label: "Spread Routing", options:
                [ { label: "L Only", value: mv 0, description: Nothing }
                , { label: "Both", value: mv 2, description: Nothing }
                , { label: "R Only", value: mv 3, description: Nothing }
                ] }
            ]
        }
      , { name: "DIP Switches", compact: true, collapsed: true, layout: DipGrid
        , description: Just "Physical DIP switches \x2014 left bank (ramping), right bank (customize)"
        , controls:
            [ Toggle { cc: cc 61, label: "L Time", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 62, label: "L Modify", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 63, label: "Blend", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 64, label: "R Modify", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 65, label: "R Time", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 66, label: "Bounce", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 67, label: "Sweep", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 68, label: "Polarity", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Toggle { cc: cc 71, label: "Miso", onLabel: "On", offLabel: "Off", description: Just "Mono in, stereo out", labelSource: Nothing }
            , Toggle { cc: cc 72, label: "Spread", onLabel: "On", offLabel: "Off", description: Just "Stereo processing (each mode has unique stereo image)", labelSource: Nothing }
            , Toggle { cc: cc 73, label: "Latch", onLabel: "On", offLabel: "Off", description: Just "Hold becomes latch instead of momentary", labelSource: Nothing }
            , Toggle { cc: cc 74, label: "L Swap", onLabel: "On", offLabel: "Off", description: Just "Swap left channel effects with right", labelSource: Nothing }
            , Toggle { cc: cc 75, label: "R Swap", onLabel: "On", offLabel: "Off", description: Just "Swap right channel effects with left", labelSource: Nothing }
            , Toggle { cc: cc 76, label: "Unsync", onLabel: "On", offLabel: "Off", description: Just "Disconnect channel tempos for free timing", labelSource: Nothing }
            , Toggle { cc: cc 77, label: "Trails", onLabel: "On", offLabel: "Off", description: Just "Effects fade out naturally after bypass", labelSource: Nothing }
            , Toggle { cc: cc 78, label: "Bank", onLabel: "On", offLabel: "Off", description: Just "Switch to alternate preset bank (3 & 4)", labelSource: Nothing }
            ]
        }
      , { name: "Ramping", compact: true, collapsed: true, layout: DefaultLayout
        , description: Just "Automate knob movement (bounce or one-shot ramp)"
        , controls:
            [ Toggle { cc: cc 52, label: "Ramp/Bounce Active", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
            , Slider { cc: cc 20, label: Static "Ramp Speed", description: Nothing
              , annotations: [ ann 0 "Slow", ann 127 "Fast" ] }
            ]
        }
      , { name: "Other", compact: true, collapsed: true, layout: DefaultLayout, description: Nothing
        , controls:
            [ Toggle { cc: cc 57, label: "Dry Kill", onLabel: "On", offLabel: "Off", description: Just "Remove dry signal from output", labelSource: Nothing }
            , Toggle { cc: cc 51, label: "MIDI Clock Follow", onLabel: "On", offLabel: "Off", description: Just "Sync timing to external clock", labelSource: Nothing }
            , Dropdown { cc: cc 53, label: "L Subdivision", description: Nothing, options:
                [ { label: "32nd", value: mv 0, description: Nothing }
                , { label: "16th", value: mv 1, description: Nothing }
                , { label: "Triplet 16th", value: mv 2, description: Nothing }
                , { label: "8th", value: mv 3, description: Nothing }
                , { label: "Triplet 8th", value: mv 4, description: Nothing }
                , { label: "Dotted 8th", value: mv 5, description: Nothing }
                , { label: "Quarter", value: mv 6, description: Nothing }
                , { label: "Triplet Qtr", value: mv 7, description: Nothing }
                , { label: "Dotted Qtr", value: mv 8, description: Nothing }
                , { label: "Half", value: mv 9, description: Nothing }
                , { label: "Dotted Half", value: mv 10, description: Nothing }
                , { label: "Whole", value: mv 11, description: Nothing }
                , { label: "Double Whole", value: mv 12, description: Nothing }
                ] }
            , Dropdown { cc: cc 54, label: "R Subdivision", description: Nothing, options:
                [ { label: "32nd", value: mv 0, description: Nothing }
                , { label: "16th", value: mv 1, description: Nothing }
                , { label: "Triplet 16th", value: mv 2, description: Nothing }
                , { label: "8th", value: mv 3, description: Nothing }
                , { label: "Triplet 8th", value: mv 4, description: Nothing }
                , { label: "Dotted 8th", value: mv 5, description: Nothing }
                , { label: "Quarter", value: mv 6, description: Nothing }
                , { label: "Triplet Qtr", value: mv 7, description: Nothing }
                , { label: "Dotted Qtr", value: mv 8, description: Nothing }
                , { label: "Half", value: mv 9, description: Nothing }
                , { label: "Dotted Half", value: mv 10, description: Nothing }
                , { label: "Whole", value: mv 11, description: Nothing }
                , { label: "Double Whole", value: mv 12, description: Nothing }
                ] }
            , Momentary { cc: cc 112, label: "Random L Mode", value: mv 127, description: Nothing }
            , Momentary { cc: cc 113, label: "Random R Mode", value: mv 127, description: Nothing }
            , Momentary { cc: cc 114, label: "Random Both", value: mv 127, description: Nothing }
            , Slider { cc: cc 100, label: Static "Expression", description: Nothing
              , annotations: [ ann 0 "Min", ann 127 "Max" ] }
            , Momentary { cc: cc 93, label: "Tap Tempo", value: mv 127, description: Nothing }
            ]
        }
      ]
  }

modes :: DualChannelModes
modes =
  { effects:
      [ { id: "reverb", label: "Reverb"
        , a: { name: "Slow-verb", time: "Size", modify: "Decay", alt: "Diffusion" }
        , b: { name: "Useful Ambience", time: "Size", modify: "Decay", alt: "Diffusion" }
        , hold: "Freeze" }
      , { id: "pitch", label: "Pitch"
        , a: { name: "Orchestral Swell", time: "Swell Time", modify: "Pitch", alt: "Feedback" }
        , b: { name: "Pitch Repeater", time: "Delay", modify: "Pitch", alt: "Repeats" }
        , hold: "Freeze" }
      , { id: "warp", label: "Warp"
        , a: { name: "Pinging Phaser", time: "Rate", modify: "Poles", alt: "Resonance" }
        , b: { name: "Spectral Modulator", time: "Rate", modify: "Depth", alt: "Resonance" }
        , hold: "Pause" }
      , { id: "delay", label: "Delay"
        , a: { name: "Tape Echo", time: "Delay Time", modify: "Feedback", alt: "Stability" }
        , b: { name: "Grain Tumbler", time: "Grain Size", modify: "Feedback", alt: "Grain Shape" }
        , hold: "Infinite" }
      , { id: "synth", label: "Synth"
        , a: { name: "Impulse Synth", time: "Portamento", modify: "Texture", alt: "Atk / Rel" }
        , b: { name: "Sympathetic Resonator", time: "Onset", modify: "Feedback", alt: "Octave" }
        , hold: "Infinite / Freeze" }
      , { id: "bend", label: "Bend"
        , a: { name: "Ensemble Expander", time: "Rate", modify: "Depth", alt: "Voices" }
        , b: { name: "Gen Lite", time: "Rate", modify: "Depth", alt: "Crinkles" }
        , hold: "Random / Tape Stop" }
      ]
  , left: { modeCC: cc 21, swapCC: cc 74, native: [ "reverb", "pitch", "warp" ] }
  , right: { modeCC: cc 23, swapCC: cc 75, native: [ "delay", "synth", "bend" ] }
  }
