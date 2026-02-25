module Pedals.Onward (pedal) where

import Color (fromHexString)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeCC, unsafeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), PedalDef, PedalId(..), Section, SectionLayout(..))
import Data.Pedal.Engage (EngageConfig(..))
import Data.Tuple (Tuple(..))
import Data.Twister (TwisterButton(..), TwisterEncoder(..), TwisterMapping)

cc :: Int -> CC
cc = unsafeCC

mv :: Int -> MidiValue
mv = unsafeMidiValue

ann :: Int -> String -> Annotation
ann pos label = { position: mv pos, label }

pedal :: PedalDef
pedal =
  { meta:
      { id: PedalId "onward"
      , name: "Onward"
      , brand: "Chase Bliss"
      , color: fromHexString "#c8a96e"
      , defaultChannel: 2
      , saveInstructions: Just "Hold both footswitches until LEDs blink, then release."
      }
  , engage: DualEngage
      { a: { cc: cc 102, label: "Freeze" }
      , b: { cc: cc 103, label: "Glitch" }
      }
  , baseline: Map.fromFoldable
      [ cc 14 /\ mv 64, cc 15 /\ mv 64, cc 16 /\ mv 64, cc 17 /\ mv 0
      , cc 18 /\ mv 64, cc 19 /\ mv 64, cc 20 /\ mv 64
      , cc 21 /\ mv 2, cc 22 /\ mv 0, cc 23 /\ mv 2
      , cc 24 /\ mv 64, cc 25 /\ mv 64, cc 26 /\ mv 0, cc 27 /\ mv 0
      , cc 28 /\ mv 64, cc 29 /\ mv 64
      , cc 31 /\ mv 0, cc 32 /\ mv 0, cc 33 /\ mv 0
      , cc 51 /\ mv 0, cc 52 /\ mv 0, cc 53 /\ mv 6
      , cc 57 /\ mv 0, cc 58 /\ mv 127
      , cc 61 /\ mv 0, cc 62 /\ mv 0, cc 63 /\ mv 0, cc 64 /\ mv 0, cc 65 /\ mv 0
      , cc 66 /\ mv 0, cc 67 /\ mv 0, cc 68 /\ mv 0
      , cc 71 /\ mv 0, cc 72 /\ mv 0, cc 73 /\ mv 0, cc 74 /\ mv 0
      , cc 75 /\ mv 0, cc 76 /\ mv 0, cc 77 /\ mv 0, cc 78 /\ mv 0
      , cc 100 /\ mv 0
      , cc 102 /\ mv 0, cc 103 /\ mv 0
      , cc 105 /\ mv 0, cc 106 /\ mv 0
      ]
  , resetOrder: []
  , twister: Just twisterMapping
  , modes: Nothing
  , sections:
      [ channelsSection
      , mainSection
      , shapeSection
      , routingSection
      , hiddenSection
      , dipSection
      , rampingSection
      , otherSection
      ]
  }

infixr 6 Tuple as /\

twisterMapping :: TwisterMapping
twisterMapping =
  { encoders:
      [ Just (TwisterCC { cc: cc 14, center: Nothing, options: Nothing })
      , Just (TwisterCC { cc: cc 15, center: Just (mv 64), options: Nothing })
      , Just (TwisterCC { cc: cc 16, center: Just (mv 64), options: Nothing })
      , Nothing
      , Just (TwisterCC { cc: cc 17, center: Nothing, options: Nothing })
      , Just (TwisterCC { cc: cc 18, center: Nothing, options: Nothing })
      , Just (TwisterCC { cc: cc 19, center: Just (mv 64), options: Nothing })
      , Nothing
      , Just (TwisterCC { cc: cc 24, center: Nothing, options: Nothing })
      , Just (TwisterCC { cc: cc 25, center: Just (mv 64), options: Nothing })
      , Just (TwisterCC { cc: cc 26, center: Nothing, options: Nothing })
      , Nothing
      , Just (TwisterCC { cc: cc 27, center: Nothing, options: Nothing })
      , Just (TwisterCC { cc: cc 28, center: Nothing, options: Nothing })
      , Just (TwisterCC { cc: cc 29, center: Just (mv 64), options: Nothing })
      , Nothing
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

channelsSection :: Section
channelsSection =
  { name: "Channels"
  , compact: true
  , collapsed: false
  , layout: DualColumn
  , description: Nothing
  , controls:
      -- Left footswitch (Glitch)
      [ Toggle { cc: cc 103, label: "Glitch", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 105, label: "Glitch Hold", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Momentary { cc: cc 108, label: "Retrigger Glitch", value: mv 127, description: Nothing }
      -- Right footswitch (Freeze)
      , Toggle { cc: cc 102, label: "Freeze", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 106, label: "Freeze Hold", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Momentary { cc: cc 109, label: "Retrigger Freeze", value: mv 127, description: Nothing }
      ]
  }

mainSection :: Section
mainSection =
  { name: "Main Controls"
  , compact: true
  , collapsed: false
  , layout: DefaultLayout
  , description: Nothing
  , controls:
      [ Slider { cc: cc 14, label: Static "Size", description: Just "Glitch length & overall timing"
        , annotations: [ ann 0 "Tiny", ann 64 "Medium", ann 127 "Max" ] }
      , Slider { cc: cc 15, label: Static "Mix", description: Just "Wet/dry balance (ramp speed when ramping)"
        , annotations: [ ann 0 "Dry", ann 64 "Even", ann 127 "Wet" ] }
      , Slider { cc: cc 16, label: Static "Octave", description: Just "Blends pitched voice"
        , annotations: [ ann 0 "Oct Down", ann 64 "Off", ann 127 "Oct Up" ] }
      , Slider { cc: cc 17, label: Static "Error", description: Just "Likelihood & intensity of randomization"
        , annotations: [ ann 0 "None", ann 127 "Max" ] }
      , Slider { cc: cc 18, label: Static "Sustain", description: Just "Hold time"
        , annotations: [ ann 0 "Short", ann 64 "Medium", ann 127 "Infinite" ] }
      , Slider { cc: cc 19, label: Static "Texture", description: Just "Digital/analog character"
        , annotations: [ ann 0 "Sample rate", ann 64 "Off", ann 127 "Soft-clip" ] }
      ]
  }

shapeSection :: Section
shapeSection =
  { name: "Shape & Error"
  , compact: true
  , collapsed: false
  , layout: DefaultLayout
  , description: Nothing
  , controls:
      [ Segmented { cc: cc 21, label: "Error Type", options:
          [ { label: "Timing", value: mv 0, description: Just "Manipulates sample length" }
          , { label: "Condition", value: mv 2, description: Just "Dropouts & sample rate shifts" }
          , { label: "Playback", value: mv 3, description: Just "Speed & direction changes" }
          ] }
      , Segmented { cc: cc 22, label: "Fade", options:
          [ { label: "Long", value: mv 0, description: Just "Soft, swell-y" }
          , { label: "User", value: mv 2, description: Just "Custom (set in Hidden Options)" }
          , { label: "Short", value: mv 3, description: Just "Immediate, responsive" }
          ] }
      , Segmented { cc: cc 23, label: "Animate", options:
          [ { label: "Vibrato", value: mv 0, description: Just "Speed set by Size knob" }
          , { label: "Off", value: mv 2, description: Nothing }
          , { label: "Chorus", value: mv 3, description: Just "Slow, fixed atmospheric speed" }
          ] }
      ]
  }

routingSection :: Section
routingSection =
  { name: "Routing"
  , compact: true
  , collapsed: false
  , layout: DefaultLayout
  , description: Just "Route each section to Glitch, Both, or Freeze channel"
  , controls:
      [ Segmented { cc: cc 31, label: "Error Routing", options:
          [ { label: "Glitch", value: mv 0, description: Nothing }
          , { label: "Both", value: mv 2, description: Nothing }
          , { label: "Freeze", value: mv 3, description: Nothing }
          ] }
      , Segmented { cc: cc 32, label: "Sustain Routing", options:
          [ { label: "Glitch", value: mv 0, description: Nothing }
          , { label: "Both", value: mv 2, description: Nothing }
          , { label: "Freeze", value: mv 3, description: Nothing }
          ] }
      , Segmented { cc: cc 33, label: "Effects Routing", options:
          [ { label: "Glitch", value: mv 0, description: Nothing }
          , { label: "Both", value: mv 2, description: Nothing }
          , { label: "Freeze", value: mv 3, description: Nothing }
          ] }
      ]
  }

hiddenSection :: Section
hiddenSection =
  { name: "Hidden Options"
  , compact: true
  , collapsed: true
  , layout: DefaultLayout
  , description: Nothing
  , controls:
      [ Slider { cc: cc 24, label: Static "Sensitivity", description: Just "How easily playing triggers resampling"
        , annotations: [ ann 0 "Less", ann 64 "Default", ann 127 "More" ] }
      , Slider { cc: cc 25, label: Static "Balance", description: Just "Relative loudness of Glitch vs Freeze"
        , annotations: [ ann 0 "Glitch", ann 64 "Even", ann 127 "Freeze" ] }
      , Slider { cc: cc 26, label: Static "Duck Depth", description: Just "Ducking/sidechain intensity"
        , annotations: [ ann 0 "Less", ann 127 "More" ] }
      , Slider { cc: cc 27, label: Static "Error Blend", description: Just "Mix in unselected error types"
        , annotations: [ ann 0 "None", ann 127 "All three" ] }
      , Slider { cc: cc 28, label: Static "User Fade", description: Just "Custom fade time for User position"
        , annotations: [ ann 0 "Slower", ann 127 "Faster" ] }
      , Slider { cc: cc 29, label: Static "Filter", description: Just "EQ: thin/darken the signal"
        , annotations: [ ann 0 "Dark", ann 64 "Flat", ann 127 "Thin" ] }
      ]
  }

dipSection :: Section
dipSection =
  { name: "DIP Switches"
  , compact: true
  , collapsed: true
  , layout: DipGrid
  , description: Just "Physical DIP switches \x2014 left bank (ramping), right bank (customize)"
  , controls:
      [ Toggle { cc: cc 61, label: "Size", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 62, label: "Error", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 63, label: "Sustain", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 64, label: "Texture", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 65, label: "Octave", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 66, label: "Bounce", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 67, label: "Sweep", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 68, label: "Polarity", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Toggle { cc: cc 71, label: "Miso", onLabel: "On", offLabel: "Off", description: Just "Mono in, stereo out", labelSource: Nothing }
      , Toggle { cc: cc 72, label: "Spread", onLabel: "On", offLabel: "Off", description: Just "Stereo widening tied to Error", labelSource: Nothing }
      , Toggle { cc: cc 73, label: "Latch", onLabel: "On", offLabel: "Off", description: Just "Hold becomes latch instead of momentary", labelSource: Nothing }
      , Toggle { cc: cc 74, label: "Sidechain", onLabel: "On", offLabel: "Off", description: Just "Freeze dips when Glitch resets", labelSource: Nothing }
      , Toggle { cc: cc 75, label: "Duck", onLabel: "On", offLabel: "Off", description: Just "Both channels dip when input detected", labelSource: Nothing }
      , Toggle { cc: cc 76, label: "Reverse", onLabel: "On", offLabel: "Off", description: Just "Glitch plays in reverse", labelSource: Nothing }
      , Toggle { cc: cc 77, label: "1/2 Speed", onLabel: "On", offLabel: "Off", description: Just "Doubles sample size, lo-fi quality", labelSource: Nothing }
      , Toggle { cc: cc 78, label: "Manual", onLabel: "On", offLabel: "Off", description: Just "Disable dynamic control, manual resample only", labelSource: Nothing }
      ]
  }

rampingSection :: Section
rampingSection =
  { name: "Ramping"
  , compact: true
  , collapsed: true
  , layout: DefaultLayout
  , description: Just "Automate knob movement (bounce or one-shot ramp)"
  , controls:
      [ Toggle { cc: cc 52, label: "Ramp/Bounce Active", onLabel: "On", offLabel: "Off", description: Nothing, labelSource: Nothing }
      , Slider { cc: cc 20, label: Static "Ramp Speed", description: Nothing
        , annotations: [ ann 0 "Slow", ann 127 "Fast" ] }
      ]
  }

otherSection :: Section
otherSection =
  { name: "Other"
  , compact: true
  , collapsed: true
  , layout: DefaultLayout
  , description: Nothing
  , controls:
      [ Toggle { cc: cc 57, label: "Dry Kill", onLabel: "On", offLabel: "Off", description: Just "Remove dry signal from output", labelSource: Nothing }
      , Toggle { cc: cc 58, label: "Trails", onLabel: "On", offLabel: "Off", description: Just "Fade away when bypassed", labelSource: Nothing }
      , Toggle { cc: cc 51, label: "MIDI Clock Follow", onLabel: "On", offLabel: "Off", description: Just "Sync timing to external clock", labelSource: Nothing }
      , Dropdown { cc: cc 53, label: "Sync Subdivision", description: Nothing, options:
          [ { label: "Whole", value: mv 0, description: Nothing }
          , { label: "Dotted Half", value: mv 1, description: Nothing }
          , { label: "Half", value: mv 2, description: Nothing }
          , { label: "Dotted Quarter", value: mv 3, description: Nothing }
          , { label: "Quarter", value: mv 4, description: Nothing }
          , { label: "Dotted 8th", value: mv 5, description: Nothing }
          , { label: "8th", value: mv 6, description: Nothing }
          , { label: "8th Triplet", value: mv 7, description: Nothing }
          , { label: "16th", value: mv 8, description: Nothing }
          ] }
      , Slider { cc: cc 100, label: Static "Expression", description: Nothing
        , annotations: [ ann 0 "Min", ann 127 "Max" ] }
      , Momentary { cc: cc 93, label: "Tap Tempo", value: mv 127, description: Nothing }
      ]
  }
