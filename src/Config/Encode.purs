module Config.Encode
  ( pedalDefToJson
  , rigConfigToJson
  ) where

import Prelude

import Color (toHexString)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unCC, unMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), ModeRangesMode, PedalDef, PedalId(..), RangeOption, Section, SectionLayout(..), SelectOption)
import Data.Pedal.Engage (EngageConfig(..))
import Data.Pedal.Modes (ChannelDef, DualChannelModes, EffectDef, EffectVariant, ModeChannel(..), ModeRole(..))
import Data.Tuple (Tuple(..))
import Data.Twister (TwisterButton(..), TwisterEncoder(..), TwisterMapping)
import Foreign.Object as FO

infixr 6 Tuple as /\

-- Helpers

str :: String -> Json
str = Json.fromString

num :: Int -> Json
num = Json.fromNumber <<< Int.toNumber

bool :: Boolean -> Json
bool = Json.fromBoolean

arr :: Array Json -> Json
arr = Json.fromArray

obj :: Array (Tuple String Json) -> Json
obj = Json.fromObject <<< FO.fromFoldable

nullable :: forall a. (a -> Json) -> Maybe a -> Json
nullable f = case _ of
  Nothing -> Json.jsonNull
  Just a -> f a

-- | Encode a complete PedalDef to JSON
pedalDefToJson :: PedalDef -> Json
pedalDefToJson def = obj
  [ "meta" /\ metaToJson def
  , "engage" /\ engageToJson def.engage
  , "baseline" /\ ccMapToJson def.baseline
  , "resetOrder" /\ arr (map (num <<< unCC) def.resetOrder)
  , "twister" /\ nullable twisterToJson def.twister
  , "modes" /\ nullable modesToJson def.modes
  , "sections" /\ arr (map sectionToJson def.sections)
  ]

metaToJson :: PedalDef -> Json
metaToJson def = obj $
  [ "id" /\ str (let PedalId pid = def.meta.id in pid)
  , "name" /\ str def.meta.name
  , "shortName" /\ str def.meta.shortName
  , "brand" /\ str def.meta.brand
  , "defaultChannel" /\ num def.meta.defaultChannel
  ] <> case def.meta.color of
    Just c -> [ "color" /\ str (toHexString c) ]
    Nothing -> []
  <> case def.meta.saveInstructions of
    Just si -> [ "saveInstructions" /\ str si ]
    Nothing -> []

engageToJson :: EngageConfig -> Json
engageToJson = case _ of
  SingleEngage cc -> obj
    [ "type" /\ str "single"
    , "cc" /\ num (unCC cc)
    ]
  DualEngage { a, b } -> obj
    [ "type" /\ str "dual"
    , "a" /\ obj [ "cc" /\ num (unCC a.cc), "label" /\ str a.label ]
    , "b" /\ obj [ "cc" /\ num (unCC b.cc), "label" /\ str b.label ]
    ]

ccMapToJson :: Map.Map CC MidiValue -> Json
ccMapToJson vals =
  obj $ map (\(Tuple cc' mv') -> show (unCC cc') /\ num (unMidiValue mv'))
    (Map.toUnfoldable vals :: Array _)

-- Twister

twisterToJson :: TwisterMapping -> Json
twisterToJson tw = obj
  [ "hue" /\ num tw.hue
  , "encoders" /\ arr (map (nullable encoderToJson) tw.encoders)
  , "buttons" /\ arr (map (nullable buttonToJson) tw.buttons)
  ]

encoderToJson :: TwisterEncoder -> Json
encoderToJson (TwisterCC { cc, center, options }) = obj $
  [ "cc" /\ num (unCC cc) ]
  <> case center of
    Just c -> [ "center" /\ num (unMidiValue c) ]
    Nothing -> []
  <> case options of
    Just opts -> [ "options" /\ arr (map (num <<< unMidiValue) opts) ]
    Nothing -> []

buttonToJson :: TwisterButton -> Json
buttonToJson = case _ of
  TwisterToggle { cc } -> obj
    [ "type" /\ str "toggle"
    , "cc" /\ num (unCC cc)
    ]
  TwisterMomentary { cc } -> obj
    [ "type" /\ str "momentary"
    , "cc" /\ num (unCC cc)
    ]
  TwisterSet { cc, value } -> obj
    [ "type" /\ str "set"
    , "cc" /\ num (unCC cc)
    , "value" /\ num (unMidiValue value)
    ]

-- Dual channel modes

modesToJson :: DualChannelModes -> Json
modesToJson modes = obj
  [ "effects" /\ arr (map effectDefToJson modes.effects)
  , "left" /\ channelDefToJson modes.left
  , "right" /\ channelDefToJson modes.right
  ]

effectDefToJson :: EffectDef -> Json
effectDefToJson e = obj
  [ "id" /\ str e.id
  , "label" /\ str e.label
  , "a" /\ variantToJson e.a
  , "b" /\ variantToJson e.b
  , "hold" /\ str e.hold
  ]

variantToJson :: EffectVariant -> Json
variantToJson v = obj
  [ "name" /\ str v.name
  , "time" /\ str v.time
  , "modify" /\ str v.modify
  , "alt" /\ str v.alt
  ]

channelDefToJson :: ChannelDef -> Json
channelDefToJson cd = obj
  [ "modeCC" /\ num (unCC cd.modeCC)
  , "swapCC" /\ num (unCC cd.swapCC)
  , "native" /\ arr (map str cd.native)
  ]

-- Sections & Controls

sectionToJson :: Section -> Json
sectionToJson s = obj $
  [ "name" /\ str s.name
  , "compact" /\ bool s.compact
  , "collapsed" /\ bool s.collapsed
  , "layout" /\ str (layoutStr s.layout)
  , "controls" /\ arr (map controlToJson s.controls)
  ] <> case s.description of
    Just d -> [ "description" /\ str d ]
    Nothing -> []

layoutStr :: SectionLayout -> String
layoutStr = case _ of
  DefaultLayout -> "default"
  DipGrid -> "dip-grid"
  DualColumn -> "dual-column"

controlToJson :: Control -> Json
controlToJson = case _ of
  Slider r -> obj $
    [ "type" /\ str "slider"
    , "cc" /\ num (unCC r.cc)
    , "label" /\ labelSourceToJson r.label
    , "annotations" /\ arr (map annotationToJson r.annotations)
    ] <> optDesc r.description

  Toggle r -> obj $
    [ "type" /\ str "toggle"
    , "cc" /\ num (unCC r.cc)
    , "label" /\ str r.label
    , "onLabel" /\ str r.onLabel
    , "offLabel" /\ str r.offLabel
    ] <> optDesc r.description
    <> case r.labelSource of
      Just ls -> [ "labelSource" /\ labelSourceToJson ls ]
      Nothing -> []

  Segmented r -> obj
    [ "type" /\ str "segmented"
    , "cc" /\ num (unCC r.cc)
    , "label" /\ str r.label
    , "options" /\ arr (map selectOptionToJson r.options)
    ]

  Dropdown r -> obj $
    [ "type" /\ str "dropdown"
    , "cc" /\ num (unCC r.cc)
    , "label" /\ str r.label
    , "options" /\ arr (map selectOptionToJson r.options)
    ] <> optDesc r.description

  Momentary r -> obj $
    [ "type" /\ str "momentary"
    , "cc" /\ num (unCC r.cc)
    , "label" /\ str r.label
    , "value" /\ num (unMidiValue r.value)
    ] <> optDesc r.description

  PianoKey r -> obj
    [ "type" /\ str "pianoKey"
    , "cc" /\ num (unCC r.cc)
    , "label" /\ str r.label
    , "options" /\ arr (map selectOptionToJson r.options)
    , "chromaticValue" /\ num (unMidiValue r.chromaticValue)
    ]

  RangeSelect r -> obj
    [ "type" /\ str "rangeSelect"
    , "cc" /\ num (unCC r.cc)
    , "label" /\ str r.label
    , "ranges" /\ arr (map rangeOptionToJson r.ranges)
    ]

  ModeRanges r -> obj
    [ "type" /\ str "modeRanges"
    , "cc" /\ num (unCC r.cc)
    , "label" /\ str r.label
    , "modeCC" /\ num (unCC r.modeCC)
    , "modes" /\ arr (map modeRangesModeToJson r.modes)
    ]

  RadioGroup r -> obj
    [ "type" /\ str "radioGroup"
    , "label" /\ str r.label
    , "mapping" /\ arr (map radioMappingToJson r.mapping)
    ]

  ModeRadio r -> obj
    [ "type" /\ str "modeRadio"
    , "label" /\ str r.label
    , "modeChannel" /\ str (modeChannelStr r.modeChannel)
    ]

  InfoToggle r -> obj $
    [ "type" /\ str "infoToggle"
    , "key" /\ str r.key
    , "label" /\ str r.label
    ] <> optDesc r.description

optDesc :: Maybe String -> Array (Tuple String Json)
optDesc = case _ of
  Just d -> [ "description" /\ str d ]
  Nothing -> []

labelSourceToJson :: LabelSource -> Json
labelSourceToJson = case _ of
  Static s -> str s
  ModeMap { cc, labels } -> obj
    [ "modeMap" /\ obj
        [ "cc" /\ num (unCC cc)
        , "labels" /\ obj (map (\(Tuple mv' lbl) -> show (unMidiValue mv') /\ str lbl)
            (Map.toUnfoldable labels :: Array _))
        ]
    ]
  ChannelMode { channel, role } -> obj
    [ "channelMode" /\ obj
        [ "channel" /\ str (modeChannelStr channel)
        , "role" /\ str (modeRoleStr role)
        ]
    ]

modeChannelStr :: ModeChannel -> String
modeChannelStr = case _ of
  LeftChannel -> "left"
  RightChannel -> "right"

modeRoleStr :: ModeRole -> String
modeRoleStr = case _ of
  TimeRole -> "time"
  ModifyRole -> "modify"
  AltRole -> "alt"
  HoldRole -> "hold"

annotationToJson :: Annotation -> Json
annotationToJson a = obj
  [ "position" /\ num (unMidiValue a.position)
  , "label" /\ str a.label
  ]

selectOptionToJson :: SelectOption -> Json
selectOptionToJson o = obj $
  [ "label" /\ str o.label
  , "value" /\ num (unMidiValue o.value)
  ] <> case o.description of
    Just d -> [ "description" /\ str d ]
    Nothing -> []

rangeOptionToJson :: RangeOption -> Json
rangeOptionToJson r = obj $
  [ "lo" /\ num (unMidiValue r.lo)
  , "hi" /\ num (unMidiValue r.hi)
  , "label" /\ str r.label
  ] <> case r.description of
    Just d -> [ "description" /\ str d ]
    Nothing -> []

modeRangesModeToJson :: ModeRangesMode -> Json
modeRangesModeToJson m = obj
  [ "lo" /\ num (unMidiValue m.lo)
  , "hi" /\ num (unMidiValue m.hi)
  , "ranges" /\ arr (map (\rs -> arr (map rangeOptionToJson rs)) m.ranges)
  ]

radioMappingToJson :: { label :: String, values :: Map.Map CC MidiValue } -> Json
radioMappingToJson r = obj
  [ "label" /\ str r.label
  , "values" /\ ccMapToJson r.values
  ]

-- | Encode rig config (for generating rig.json)
rigConfigToJson :: { name :: String, pedals :: Array { file :: String, channel :: Int } } -> Json
rigConfigToJson rig = obj
  [ "name" /\ str rig.name
  , "storagePrefix" /\ str "pedal-explorer-"
  , "pedals" /\ arr (map (\p -> obj [ "file" /\ str p.file, "channel" /\ num p.channel ]) rig.pedals)
  , "midiRouting" /\ obj
      [ "pedalOutput" /\ obj [ "match" /\ str "Morningstar" ]
      , "twisterInput" /\ obj [ "match" /\ str "Midi Fighter Twister" ]
      , "twisterOutput" /\ obj [ "match" /\ str "Midi Fighter Twister" ]
      , "loopyOutput" /\ obj [ "match" /\ str "AUDIO4c" ]
      , "loopyChannel" /\ num 16
      ]
  , "slotRanges" /\ arr
      [ obj [ "brand" /\ str "Meris", "range" /\ obj [ "start" /\ num 0, "count" /\ num 16 ] ]
      , obj [ "brand" /\ str "Strymon", "range" /\ obj [ "start" /\ num 50, "count" /\ num 26 ] ]
      , obj [ "brand" /\ str "Chase Bliss", "range" /\ obj [ "start" /\ num 1, "count" /\ num 122 ] ]
      ]
  , "looper" /\ str "loopers/loopypro.json"
  , "controller" /\ str "controllers/mc6-banks.json"
  ]
