module Config.Preset
  ( presetToReadableJson
  , boardPresetToReadableJson
  , presetsToReadableJsonString
  , boardPresetsToReadableJsonString
  ) where

import Prelude

import Config.Registry (PedalRegistry, findPedal)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, unCC, unMidiValue, unProgramNumber)
import Data.Pedal (Control(..), LabelSource(..), PedalDef, PedalId(..))
import Data.Preset (BoardPreset, BoardPresetEntry, PedalPreset)
import Data.Tuple (Tuple(..))
import Engine.Storage (engageStateToString)
import Foreign.Object as FO

-- | Build a Map from CC number to label string for a pedal definition.
-- | For dynamic labels (ModeMap, ChannelMode), falls back to "CC <n>".
buildCCLabels :: PedalDef -> Map.Map CC String
buildCCLabels def =
  let controls = Array.concatMap _.controls def.sections
  in Map.fromFoldable $ Array.concatMap controlLabels controls
  where
  controlLabels :: Control -> Array (Tuple CC String)
  controlLabels = case _ of
    Slider r -> [ Tuple r.cc (resolveLabel r.label) ]
    Toggle r -> [ Tuple r.cc r.label ]
    Segmented r -> [ Tuple r.cc r.label ]
    Dropdown r -> [ Tuple r.cc r.label ]
    Momentary r -> [ Tuple r.cc r.label ]
    PianoKey r -> [ Tuple r.cc r.label ]
    RangeSelect r -> [ Tuple r.cc r.label ]
    ModeRanges r -> [ Tuple r.cc r.label ]
    RadioGroup _ -> []
    ModeRadio _ -> []
    InfoToggle _ -> []

  resolveLabel :: LabelSource -> String
  resolveLabel = case _ of
    Static s -> s
    ModeMap r -> "CC " <> show (unCC r.cc)
    ChannelMode _ -> "CC (channel mode)"

-- | Encode a preset value entry as { "value": n, "label": "..." }
valueEntryToJson :: String -> Int -> Json
valueEntryToJson label val =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "value" (Json.fromNumber (Int.toNumber val))
    , Tuple "label" (Json.fromString label)
    ]

-- | Encode preset values with human-readable labels from the pedal registry.
readableValuesToJson :: Map.Map CC String -> Map.Map CC MidiValue -> Json
readableValuesToJson labels vals =
  Json.fromObject $ FO.fromFoldable $
    map (\(Tuple cc mv) ->
      let ccInt = unCC cc
          label = fromMaybe ("CC " <> show ccInt) (Map.lookup cc labels)
      in Tuple (show ccInt) (valueEntryToJson label (unMidiValue mv))
    ) (Map.toUnfoldable vals :: Array _)

-- | Encode a PedalPreset to human-readable JSON with control labels.
presetToReadableJson :: PedalRegistry -> PedalPreset -> Json
presetToReadableJson registry preset =
  let labels = case findPedal registry preset.pedalId of
        Just def -> buildCCLabels def
        Nothing -> Map.empty
      (PedalId pid) = preset.pedalId
  in Json.fromObject $ FO.fromFoldable $
    [ Tuple "id" (Json.fromString preset.id)
    , Tuple "pedal" (Json.fromString pid)
    , Tuple "name" (Json.fromString preset.name)
    , Tuple "description" (Json.fromString preset.description)
    , Tuple "notes" (Json.fromString preset.notes)
    , Tuple "values" (readableValuesToJson labels preset.values)
    , Tuple "slot" (case preset.savedSlot of
        Nothing -> Json.jsonNull
        Just slot -> Json.fromNumber (Int.toNumber (unProgramNumber slot)))
    , Tuple "created" (Json.fromString preset.created)
    , Tuple "modified" (Json.fromString preset.modified)
    ]

-- | Encode a board preset entry with decorative preset name.
boardEntryToReadableJson :: Array PedalPreset -> BoardPresetEntry -> Json
boardEntryToReadableJson allPresets entry =
  let presetName = do
        pid <- entry.presetId
        p <- Array.find (\pr -> pr.id == pid) allPresets
        pure p.name
  in Json.fromObject $ FO.fromFoldable
    [ Tuple "preset" (case entry.presetId of
        Nothing -> Json.jsonNull
        Just pid -> Json.fromString pid)
    , Tuple "presetName" (case presetName of
        Nothing -> Json.jsonNull
        Just n -> Json.fromString n)
    , Tuple "engage" (Json.fromString (engageStateToString entry.engage))
    ]

-- | Encode a BoardPreset to human-readable JSON with decorative preset names.
boardPresetToReadableJson :: Array PedalPreset -> BoardPreset -> Json
boardPresetToReadableJson allPresets bp =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "id" (Json.fromString bp.id)
    , Tuple "name" (Json.fromString bp.name)
    , Tuple "description" (Json.fromString bp.description)
    , Tuple "notes" (Json.fromString bp.notes)
    , Tuple "pedals" (boardPedalsToReadableJson allPresets bp.pedals)
    , Tuple "created" (Json.fromString bp.created)
    , Tuple "modified" (Json.fromString bp.modified)
    ]
  where
  boardPedalsToReadableJson :: Array PedalPreset -> Map.Map PedalId BoardPresetEntry -> Json
  boardPedalsToReadableJson presets pedals =
    Json.fromObject $ FO.fromFoldable $
      map (\(Tuple (PedalId pid) entry) -> Tuple pid (boardEntryToReadableJson presets entry))
        (Map.toUnfoldable pedals :: Array _)

presetsToReadableJsonString :: PedalRegistry -> Array PedalPreset -> String
presetsToReadableJsonString registry = stringify <<< Json.fromArray <<< map (presetToReadableJson registry)

boardPresetsToReadableJsonString :: Array PedalPreset -> Array BoardPreset -> String
boardPresetsToReadableJsonString presets = stringify <<< Json.fromArray <<< map (boardPresetToReadableJson presets)
