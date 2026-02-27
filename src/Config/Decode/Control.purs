module Config.Decode.Control
  ( decodeControl
  , decodeLabelSource
  , decodeSelectOption
  , decodeRangeOption
  , decodeAnnotation
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, makeCC, makeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), ModeRangesMode, RangeOption, SelectOption)
import Data.Pedal.Modes (ModeChannel(..), ModeRole(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

-- Helpers

lookupStr :: String -> FO.Object Json -> Maybe String
lookupStr key obj = FO.lookup key obj >>= Json.toString

lookupNum :: String -> FO.Object Json -> Maybe Int
lookupNum key obj = do
  j <- FO.lookup key obj
  n <- Json.toNumber j
  Int.fromNumber n

lookupCC :: String -> FO.Object Json -> Maybe CC
lookupCC key obj = lookupNum key obj >>= makeCC

lookupMV :: String -> FO.Object Json -> Maybe MidiValue
lookupMV key obj = lookupNum key obj >>= makeMidiValue

-- | Decode a control from JSON using "type" discriminator
decodeControl :: Json -> Maybe Control
decodeControl json = do
  obj <- Json.toObject json
  typ <- lookupStr "type" obj
  case typ of
    "slider" -> decodeSlider obj
    "toggle" -> decodeToggle obj
    "segmented" -> decodeSegmented obj
    "dropdown" -> decodeDropdown obj
    "momentary" -> decodeMomentary obj
    "pianoKey" -> decodePianoKey obj
    "rangeSelect" -> decodeRangeSelect obj
    "modeRanges" -> decodeModeRanges obj
    "radioGroup" -> decodeRadioGroup obj
    "modeRadio" -> decodeModeRadio obj
    "infoToggle" -> decodeInfoToggle obj
    _ -> Nothing

decodeSlider :: FO.Object Json -> Maybe Control
decodeSlider obj = do
  cc <- lookupCC "cc" obj
  label <- FO.lookup "label" obj >>= decodeLabelSource
  let description = lookupStr "description" obj
  annotations <- case FO.lookup "annotations" obj of
    Just arrJson -> do
      arr <- Json.toArray arrJson
      traverse decodeAnnotation arr
    Nothing -> Just []
  Just $ Slider { cc, label, description, annotations }

decodeToggle :: FO.Object Json -> Maybe Control
decodeToggle obj = do
  cc <- lookupCC "cc" obj
  label <- lookupStr "label" obj
  let onLabel = case lookupStr "onLabel" obj of
        Just l -> l
        Nothing -> "On"
      offLabel = case lookupStr "offLabel" obj of
        Just l -> l
        Nothing -> "Off"
      description = lookupStr "description" obj
      labelSource = FO.lookup "labelSource" obj >>= decodeLabelSource
  Just $ Toggle { cc, label, onLabel, offLabel, description, labelSource }

decodeSegmented :: FO.Object Json -> Maybe Control
decodeSegmented obj = do
  cc <- lookupCC "cc" obj
  label <- lookupStr "label" obj
  optionsJson <- FO.lookup "options" obj >>= Json.toArray
  options <- traverse decodeSelectOption optionsJson
  Just $ Segmented { cc, label, options }

decodeDropdown :: FO.Object Json -> Maybe Control
decodeDropdown obj = do
  cc <- lookupCC "cc" obj
  label <- lookupStr "label" obj
  optionsJson <- FO.lookup "options" obj >>= Json.toArray
  options <- traverse decodeSelectOption optionsJson
  let description = lookupStr "description" obj
  Just $ Dropdown { cc, label, options, description }

decodeMomentary :: FO.Object Json -> Maybe Control
decodeMomentary obj = do
  cc <- lookupCC "cc" obj
  label <- lookupStr "label" obj
  value <- lookupMV "value" obj
  let description = lookupStr "description" obj
  Just $ Momentary { cc, label, value, description }

decodePianoKey :: FO.Object Json -> Maybe Control
decodePianoKey obj = do
  cc <- lookupCC "cc" obj
  label <- lookupStr "label" obj
  optionsJson <- FO.lookup "options" obj >>= Json.toArray
  options <- traverse decodeSelectOption optionsJson
  chromaticValue <- lookupMV "chromaticValue" obj
  Just $ PianoKey { cc, label, options, chromaticValue }

decodeRangeSelect :: FO.Object Json -> Maybe Control
decodeRangeSelect obj = do
  cc <- lookupCC "cc" obj
  label <- lookupStr "label" obj
  rangesJson <- FO.lookup "ranges" obj >>= Json.toArray
  ranges <- traverse decodeRangeOption rangesJson
  Just $ RangeSelect { cc, label, ranges }

decodeModeRanges :: FO.Object Json -> Maybe Control
decodeModeRanges obj = do
  cc <- lookupCC "cc" obj
  label <- lookupStr "label" obj
  modeCC <- lookupCC "modeCC" obj
  modesJson <- FO.lookup "modes" obj >>= Json.toArray
  modes <- traverse decodeModeRangesMode modesJson
  Just $ ModeRanges { cc, label, modeCC, modes }

decodeRadioGroup :: FO.Object Json -> Maybe Control
decodeRadioGroup obj = do
  label <- lookupStr "label" obj
  mappingJson <- FO.lookup "mapping" obj >>= Json.toArray
  mapping <- traverse decodeRadioMapping mappingJson
  Just $ RadioGroup { label, mapping }

decodeModeRadio :: FO.Object Json -> Maybe Control
decodeModeRadio obj = do
  label <- lookupStr "label" obj
  modeChannelStr <- lookupStr "modeChannel" obj
  modeChannel <- parseModeChannel modeChannelStr
  Just $ ModeRadio { label, modeChannel }

decodeInfoToggle :: FO.Object Json -> Maybe Control
decodeInfoToggle obj = do
  key <- lookupStr "key" obj
  label <- lookupStr "label" obj
  let description = lookupStr "description" obj
  Just $ InfoToggle { key, label, description }

-- Sub-decoders

decodeLabelSource :: Json -> Maybe LabelSource
decodeLabelSource json =
  -- String -> Static
  case Json.toString json of
    Just s -> Just (Static s)
    Nothing -> do
      obj <- Json.toObject json
      -- { modeMap: { cc, labels } }
      case FO.lookup "modeMap" obj of
        Just mmJson -> do
          mmObj <- Json.toObject mmJson
          cc <- lookupCC "cc" mmObj
          labelsObj <- FO.lookup "labels" mmObj >>= Json.toObject
          let entries = FO.toUnfoldable labelsObj :: Array (Tuple String Json)
          parsed <- traverse parseModeLabel entries
          Just (ModeMap { cc, labels: Map.fromFoldable parsed })
        Nothing ->
          -- { channelMode: { channel, role } }
          case FO.lookup "channelMode" obj of
            Just cmJson -> do
              cmObj <- Json.toObject cmJson
              channelStr <- lookupStr "channel" cmObj
              channel <- parseModeChannel channelStr
              roleStr <- lookupStr "role" cmObj
              role <- parseModeRole roleStr
              Just (ChannelMode { channel, role })
            Nothing -> Nothing

parseModeLabel :: Tuple String Json -> Maybe (Tuple MidiValue String)
parseModeLabel (Tuple key valJson) = do
  intKey <- Int.fromString key
  mv <- makeMidiValue intKey
  label <- Json.toString valJson
  Just (Tuple mv label)

parseModeChannel :: String -> Maybe ModeChannel
parseModeChannel = case _ of
  "left" -> Just LeftChannel
  "right" -> Just RightChannel
  _ -> Nothing

parseModeRole :: String -> Maybe ModeRole
parseModeRole = case _ of
  "time" -> Just TimeRole
  "modify" -> Just ModifyRole
  "alt" -> Just AltRole
  "hold" -> Just HoldRole
  _ -> Nothing

decodeAnnotation :: Json -> Maybe Annotation
decodeAnnotation json = do
  obj <- Json.toObject json
  position <- lookupMV "position" obj
  label <- lookupStr "label" obj
  Just { position, label }

decodeSelectOption :: Json -> Maybe SelectOption
decodeSelectOption json = do
  obj <- Json.toObject json
  label <- lookupStr "label" obj
  value <- lookupMV "value" obj
  let description = lookupStr "description" obj
  Just { label, value, description }

decodeRangeOption :: Json -> Maybe RangeOption
decodeRangeOption json = do
  obj <- Json.toObject json
  lo <- lookupMV "lo" obj
  hi <- lookupMV "hi" obj
  label <- lookupStr "label" obj
  let description = lookupStr "description" obj
  Just { lo, hi, label, description }

decodeModeRangesMode :: Json -> Maybe ModeRangesMode
decodeModeRangesMode json = do
  obj <- Json.toObject json
  lo <- lookupMV "lo" obj
  hi <- lookupMV "hi" obj
  rangesJson <- FO.lookup "ranges" obj >>= Json.toArray
  ranges <- traverse (\rj -> do
    innerArr <- Json.toArray rj
    traverse decodeRangeOption innerArr) rangesJson
  Just { lo, hi, ranges }

decodeRadioMapping :: Json -> Maybe { label :: String, values :: Map.Map CC MidiValue }
decodeRadioMapping json = do
  obj <- Json.toObject json
  label <- lookupStr "label" obj
  valuesObj <- FO.lookup "values" obj >>= Json.toObject
  let entries = FO.toUnfoldable valuesObj :: Array (Tuple String Json)
  parsed <- traverse parseValueEntry entries
  Just { label, values: Map.fromFoldable parsed }
  where
  parseValueEntry :: Tuple String Json -> Maybe (Tuple CC MidiValue)
  parseValueEntry (Tuple key valJson) = do
    ccInt <- Int.fromString key
    cc <- makeCC ccInt
    numVal <- Json.toNumber valJson
    intVal <- Int.fromNumber numVal
    mv <- makeMidiValue intVal
    Just (Tuple cc mv)
