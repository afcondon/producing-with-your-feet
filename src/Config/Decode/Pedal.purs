module Config.Decode.Pedal
  ( decodePedal
  ) where

import Prelude

import Color (fromHexString)
import Config.Decode.Control (decodeControl)
import Config.Decode.Modes (decodeDualChannelModes)
import Config.Decode.Twister (decodeTwisterMapping)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, makeCC, makeMidiValue)
import Data.Pedal (PedalDef, PedalId(..), PedalMeta, Section, SectionLayout(..))
import Data.Pedal.Engage (EngageConfig(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

lookupStr :: String -> FO.Object Json -> Maybe String
lookupStr key obj = FO.lookup key obj >>= Json.toString

lookupNum :: String -> FO.Object Json -> Maybe Int
lookupNum key obj = do
  j <- FO.lookup key obj
  n <- Json.toNumber j
  Int.fromNumber n

lookupBool :: String -> FO.Object Json -> Maybe Boolean
lookupBool key obj = FO.lookup key obj >>= Json.toBoolean

-- | Decode a complete PedalDef from JSON.
-- | The channel parameter allows rig.json to override the pedal's defaultChannel.
decodePedal :: Maybe Int -> Json -> Maybe PedalDef
decodePedal channelOverride json = do
  obj <- Json.toObject json
  meta <- FO.lookup "meta" obj >>= decodeMeta channelOverride
  engage <- FO.lookup "engage" obj >>= decodeEngage
  baseline <- FO.lookup "baseline" obj >>= decodeCCMap
  resetOrder <- case FO.lookup "resetOrder" obj of
    Just roJson -> do
      arr <- Json.toArray roJson
      traverse (\j -> Json.toNumber j >>= Int.fromNumber >>= makeCC) arr
    Nothing -> Just []
  let twister = FO.lookup "twister" obj >>= \j ->
        if Json.isNull j then Nothing else decodeTwisterMapping j
      modes = FO.lookup "modes" obj >>= \j ->
        if Json.isNull j then Nothing else decodeDualChannelModes j
  sectionsJson <- FO.lookup "sections" obj >>= Json.toArray
  sections <- traverse decodeSection sectionsJson
  Just { meta, engage, baseline, resetOrder, twister, modes, layout: Nothing, sections }

decodeMeta :: Maybe Int -> Json -> Maybe PedalMeta
decodeMeta channelOverride json = do
  obj <- Json.toObject json
  idStr <- lookupStr "id" obj
  name <- lookupStr "name" obj
  shortName <- lookupStr "shortName" obj
  brand <- lookupStr "brand" obj
  let defaultChannel = case channelOverride of
        Just ch -> ch
        Nothing -> case lookupNum "defaultChannel" obj of
          Just ch -> ch
          Nothing -> 1
      color = lookupStr "color" obj >>= fromHexString
      saveInstructions = lookupStr "saveInstructions" obj
  Just { id: PedalId idStr, name, shortName, brand, color, defaultChannel, saveInstructions }

decodeEngage :: Json -> Maybe EngageConfig
decodeEngage json = do
  obj <- Json.toObject json
  typ <- lookupStr "type" obj
  case typ of
    "single" -> do
      cc <- lookupNum "cc" obj >>= makeCC
      Just (SingleEngage cc)
    "dual" -> do
      aObj <- FO.lookup "a" obj >>= Json.toObject
      aCc <- lookupNum "cc" aObj >>= makeCC
      aLabel <- lookupStr "label" aObj
      bObj <- FO.lookup "b" obj >>= Json.toObject
      bCc <- lookupNum "cc" bObj >>= makeCC
      bLabel <- lookupStr "label" bObj
      Just (DualEngage { a: { cc: aCc, label: aLabel }, b: { cc: bCc, label: bLabel } })
    _ -> Nothing

decodeCCMap :: Json -> Maybe (Map.Map CC MidiValue)
decodeCCMap json = do
  obj <- Json.toObject json
  let entries = FO.toUnfoldable obj :: Array (Tuple String Json)
  parsed <- traverse parseEntry entries
  Just (Map.fromFoldable parsed)
  where
  parseEntry :: Tuple String Json -> Maybe (Tuple CC MidiValue)
  parseEntry (Tuple key valJson) = do
    ccInt <- Int.fromString key
    cc <- makeCC ccInt
    numVal <- Json.toNumber valJson
    intVal <- Int.fromNumber numVal
    mv <- makeMidiValue intVal
    Just (Tuple cc mv)

decodeSection :: Json -> Maybe Section
decodeSection json = do
  obj <- Json.toObject json
  name <- lookupStr "name" obj
  let compact = case lookupBool "compact" obj of
        Just b -> b
        Nothing -> false
      collapsed = case lookupBool "collapsed" obj of
        Just b -> b
        Nothing -> false
      layout = case lookupStr "layout" obj of
        Just "dip-grid" -> DipGrid
        Just "dual-column" -> DualColumn
        _ -> DefaultLayout
      description = lookupStr "description" obj
  controlsJson <- FO.lookup "controls" obj >>= Json.toArray
  controls <- traverse decodeControl controlsJson
  Just { name, compact, collapsed, layout, description, controls }
