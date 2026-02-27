module Engine.Storage
  ( saveEngine
  , loadEngine
  , loadEngineState
  , savePresets
  , loadPresets
  , loadPresetsParsed
  , saveBoardPresets
  , loadBoardPresets
  , loadBoardPresetsParsed
  , saveCardOrder
  , loadCardOrder
  , loadCardOrderParsed
  , parseEngine
  , parseCardOrder
  , parsePresets
  , parseBoardPresets
  , parseEngageState
  , engineToJson
  , nowISO
  , engageStateToString
  , presetToJson
  , boardPresetToJson
  , presetsToJsonString
  , boardPresetsToJsonString
  , StorageKey(..)
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Either (hush)
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, makeCC, makeMidiValue, makeProgramNumber, unCC, unMidiValue, unProgramNumber)
import Data.Pedal (PedalId(..))
import Data.Pedal.Engage (EngageState(..))
import Data.Preset (BoardPreset, BoardPresetEntry, PedalPreset)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Engine (EngineState, PedalState)
import Foreign.Object as FO
import Pedals.Registry as Registry
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as Storage

data StorageKey
  = EngineKey
  | AutosaveKey
  | PresetsKey
  | BoardPresetsKey
  | CardOrderKey
  | LoopyChannelKey

keyString :: StorageKey -> String
keyString = case _ of
  EngineKey -> "pedal-explorer-engine"
  AutosaveKey -> "pedal-explorer-autosave"
  PresetsKey -> "pedal-explorer-presets"
  BoardPresetsKey -> "pedal-explorer-board-presets"
  CardOrderKey -> "pedal-explorer-card-order"
  LoopyChannelKey -> "pedal-explorer-loopy-channel"

getStorage :: Effect Storage.Storage
getStorage = window >>= localStorage

setItem :: StorageKey -> String -> Effect Unit
setItem key val = do
  store <- getStorage
  Storage.setItem (keyString key) val store

getItem :: StorageKey -> Effect (Maybe String)
getItem key = do
  store <- getStorage
  Storage.getItem (keyString key) store

-- Encoders

saveEngine :: EngineState -> Effect Unit
saveEngine engine = do
  let json = engineToJson engine
  setItem EngineKey (stringify json)

engineToJson :: EngineState -> Json
engineToJson engine =
  Json.fromObject $ FO.fromFoldable $
    map
      (\(Tuple (PedalId pid) ps) ->
        Tuple pid (pedalStateToJson ps)
      )
      (Map.toUnfoldable engine :: Array _)

pedalStateToJson :: PedalState -> Json
pedalStateToJson ps =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "channel" (Json.fromNumber (Int.toNumber ps.channel))
    , Tuple "values" (valuesToJson ps.values)
    , Tuple "info" (infoToJson ps.info)
    ]

valuesToJson :: Map.Map CC MidiValue -> Json
valuesToJson vals =
  Json.fromObject $ FO.fromFoldable $
    map (\(Tuple cc' mv') -> Tuple (show (unCC cc')) (Json.fromNumber (Int.toNumber (unMidiValue mv'))))
      (Map.toUnfoldable vals :: Array _)

infoToJson :: Map.Map String Int -> Json
infoToJson info =
  Json.fromObject $ FO.fromFoldable $
    map (\(Tuple k v) -> Tuple k (Json.fromNumber (Int.toNumber v)))
      (Map.toUnfoldable info :: Array _)

loadEngine :: Effect (Maybe String)
loadEngine = getItem EngineKey

savePresets :: String -> Effect Unit
savePresets = setItem PresetsKey

loadPresets :: Effect (Maybe String)
loadPresets = getItem PresetsKey

saveBoardPresets :: String -> Effect Unit
saveBoardPresets = setItem BoardPresetsKey

loadBoardPresets :: Effect (Maybe String)
loadBoardPresets = getItem BoardPresetsKey

saveCardOrder :: Array PedalId -> Effect Unit
saveCardOrder order = do
  let json = Json.fromArray $ map (\(PedalId pid) -> Json.fromString pid) order
  setItem CardOrderKey (stringify json)

loadCardOrder :: Effect (Maybe String)
loadCardOrder = getItem CardOrderKey

-- Decoders

parseEngine :: String -> Maybe EngineState
parseEngine str = do
  json <- hush (jsonParser str)
  obj <- Json.toObject json
  let entries = FO.toUnfoldable obj :: Array (Tuple String Json)
  parsed <- traverse parsePedalEntry entries
  pure (Map.fromFoldable parsed)
  where
  parsePedalEntry :: Tuple String Json -> Maybe (Tuple PedalId PedalState)
  parsePedalEntry (Tuple key valJson) = do
    ps <- parsePedalState valJson
    pure (Tuple (PedalId key) ps)

parsePedalState :: Json -> Maybe PedalState
parsePedalState json = do
  obj <- Json.toObject json
  channelJson <- FO.lookup "channel" obj
  channelNum <- Json.toNumber channelJson
  channel <- Int.fromNumber channelNum
  values <- case FO.lookup "values" obj of
    Just vJson -> parseValues vJson
    Nothing -> Just Map.empty
  let info = case FO.lookup "info" obj of
        Just iJson -> fromMaybe Map.empty (parseInfo iJson)
        Nothing -> Map.empty
  pure { channel, values, info }

parseValues :: Json -> Maybe (Map.Map CC MidiValue)
parseValues json = do
  obj <- Json.toObject json
  let entries = FO.toUnfoldable obj :: Array (Tuple String Json)
  parsed <- traverse parseValueEntry entries
  pure (Map.fromFoldable parsed)
  where
  parseValueEntry :: Tuple String Json -> Maybe (Tuple CC MidiValue)
  parseValueEntry (Tuple key valJson) = do
    ccInt <- Int.fromString key
    cc <- makeCC ccInt
    numVal <- Json.toNumber valJson
    intVal <- Int.fromNumber numVal
    mv <- makeMidiValue intVal
    pure (Tuple cc mv)

parseInfo :: Json -> Maybe (Map.Map String Int)
parseInfo json = do
  obj <- Json.toObject json
  let entries = FO.toUnfoldable obj :: Array (Tuple String Json)
  parsed <- traverse parseInfoEntry entries
  pure (Map.fromFoldable parsed)
  where
  parseInfoEntry :: Tuple String Json -> Maybe (Tuple String Int)
  parseInfoEntry (Tuple key valJson) = do
    numVal <- Json.toNumber valJson
    intVal <- Int.fromNumber numVal
    pure (Tuple key intVal)

parseCardOrder :: String -> Maybe (Array PedalId)
parseCardOrder str = do
  json <- hush (jsonParser str)
  arr <- Json.toArray json
  traverse (\j -> PedalId <$> Json.toString j) arr

parsePresets :: String -> Maybe (Array PedalPreset)
parsePresets str = do
  json <- hush (jsonParser str)
  arr <- Json.toArray json
  traverse parsePreset arr

parsePreset :: Json -> Maybe PedalPreset
parsePreset json = do
  obj <- Json.toObject json
  idJson <- FO.lookup "id" obj
  id <- Json.toString idJson
  pedalIdJson <- FO.lookup "pedalId" obj
  pedalId <- PedalId <$> Json.toString pedalIdJson
  nameJson <- FO.lookup "name" obj
  name <- Json.toString nameJson
  descJson <- FO.lookup "description" obj
  description <- Json.toString descJson
  notesJson <- FO.lookup "notes" obj
  notes <- Json.toString notesJson
  valuesJson <- FO.lookup "values" obj
  values <- parseValues valuesJson
  let info = case FO.lookup "info" obj of
        Just iJson -> fromMaybe Map.empty (parseInfo iJson)
        Nothing -> Map.empty
  let savedSlot = do
        slotJson <- FO.lookup "savedSlot" obj
        numVal <- Json.toNumber slotJson
        intVal <- Int.fromNumber numVal
        makeProgramNumber intVal
  createdJson <- FO.lookup "created" obj
  created <- Json.toString createdJson
  modifiedJson <- FO.lookup "modified" obj
  modified <- Json.toString modifiedJson
  pure { id, pedalId, name, description, notes, values, info, savedSlot, created, modified }

parseBoardPresets :: String -> Maybe (Array BoardPreset)
parseBoardPresets str = do
  json <- hush (jsonParser str)
  arr <- Json.toArray json
  traverse parseBoardPreset arr

parseBoardPreset :: Json -> Maybe BoardPreset
parseBoardPreset json = do
  obj <- Json.toObject json
  idJson <- FO.lookup "id" obj
  id <- Json.toString idJson
  nameJson <- FO.lookup "name" obj
  name <- Json.toString nameJson
  descJson <- FO.lookup "description" obj
  description <- Json.toString descJson
  notesJson <- FO.lookup "notes" obj
  notes <- Json.toString notesJson
  pedalsJson <- FO.lookup "pedals" obj
  pedals <- parseBoardPedals pedalsJson
  createdJson <- FO.lookup "created" obj
  created <- Json.toString createdJson
  modifiedJson <- FO.lookup "modified" obj
  modified <- Json.toString modifiedJson
  pure { id, name, description, notes, pedals, created, modified }

parseBoardPedals :: Json -> Maybe (Map.Map PedalId BoardPresetEntry)
parseBoardPedals json = do
  obj <- Json.toObject json
  let entries = FO.toUnfoldable obj :: Array (Tuple String Json)
  parsed <- traverse parseBoardPedalEntry entries
  pure (Map.fromFoldable parsed)
  where
  parseBoardPedalEntry :: Tuple String Json -> Maybe (Tuple PedalId BoardPresetEntry)
  parseBoardPedalEntry (Tuple key valJson) = do
    entry <- parseBoardPresetEntry valJson
    pure (Tuple (PedalId key) entry)

parseBoardPresetEntry :: Json -> Maybe BoardPresetEntry
parseBoardPresetEntry json = do
  obj <- Json.toObject json
  engageJson <- FO.lookup "engage" obj
  engageStr <- Json.toString engageJson
  engage <- parseEngageState engageStr
  let presetId = do
        pidJson <- FO.lookup "presetId" obj
        Json.toString pidJson
  pure { presetId, engage }

parseEngageState :: String -> Maybe EngageState
parseEngageState = case _ of
  "on" -> Just EngageOn
  "off" -> Just EngageOff
  "a" -> Just EngageA
  "b" -> Just EngageB
  "no-change" -> Just EngageNoChange
  _ -> Nothing

-- Typed load functions

loadEngineState :: Effect (Maybe EngineState)
loadEngineState = do
  mAutosave <- getItem AutosaveKey
  case mAutosave >>= parseEngine of
    Just eng -> pure (Just eng)
    Nothing -> do
      mEngine <- getItem EngineKey
      pure (mEngine >>= parseEngine)

loadCardOrderParsed :: Effect (Array PedalId)
loadCardOrderParsed = do
  mStr <- getItem CardOrderKey
  pure $ fromMaybe (map _.meta.id Registry.pedals) (mStr >>= parseCardOrder)

loadPresetsParsed :: Effect (Array PedalPreset)
loadPresetsParsed = do
  mStr <- getItem PresetsKey
  pure $ fromMaybe [] (mStr >>= parsePresets)

loadBoardPresetsParsed :: Effect (Array BoardPreset)
loadBoardPresetsParsed = do
  mStr <- getItem BoardPresetsKey
  pure $ fromMaybe [] (mStr >>= parseBoardPresets)

-- Serializers

nowISO :: Effect String
nowISO = do
  d <- JSDate.now
  JSDate.toISOString d

engageStateToString :: EngageState -> String
engageStateToString = case _ of
  EngageOn -> "on"
  EngageOff -> "off"
  EngageA -> "a"
  EngageB -> "b"
  EngageNoChange -> "no-change"

presetToJson :: PedalPreset -> Json
presetToJson p =
  Json.fromObject $ FO.fromFoldable $
    [ Tuple "id" (Json.fromString p.id)
    , Tuple "pedalId" (let (PedalId pid) = p.pedalId in Json.fromString pid)
    , Tuple "name" (Json.fromString p.name)
    , Tuple "description" (Json.fromString p.description)
    , Tuple "notes" (Json.fromString p.notes)
    , Tuple "values" (valuesToJson p.values)
    , Tuple "info" (infoToJson p.info)
    , Tuple "created" (Json.fromString p.created)
    , Tuple "modified" (Json.fromString p.modified)
    ] <> case p.savedSlot of
      Nothing -> []
      Just slot -> [ Tuple "savedSlot" (Json.fromNumber (Int.toNumber (unProgramNumber slot))) ]

boardPresetEntryToJson :: BoardPresetEntry -> Json
boardPresetEntryToJson entry =
  Json.fromObject $ FO.fromFoldable $
    [ Tuple "engage" (Json.fromString (engageStateToString entry.engage))
    ] <> case entry.presetId of
      Nothing -> []
      Just pid -> [ Tuple "presetId" (Json.fromString pid) ]

boardPresetToJson :: BoardPreset -> Json
boardPresetToJson bp =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "id" (Json.fromString bp.id)
    , Tuple "name" (Json.fromString bp.name)
    , Tuple "description" (Json.fromString bp.description)
    , Tuple "notes" (Json.fromString bp.notes)
    , Tuple "pedals" (boardPedalsToJson bp.pedals)
    , Tuple "created" (Json.fromString bp.created)
    , Tuple "modified" (Json.fromString bp.modified)
    ]
  where
  boardPedalsToJson :: Map.Map PedalId BoardPresetEntry -> Json
  boardPedalsToJson pedals =
    Json.fromObject $ FO.fromFoldable $
      map (\(Tuple (PedalId pid) entry) -> Tuple pid (boardPresetEntryToJson entry))
        (Map.toUnfoldable pedals :: Array _)

presetsToJsonString :: Array PedalPreset -> String
presetsToJsonString = stringify <<< Json.fromArray <<< map presetToJson

boardPresetsToJsonString :: Array BoardPreset -> String
boardPresetsToJsonString = stringify <<< Json.fromArray <<< map boardPresetToJson
