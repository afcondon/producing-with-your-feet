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
  , saveMC6Assignments
  , loadMC6AssignmentsParsed
  , saveControlBanks
  , loadControlBanksParsed
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
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch)
import Data.MC6.Types (MC6Message, MC6TogglePosition(..), mc6MsgTypeToInt, intToMC6MsgType, mc6ActionToInt, intToMC6Action, mc6ToggleToInt)
import Data.Midi (CC, MidiValue, makeCC, makeMidiValue, makeProgramNumber, unCC, unMidiValue, unProgramNumber)
import Data.Pedal (PedalId(..))
import Data.Pedal.Engage (EngageState(..))
import Data.Preset (BoardPreset, BoardPresetEntry, PedalPreset)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Engine (EngineState, MC6Assignment, PedalState)
import Foreign.Object as FO
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
  | MC6AssignmentsKey
  | ControlBanksKey

keyString :: StorageKey -> String
keyString = case _ of
  EngineKey -> "pedal-explorer-engine"
  AutosaveKey -> "pedal-explorer-autosave"
  PresetsKey -> "pedal-explorer-presets"
  BoardPresetsKey -> "pedal-explorer-board-presets"
  CardOrderKey -> "pedal-explorer-card-order"
  LoopyChannelKey -> "pedal-explorer-loopy-channel"
  MC6AssignmentsKey -> "pedal-explorer-mc6-assignments"
  ControlBanksKey -> "pedal-explorer-control-banks"

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

saveMC6Assignments :: Array MC6Assignment -> Effect Unit
saveMC6Assignments assignments = do
  let json = Json.fromArray $ map mc6AssignmentToJson assignments
  setItem MC6AssignmentsKey (stringify json)

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
    -- Support both plain number ("14": 64) and readable object ("14": { "value": 64, "label": "Mix" })
    intVal <- case Json.toNumber valJson of
      Just numVal -> Int.fromNumber numVal
      Nothing -> do
        obj <- Json.toObject valJson
        vJson <- FO.lookup "value" obj
        numVal <- Json.toNumber vJson
        Int.fromNumber numVal
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
  -- Support both "pedalId" (internal) and "pedal" (readable format)
  pedalId <- PedalId <$> case FO.lookup "pedalId" obj of
    Just pj -> Json.toString pj
    Nothing -> FO.lookup "pedal" obj >>= Json.toString
  nameJson <- FO.lookup "name" obj
  name <- Json.toString nameJson
  descJson <- FO.lookup "description" obj
  description <- Json.toString descJson
  let notes = fromMaybe "" (FO.lookup "notes" obj >>= Json.toString)
  valuesJson <- FO.lookup "values" obj
  values <- parseValues valuesJson
  let info = case FO.lookup "info" obj of
        Just iJson -> fromMaybe Map.empty (parseInfo iJson)
        Nothing -> Map.empty
  -- Support both "savedSlot" (internal) and "slot" (readable format)
  let savedSlot = do
        slotJson <- case FO.lookup "savedSlot" obj of
          Just sj -> Just sj
          Nothing -> FO.lookup "slot" obj
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
  -- Support both "presetId" (internal) and "preset" (readable format)
  let presetId = case FO.lookup "presetId" obj of
        Just pidJson -> Json.toString pidJson
        Nothing -> FO.lookup "preset" obj >>= Json.toString
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

loadCardOrderParsed :: Array PedalId -> Effect (Array PedalId)
loadCardOrderParsed defaultOrder = do
  mStr <- getItem CardOrderKey
  pure $ fromMaybe defaultOrder (mStr >>= parseCardOrder)

loadPresetsParsed :: Effect (Array PedalPreset)
loadPresetsParsed = do
  mStr <- getItem PresetsKey
  pure $ fromMaybe [] (mStr >>= parsePresets)

loadBoardPresetsParsed :: Effect (Array BoardPreset)
loadBoardPresetsParsed = do
  mStr <- getItem BoardPresetsKey
  pure $ fromMaybe [] (mStr >>= parseBoardPresets)

loadMC6AssignmentsParsed :: Effect (Array MC6Assignment)
loadMC6AssignmentsParsed = do
  mStr <- getItem MC6AssignmentsKey
  pure $ fromMaybe [] (mStr >>= parseMC6Assignments)

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

-- MC6 Assignment serialization

mc6AssignmentToJson :: MC6Assignment -> Json
mc6AssignmentToJson a =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "bankNumber" (Json.fromNumber (Int.toNumber a.bankNumber))
    , Tuple "switchIndex" (Json.fromNumber (Int.toNumber a.switchIndex))
    , Tuple "boardPresetId" (Json.fromString a.boardPresetId)
    ]

parseMC6Assignments :: String -> Maybe (Array MC6Assignment)
parseMC6Assignments str = do
  json <- hush (jsonParser str)
  arr <- Json.toArray json
  traverse parseMC6Assignment arr

parseMC6Assignment :: Json -> Maybe MC6Assignment
parseMC6Assignment json = do
  obj <- Json.toObject json
  bnJson <- FO.lookup "bankNumber" obj
  bankNumber <- Json.toNumber bnJson >>= Int.fromNumber
  siJson <- FO.lookup "switchIndex" obj
  switchIndex <- Json.toNumber siJson >>= Int.fromNumber
  bpJson <- FO.lookup "boardPresetId" obj
  boardPresetId <- Json.toString bpJson
  pure { bankNumber, switchIndex, boardPresetId }

-- Control Bank serialization

saveControlBanks :: Array ControlBank -> Effect Unit
saveControlBanks banks = do
  let json = Json.fromArray (map controlBankToJson banks)
  setItem ControlBanksKey (stringify json)

loadControlBanksParsed :: Array ControlBank -> Effect (Array ControlBank)
loadControlBanksParsed defaults = do
  mStr <- getItem ControlBanksKey
  pure $ fromMaybe defaults (mStr >>= parseControlBanks)

parseControlBanks :: String -> Maybe (Array ControlBank)
parseControlBanks str = do
  json <- hush (jsonParser str)
  arr <- Json.toArray json
  traverse parseControlBank arr

controlBankToJson :: ControlBank -> Json
controlBankToJson cb =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "id" (Json.fromString cb.id)
    , Tuple "name" (Json.fromString cb.name)
    , Tuple "description" (Json.fromString cb.description)
    , Tuple "mc6BankNumber" (Json.fromNumber (Int.toNumber cb.mc6BankNumber))
    , Tuple "returnSwitchIndex" (Json.fromNumber (Int.toNumber cb.returnSwitchIndex))
    , Tuple "switches" (Json.fromArray (map controlBankSwitchToJson cb.switches))
    ]

controlBankSwitchToJson :: ControlBankSwitch -> Json
controlBankSwitchToJson sw =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "label" (Json.fromString sw.label)
    , Tuple "longName" (Json.fromString sw.longName)
    , Tuple "toToggle" (Json.fromBoolean sw.toToggle)
    , Tuple "messages" (Json.fromArray (map mc6MessageToJson sw.messages))
    ]

mc6MessageToJson :: MC6Message -> Json
mc6MessageToJson msg =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "t" (Json.fromNumber (Int.toNumber (mc6MsgTypeToInt msg.msgType)))
    , Tuple "c" (Json.fromNumber (Int.toNumber msg.channel))
    , Tuple "d1" (Json.fromNumber (Int.toNumber msg.data1))
    , Tuple "d2" (Json.fromNumber (Int.toNumber msg.data2))
    , Tuple "d3" (Json.fromNumber (Int.toNumber msg.data3))
    , Tuple "d4" (Json.fromNumber (Int.toNumber msg.data4))
    , Tuple "a" (Json.fromNumber (Int.toNumber (mc6ActionToInt msg.action)))
    , Tuple "tg" (Json.fromNumber (Int.toNumber (mc6ToggleToInt msg.togglePosition)))
    , Tuple "m" (Json.fromNumber (Int.toNumber msg.msgIndex))
    ]

parseControlBank :: Json -> Maybe ControlBank
parseControlBank json = do
  obj <- Json.toObject json
  id <- FO.lookup "id" obj >>= Json.toString
  name <- FO.lookup "name" obj >>= Json.toString
  description <- pure $ fromMaybe "" (FO.lookup "description" obj >>= Json.toString)
  mc6BankNumber <- FO.lookup "mc6BankNumber" obj >>= Json.toNumber >>= Int.fromNumber
  returnSwitchIndex <- FO.lookup "returnSwitchIndex" obj >>= Json.toNumber >>= Int.fromNumber
  switchesJson <- FO.lookup "switches" obj >>= Json.toArray
  switches <- traverse parseControlBankSwitch switchesJson
  pure { id, name, description, mc6BankNumber, returnSwitchIndex, switches }

parseControlBankSwitch :: Json -> Maybe ControlBankSwitch
parseControlBankSwitch json = do
  obj <- Json.toObject json
  label <- FO.lookup "label" obj >>= Json.toString
  longName <- pure $ fromMaybe "" (FO.lookup "longName" obj >>= Json.toString)
  toToggle <- pure $ fromMaybe false (FO.lookup "toToggle" obj >>= Json.toBoolean)
  messagesJson <- FO.lookup "messages" obj >>= Json.toArray
  messages <- traverse parseMC6Message messagesJson
  pure { label, longName, toToggle, messages }

parseMC6Message :: Json -> Maybe MC6Message
parseMC6Message json = do
  obj <- Json.toObject json
  t <- FO.lookup "t" obj >>= Json.toNumber >>= Int.fromNumber
  c <- FO.lookup "c" obj >>= Json.toNumber >>= Int.fromNumber
  d1 <- FO.lookup "d1" obj >>= Json.toNumber >>= Int.fromNumber
  d2 <- FO.lookup "d2" obj >>= Json.toNumber >>= Int.fromNumber
  d3 <- pure $ fromMaybe 0 (FO.lookup "d3" obj >>= Json.toNumber >>= Int.fromNumber)
  d4 <- pure $ fromMaybe 0 (FO.lookup "d4" obj >>= Json.toNumber >>= Int.fromNumber)
  a <- FO.lookup "a" obj >>= Json.toNumber >>= Int.fromNumber
  tg <- pure $ fromMaybe 2 (FO.lookup "tg" obj >>= Json.toNumber >>= Int.fromNumber)
  m <- pure $ fromMaybe 0 (FO.lookup "m" obj >>= Json.toNumber >>= Int.fromNumber)
  pure { msgType: intToMC6MsgType t, channel: c, data1: d1, data2: d2, data3: d3, data4: d4
       , action: intToMC6Action a, togglePosition: intToToggle tg, msgIndex: m }
  where
  intToToggle = case _ of
    0 -> ToggleOff
    1 -> ToggleOn
    _ -> ToggleBoth
