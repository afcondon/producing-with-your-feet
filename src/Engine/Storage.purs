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
  , saveDeviceRead
  , loadDeviceRead
  , saveDumpedBanks
  , loadDumpedBanks
  , dumpedBanksToJsonString
  , parseDumpedBanks
  , loadMC6AssignmentsParsed
  , saveControlBanks
  , loadControlBanksParsed
  , saveGlobalSwitches
  , loadGlobalSwitchesParsed
  , loadLegacyOverrides
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
  , mc6AssignmentsToJsonString
  , hydrateFromSnapshot
  , snapshotToJsonString
  , StorageKey(..)
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (hush)
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch)
import Data.MC6.Global (GlobalSwitch)
import Data.MC6.Types (MC6Message, MC6NativeBank, MC6Preset, MC6TogglePosition(..), mc6MsgTypeToInt, intToMC6MsgType, mc6ActionToInt, intToMC6Action, mc6ToggleToInt)
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
  | MC6AssignmentsKey
  | ControlBanksKey
  | GlobalSwitchesKey
  | DeviceReadKey
  | DumpedBanksKey

keyString :: StorageKey -> String
keyString = case _ of
  EngineKey -> "pedal-explorer-engine"
  AutosaveKey -> "pedal-explorer-autosave"
  PresetsKey -> "pedal-explorer-presets"
  BoardPresetsKey -> "pedal-explorer-board-presets"
  CardOrderKey -> "pedal-explorer-card-order"
  MC6AssignmentsKey -> "pedal-explorer-mc6-assignments"
  ControlBanksKey -> "pedal-explorer-control-banks"
  -- Still the old string: renaming a storage key throws away the data it
  -- names, and the shape did not change when the concept did.
  GlobalSwitchesKey -> "pedal-explorer-shared-switches"
  DeviceReadKey -> "pedal-explorer-mc6-device-read"
  DumpedBanksKey -> "pedal-explorer-mc6-dumped-banks"

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

-- | What the MC6 last said about itself, kept across reloads.
-- |
-- | The timestamp is stored *with* the maps rather than beside them, because the
-- | two are one fact: these were the names at that moment. Anything that renders
-- | the maps can therefore say how old they are, which is the difference between
-- | a known-good baseline and a stale one wearing its clothes.
saveDeviceRead :: Map.Map Int String -> Map.Map Int (Array String) -> String -> Effect Unit
saveDeviceRead names switches readAt =
  setItem DeviceReadKey $ stringify $ Json.fromObject $ FO.fromFoldable
    [ Tuple "readAt" (Json.fromString readAt)
    , Tuple "names" (Json.fromObject (FO.fromFoldable
        (map (\(Tuple n v) -> Tuple (show n) (Json.fromString v))
          (Map.toUnfoldable names :: Array _))))
    , Tuple "switches" (Json.fromObject (FO.fromFoldable
        (map (\(Tuple n vs) -> Tuple (show n) (Json.fromArray (map Json.fromString vs)))
          (Map.toUnfoldable switches :: Array _))))
    ]

loadDeviceRead
  :: Effect (Maybe { names :: Map.Map Int String
                   , switches :: Map.Map Int (Array String)
                   , readAt :: String
                   })
loadDeviceRead = do
  mStr <- getItem DeviceReadKey
  pure do
    str <- mStr
    json <- hush (jsonParser str)
    obj <- Json.toObject json
    readAt <- FO.lookup "readAt" obj >>= Json.toString
    namesObj <- FO.lookup "names" obj >>= Json.toObject
    switchesObj <- FO.lookup "switches" obj >>= Json.toObject
    names <- traverse (\(Tuple k v) -> Tuple <$> Int.fromString k <*> Json.toString v)
               (FO.toUnfoldable namesObj :: Array _)
    switches <- traverse
                  (\(Tuple k v) -> Tuple <$> Int.fromString k
                                      <*> (Json.toArray v >>= traverse Json.toString))
                  (FO.toUnfoldable switchesObj :: Array _)
    pure { names: Map.fromFoldable names
         , switches: Map.fromFoldable switches
         , readAt
         }

-- | Every bank the last full dump returned, messages included.
-- |
-- | Kept separately from `saveDeviceRead` because it is a different order of
-- | claim and a much larger payload: names describe the device, messages
-- | reproduce it. A dump takes a minute of the hardware's time and four hundred
-- | and fifty frames, so losing it to a page reload would make the whole
-- | exercise a chore rather than a baseline.
saveDumpedBanks :: Array MC6NativeBank -> Effect Unit
saveDumpedBanks = setItem DumpedBanksKey <<< dumpedBanksToJsonString

loadDumpedBanks :: Effect (Array MC6NativeBank)
loadDumpedBanks = do
  mStr <- getItem DumpedBanksKey
  pure $ fromMaybe [] (mStr >>= parseDumpedBanks)

-- | Split from the effectful pair so the round trip can be tested. A lossy save
-- | here would be indistinguishable from a device that read badly.
dumpedBanksToJsonString :: Array MC6NativeBank -> String
dumpedBanksToJsonString = stringify <<< Json.fromArray <<< map nativeBankToJson

parseDumpedBanks :: String -> Maybe (Array MC6NativeBank)
parseDumpedBanks str = do
  json <- hush (jsonParser str)
  arr <- Json.toArray json
  traverse parseNativeBank arr

nativeBankToJson :: MC6NativeBank -> Json
nativeBankToJson nb =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "bankNumber" (Json.fromNumber (Int.toNumber nb.bankNumber))
    , Tuple "bankName" (Json.fromString nb.bankName)
    , Tuple "presets" (Json.fromArray (map nativePresetToJson nb.presets))
    ]

nativePresetToJson :: MC6Preset -> Json
nativePresetToJson p =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "presetNum" (Json.fromNumber (Int.toNumber p.presetNum))
    , Tuple "shortName" (Json.fromString p.shortName)
    , Tuple "toggleName" (Json.fromString p.toggleName)
    , Tuple "longName" (Json.fromString p.longName)
    , Tuple "toToggle" (Json.fromBoolean p.toToggle)
    , Tuple "toggleGroup" (Json.fromNumber (Int.toNumber p.toggleGroup))
    , Tuple "messages" (Json.fromArray (map mc6MessageToJson p.messages))
    ]

parseNativeBank :: Json -> Maybe MC6NativeBank
parseNativeBank json = do
  obj <- Json.toObject json
  bankNumber <- FO.lookup "bankNumber" obj >>= Json.toNumber >>= Int.fromNumber
  let bankName = fromMaybe "" (FO.lookup "bankName" obj >>= Json.toString)
  presetsJson <- FO.lookup "presets" obj >>= Json.toArray
  presets <- traverse parseNativePreset presetsJson
  pure { bankNumber, bankName, bankClearToggle: false, presets }

parseNativePreset :: Json -> Maybe MC6Preset
parseNativePreset json = do
  obj <- Json.toObject json
  presetNum <- FO.lookup "presetNum" obj >>= Json.toNumber >>= Int.fromNumber
  let str k = fromMaybe "" (FO.lookup k obj >>= Json.toString)
  messagesJson <- FO.lookup "messages" obj >>= Json.toArray
  messages <- traverse parseMC6Message messagesJson
  pure { presetNum
       , shortName: str "shortName"
       , toggleName: str "toggleName"
       , longName: str "longName"
       , toToggle: fromMaybe false (FO.lookup "toToggle" obj >>= Json.toBoolean)
       , toggleGroup: fromMaybe 0 (FO.lookup "toggleGroup" obj >>= Json.toNumber >>= Int.fromNumber)
       , messages
       }

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

saveGlobalSwitches :: Array GlobalSwitch -> Effect Unit
saveGlobalSwitches globals = do
  let json = Json.fromArray (map globalSwitchToJson globals)
  setItem GlobalSwitchesKey (stringify json)

loadGlobalSwitchesParsed :: Effect (Array GlobalSwitch)
loadGlobalSwitchesParsed = do
  mStr <- getItem GlobalSwitchesKey
  pure $ fromMaybe [] (mStr >>= parseGlobalSwitches)

parseGlobalSwitches :: String -> Maybe (Array GlobalSwitch)
parseGlobalSwitches str = do
  json <- hush (jsonParser str)
  arr <- Json.toArray json
  traverse parseGlobalSwitch arr

globalSwitchToJson :: GlobalSwitch -> Json
globalSwitchToJson s =
  Json.fromObject $ FO.fromFoldable
    [ Tuple "id" (Json.fromString s.id)
    , Tuple "slot" (Json.fromNumber (Int.toNumber s.slot))
    , Tuple "label" (Json.fromString s.label)
    , Tuple "longName" (Json.fromString s.longName)
    , Tuple "toToggle" (Json.fromBoolean s.toToggle)
    , Tuple "messages" (Json.fromArray (map mc6MessageToJson s.messages))
    ]

parseGlobalSwitch :: Json -> Maybe GlobalSwitch
parseGlobalSwitch json = do
  obj <- Json.toObject json
  id <- FO.lookup "id" obj >>= Json.toString
  slot <- FO.lookup "slot" obj >>= Json.toNumber >>= Int.fromNumber
  label <- FO.lookup "label" obj >>= Json.toString
  let longName = fromMaybe "" (FO.lookup "longName" obj >>= Json.toString)
      toToggle = fromMaybe false (FO.lookup "toToggle" obj >>= Json.toBoolean)
  messagesJson <- FO.lookup "messages" obj >>= Json.toArray
  messages <- traverse parseMC6Message messagesJson
  pure { id, slot, label, longName, toToggle, messages }

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

-- | The per-page override lists left in the store by the shared-switch era.
-- |
-- | Read separately from the banks, and only once at load, because a bank no
-- | longer has anywhere to put them — `Global.retireOverrides` consumes this
-- | array and the next save drops the field for good. Parallel to the bank
-- | array by position, so an unparseable store yields `[]` and the reconciler
-- | correctly concludes nothing was ever refused.
loadLegacyOverrides :: Effect (Array (Array Int))
loadLegacyOverrides = do
  mStr <- getItem ControlBanksKey
  pure $ fromMaybe [] do
    str <- mStr
    json <- hush (jsonParser str)
    arr <- Json.toArray json
    pure $ map overridesOf arr
  where
  overridesOf json = fromMaybe [] do
    obj <- Json.toObject json
    slots <- FO.lookup "sharedOverrides" obj >>= Json.toArray
    traverse (\j -> Json.toNumber j >>= Int.fromNumber) slots

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

-- Remote store <-> cache
--
-- localStorage is no longer the system of record; pwyf-store is. These two
-- functions are the bridge, and they are deliberately the only place that
-- knows it: everything downstream still reads the same cache keys it always
-- did, so a store that cannot be reached simply leaves the last good copy in
-- place and the app carries on.

mc6AssignmentsToJsonString :: Array MC6Assignment -> String
mc6AssignmentsToJsonString =
  stringify <<< Json.fromArray <<< map mc6AssignmentToJson

-- | The document the store expects: whole collections, keyed as it keys them.
snapshotToJsonString
  :: Array PedalPreset
  -> Array BoardPreset
  -> Array ControlBank
  -> Array MC6Assignment
  -> String
snapshotToJsonString presets boards banks assignments =
  stringify $ Json.fromObject $ FO.fromFoldable
    [ Tuple "presets" (Json.fromArray (map presetToJson presets))
    , Tuple "patches" (Json.fromArray (map boardPresetToJson boards))
    , Tuple "banks" (Json.fromArray (map controlBankToJson banks))
    , Tuple "assignments" (Json.fromArray (map mc6AssignmentToJson assignments))
    ]

-- | Fill the cache from a snapshot fetched off the store.
-- |
-- | Returns false if the payload could not be read at all, so the caller can
-- | say so rather than silently continuing on stale data.
-- |
-- | Two collections are skipped rather than written, mirroring the guards
-- | `Store.replaceAll` puts on the write side — a reconciling *read* is one bug
-- | away from being an erase in exactly the same way:
-- |
-- |   * ABSENT means "no change". The store says nothing about that collection.
-- |   * EMPTY also means "no change", because the alternative is that the first
-- |     load against a store which has never held this collection silently
-- |     replaces good local work with nothing. That is not hypothetical: the
-- |     `banks` collection did not exist until control banks moved into the
-- |     store, so every existing browser had pages the store had never seen.
-- |
-- | The cost is that a collection emptied deliberately elsewhere will not empty
-- | here until this client next pushes. That is the right way round: a stale
-- | extra bank is a nuisance, a deleted one is lost work.
hydrateFromSnapshot :: String -> Effect Boolean
hydrateFromSnapshot raw = case hush (jsonParser raw) >>= Json.toObject of
  Nothing -> pure false
  Just o -> do
    write "presets" PresetsKey o
    write "patches" BoardPresetsKey o
    write "banks" ControlBanksKey o
    write "assignments" MC6AssignmentsKey o
    pure true
  where
  write key storageKey o = case FO.lookup key o of
    Nothing -> pure unit
    Just j
      | isEmptyArray j -> pure unit
      | otherwise -> setItem storageKey (stringify j)

  isEmptyArray j = case Json.toArray j of
    Just xs -> Array.null xs
    Nothing -> false
