module Config.Decode.Controller
  ( ControllerConfig
  , decodeController
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Int as Int
import Data.MC6.Message (emptyMessage)
import Data.MC6.Types (MC6Action(..), MC6Message, MC6Model, MC6MsgType(..), MC6NativeBank, MC6Preset, MC6TogglePosition(..), mc6ActionFromString, mc6ModelFromString, mc6MsgTypeFromString, mc6ToggleFromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Foreign.Object as FO

type ControllerConfig =
  { deviceModel :: MC6Model
  , mc6Channel :: Int
  , banks :: Array MC6NativeBank
  }

lookupStr :: String -> FO.Object Json -> Maybe String
lookupStr key obj = FO.lookup key obj >>= Json.toString

lookupNum :: String -> FO.Object Json -> Maybe Int
lookupNum key obj = do
  j <- FO.lookup key obj
  n <- Json.toNumber j
  Int.fromNumber n

lookupBool :: String -> FO.Object Json -> Maybe Boolean
lookupBool key obj = FO.lookup key obj >>= Json.toBoolean

decodeController :: Json -> Maybe ControllerConfig
decodeController json = do
  obj <- Json.toObject json
  modelStr <- lookupStr "deviceModel" obj
  deviceModel <- mc6ModelFromString modelStr
  let mc6Channel = fromMaybe 1 (lookupNum "mc6Channel" obj)
  banksJson <- FO.lookup "banks" obj >>= Json.toArray
  banks <- traverse decodeBank banksJson
  Just { deviceModel, mc6Channel, banks }

decodeBank :: Json -> Maybe MC6NativeBank
decodeBank json = do
  obj <- Json.toObject json
  bankName <- lookupStr "name" obj
  bankNumber <- lookupNum "bankNumber" obj
  let bankClearToggle = fromMaybe false (lookupBool "clearToggle" obj)
  presetsJson <- FO.lookup "presets" obj >>= Json.toArray
  rawPresets <- traverse decodePreset (Array.mapWithIndex (\i j -> { idx: i, json: j }) presetsJson)
  let presets = padPresets rawPresets
  Just { bankNumber, bankName, bankClearToggle, presets }

decodePreset :: { idx :: Int, json :: Json } -> Maybe MC6Preset
decodePreset { idx, json } = do
  obj <- Json.toObject json
  shortName <- lookupStr "shortName" obj
  let toggleName = fromMaybe "" (lookupStr "toggleName" obj)
      longName = fromMaybe shortName (lookupStr "longName" obj)
      toToggle = fromMaybe false (lookupBool "toggle" obj)
      toggleGroup = fromMaybe 0 (lookupNum "toggleGroup" obj)
  messagesJson <- FO.lookup "messages" obj >>= Json.toArray
  rawMsgs <- traverse decodeMessage messagesJson
  let messages = padMessages rawMsgs
  Just { presetNum: idx, shortName, toggleName, longName, toToggle, toggleGroup, messages }

decodeMessage :: Json -> Maybe MC6Message
decodeMessage json = do
  obj <- Json.toObject json
  typeStr <- lookupStr "type" obj
  msgType <- mc6MsgTypeFromString typeStr
  actionStr <- lookupStr "action" obj
  action <- mc6ActionFromString actionStr
  let toggleStr = fromMaybe "both" (lookupStr "toggle" obj)
      togglePosition = mc6ToggleFromString toggleStr
  decodeMessageByType msgType obj action togglePosition

decodeMessageByType :: MC6MsgType -> FO.Object Json -> MC6Action -> MC6TogglePosition -> Maybe MC6Message
decodeMessageByType msgType obj action togglePosition = case msgType of
  MsgCC -> do
    channel <- lookupNum "channel" obj
    cc <- lookupNum "cc" obj
    value <- lookupNum "value" obj
    Just { msgType, channel, data1: cc, data2: value, data3: 0, data4: 0, action, togglePosition, msgIndex: 0 }

  MsgPC -> do
    channel <- lookupNum "channel" obj
    program <- lookupNum "program" obj
    Just { msgType, channel, data1: program, data2: 0, data3: 0, data4: 0, action, togglePosition, msgIndex: 0 }

  MsgBankJump -> do
    bank <- lookupNum "bank" obj
    Just { msgType, channel: 1, data1: bank, data2: 0, data3: 0, data4: 0, action, togglePosition, msgIndex: 0 }

  MsgDelay -> do
    value <- lookupNum "value" obj
    Just { msgType, channel: 1, data1: value, data2: 0, data3: 0, data4: 0, action: ActionNone, togglePosition: ToggleBoth, msgIndex: 0 }

  MsgEngagePreset -> do
    preset <- lookupNum "preset" obj
    Just { msgType, channel: 1, data1: preset, data2: 0, data3: 0, data4: 0, action, togglePosition, msgIndex: 0 }

  MsgEmpty ->
    Just { msgType: MsgEmpty, channel: 1, data1: 0, data2: 0, data3: 0, data4: 0, action: ActionNone, togglePosition: ToggleBoth, msgIndex: 0 }

  _ -> Nothing

-- | Pad messages to 16 with empty slots, assigning sequential msgIndex values
padMessages :: Array MC6Message -> Array MC6Message
padMessages msgs =
  let indexed = Array.mapWithIndex (\i m -> m { msgIndex = i }) msgs
      padCount = 16 - Array.length indexed
  in if padCount > 0
     then indexed <> map (\i -> emptyMessage (Array.length indexed + i)) (Array.range 0 (padCount - 1))
     else Array.take 16 indexed

-- | Pad presets to 12 per bank
padPresets :: Array MC6Preset -> Array MC6Preset
padPresets presets =
  let padCount = 12 - Array.length presets
  in if padCount > 0
     then presets <> map emptyPresetAt (Array.range (Array.length presets) 11)
     else Array.take 12 presets
  where
  emptyPresetAt :: Int -> MC6Preset
  emptyPresetAt n =
    { presetNum: n
    , shortName: ""
    , toggleName: ""
    , longName: ""
    , toToggle: false
    , toggleGroup: 0
    , messages: map emptyMessage (Array.range 0 15)
    }
