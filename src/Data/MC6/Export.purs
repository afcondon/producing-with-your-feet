module Data.MC6.Export
  ( encodeMessage
  , encodePreset
  , encodeBank
  , encodeBackup
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.MC6.Message (emptyMessage)
import Data.MC6.Types (MC6Message, MC6Model, MC6NativeBank, MC6Preset, mc6ActionToInt, mc6ModelToInt, mc6MsgTypeToInt, mc6ToggleToInt)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

infixr 6 Tuple as /\

-- | Encode a message to MC6 backup JSON format
encodeMessage :: MC6Message -> Json
encodeMessage msg = fromObject $ Object.fromFoldable
  [ "d1" /\ fromNumber (toNumber msg.data1)
  , "d2" /\ fromNumber (toNumber msg.data2)
  , "d3" /\ fromNumber (toNumber msg.data3)
  , "d4" /\ fromNumber (toNumber msg.data4)
  , "m" /\ fromNumber (toNumber msg.msgIndex)
  , "c" /\ fromNumber (toNumber msg.channel)
  , "t" /\ fromNumber (toNumber (mc6MsgTypeToInt msg.msgType))
  , "a" /\ fromNumber (toNumber (mc6ActionToInt msg.action))
  , "tg" /\ fromNumber (toNumber (mc6ToggleToInt msg.togglePosition))
  , "mi" /\ fromString ""
  ]

-- | Encode a preset to MC6 backup JSON format
encodePreset :: Int -> MC6Preset -> Json
encodePreset bankNum p = fromObject $ Object.fromFoldable
  [ "presetNum" /\ fromNumber (toNumber p.presetNum)
  , "bankNum" /\ fromNumber (toNumber bankNum)
  , "isExp" /\ fromBoolean false
  , "shortName" /\ fromString p.shortName
  , "toggleName" /\ fromString p.toggleName
  , "longName" /\ fromString p.longName
  , "toToggle" /\ fromBoolean p.toToggle
  , "toBlink" /\ fromBoolean false
  , "toMsgScroll" /\ fromBoolean false
  , "toggleGroup" /\ fromNumber (toNumber p.toggleGroup)
  , "msgArray" /\ fromArray (map encodeMessage (padMessages p.messages))
  ]

-- | Encode a bank to MC6 backup JSON format
encodeBank :: MC6NativeBank -> Json
encodeBank b = fromObject $ Object.fromFoldable
  [ "bankNumber" /\ fromNumber (toNumber b.bankNumber)
  , "bankName" /\ fromString b.bankName
  , "bankClearToggle" /\ fromBoolean b.bankClearToggle
  , "bankMsgArray" /\ fromArray (map encodeMessage emptyBankMsgs)
  , "presetArray" /\ fromArray (map (encodePreset b.bankNumber) (padPresets b.presets))
  ]

-- | Encode a full backup from sparse bank definitions
encodeBackup :: MC6Model -> Array MC6NativeBank -> Json
encodeBackup model banks = fromObject $ Object.fromFoldable
  [ "schemaVersion" /\ fromNumber 1.0
  , "dumpType" /\ fromString "allBanks"
  , "deviceModel" /\ fromNumber (toNumber (mc6ModelToInt model))
  , "downloadDate" /\ fromString ""
  , "hash" /\ fromNumber 0.0
  , "data" /\ fromObject (Object.fromFoldable
      [ "bankArray" /\ fromArray (map encodeBank (padBanks banks))
      ])
  ]

-- | Pad messages to 16 slots
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
     then presets <> map emptyPreset (Array.range (Array.length presets) (11))
     else Array.take 12 presets

-- | Pad sparse banks to 30 (fill gaps with empty banks)
padBanks :: Array MC6NativeBank -> Array MC6NativeBank
padBanks banks = map (\n -> findOrEmpty n banks) (Array.range 0 29)
  where
  findOrEmpty :: Int -> Array MC6NativeBank -> MC6NativeBank
  findOrEmpty n bs = case Array.find (\b -> b.bankNumber == n) bs of
    Just b -> b
    Nothing -> emptyNativeBank n

emptyNativeBank :: Int -> MC6NativeBank
emptyNativeBank n =
  { bankNumber: n
  , bankName: ""
  , bankClearToggle: false
  , presets: map emptyPreset (Array.range 0 11)
  }

emptyPreset :: Int -> MC6Preset
emptyPreset n =
  { presetNum: n
  , shortName: ""
  , toggleName: ""
  , longName: ""
  , toToggle: false
  , toggleGroup: 0
  , messages: map emptyMessage (Array.range 0 15)
  }

-- | 16 empty bank-level messages (bankMsgArray)
emptyBankMsgs :: Array MC6Message
emptyBankMsgs = map emptyMessage (Array.range 0 15)
