module Data.MC6.Backup
  ( MC6Backup
  , MC6BackupData
  , MC6FullBank
  , MC6BackupPreset
  , decodeBackup
  , encodeBackup
  , mergeBanks
  , roundtripCheck
  ) where

import Prelude

import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut (JsonDecodeError(..))
import Data.Codec.Argonaut.Record as CAR
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Int as Int
import Data.MC6.Message (emptyMessage)
import Data.MC6.Types (MC6Message, MC6NativeBank, MC6Preset, MC6TogglePosition(..), intToMC6Action, intToMC6MsgType, mc6ActionToInt, mc6MsgTypeToInt, mc6ToggleToInt)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

infixr 6 Tuple as /\

-- Types -----------------------------------------------------------------------

-- | Full backup as stored in MC6 Editor JSON files.
-- | Field `backupData` maps to JSON key "data".
type MC6Backup =
  { schemaVersion :: Int
  , dumpType :: String
  , deviceModel :: Int
  , description :: String
  , downloadDate :: String
  , hash :: Number
  , backupData :: MC6BackupData
  }

-- | Backup data section.
-- | Field `controllerSettings` maps to JSON key "controller_settings" and is
-- | preserved verbatim as raw Json for perfect roundtrip fidelity.
type MC6BackupData =
  { bankArray :: Array MC6FullBank
  , controllerSettings :: J.Json
  }

-- | Bank as stored in backup (superset of config MC6NativeBank).
type MC6FullBank =
  { bankNumber :: Int
  , bankName :: String
  , bankClearToggle :: Boolean
  , presetArray :: Array MC6BackupPreset
  , bankMsgArray :: Array MC6Message
  , expPresetArray :: Array MC6BackupPreset
  }

-- | Preset as stored in backup (superset of config MC6Preset).
type MC6BackupPreset =
  { presetNum :: Int
  , bankNum :: Int
  , isExp :: Boolean
  , shortName :: String
  , toggleName :: String
  , longName :: String
  , toToggle :: Boolean
  , toBlink :: Boolean
  , toMsgScroll :: Boolean
  , toggleGroup :: Int
  , msgArray :: Array MC6Message
  }

-- Codecs (internal) -----------------------------------------------------------

intToToggle :: Int -> MC6TogglePosition
intToToggle = case _ of
  0 -> ToggleOff
  1 -> ToggleOn
  _ -> ToggleBoth

-- | Codec for MC6 messages. JSON uses abbreviated keys (d1, d2, t, a, tg, etc.)
-- | that don't match PureScript field names, so we use a manual codec.
messageCodec :: CA.JsonCodec MC6Message
messageCodec = CA.codec' decodeMsg encodeMsg
  where
  decodeMsg :: J.Json -> Either JsonDecodeError MC6Message
  decodeMsg j = case J.toObject j of
    Nothing -> Left (TypeMismatch "Object")
    Just obj -> do
      d1 <- getInt "d1" obj
      d2 <- getInt "d2" obj
      d3 <- getInt "d3" obj
      d4 <- getInt "d4" obj
      m  <- getInt "m" obj
      c  <- getInt "c" obj
      t  <- getInt "t" obj
      a  <- getInt "a" obj
      tg <- getInt "tg" obj
      pure
        { msgType: intToMC6MsgType t
        , channel: c
        , data1: d1
        , data2: d2
        , data3: d3
        , data4: d4
        , action: intToMC6Action a
        , togglePosition: intToToggle tg
        , msgIndex: m
        }

  getInt :: String -> Object J.Json -> Either JsonDecodeError Int
  getInt key obj = case Object.lookup key obj of
    Nothing -> Left (AtKey key MissingValue)
    Just jv -> case J.toNumber jv >>= Int.fromNumber of
      Nothing -> Left (AtKey key (TypeMismatch "Int"))
      Just n -> Right n

  encodeMsg :: MC6Message -> J.Json
  encodeMsg msg = J.fromObject $ Object.fromFoldable
    [ "d1" /\ J.fromNumber (Int.toNumber msg.data1)
    , "d2" /\ J.fromNumber (Int.toNumber msg.data2)
    , "d3" /\ J.fromNumber (Int.toNumber msg.data3)
    , "d4" /\ J.fromNumber (Int.toNumber msg.data4)
    , "m"  /\ J.fromNumber (Int.toNumber msg.msgIndex)
    , "c"  /\ J.fromNumber (Int.toNumber msg.channel)
    , "t"  /\ J.fromNumber (Int.toNumber (mc6MsgTypeToInt msg.msgType))
    , "a"  /\ J.fromNumber (Int.toNumber (mc6ActionToInt msg.action))
    , "tg" /\ J.fromNumber (Int.toNumber (mc6ToggleToInt msg.togglePosition))
    , "mi" /\ J.fromString ""
    ]

-- | Codec for backup presets — all field names match JSON keys.
backupPresetCodec :: CA.JsonCodec MC6BackupPreset
backupPresetCodec = CAR.object "MC6BackupPreset"
  { presetNum: CA.int
  , bankNum: CA.int
  , isExp: CA.boolean
  , shortName: CA.string
  , toggleName: CA.string
  , longName: CA.string
  , toToggle: CA.boolean
  , toBlink: CA.boolean
  , toMsgScroll: CA.boolean
  , toggleGroup: CA.int
  , msgArray: CA.array messageCodec
  }

-- | Codec for full banks — all field names match JSON keys.
fullBankCodec :: CA.JsonCodec MC6FullBank
fullBankCodec = CAR.object "MC6FullBank"
  { bankNumber: CA.int
  , bankName: CA.string
  , bankClearToggle: CA.boolean
  , presetArray: CA.array backupPresetCodec
  , bankMsgArray: CA.array messageCodec
  , expPresetArray: CA.array backupPresetCodec
  }

-- Public API ------------------------------------------------------------------

-- | Parse a complete MC6 Editor backup from Json.
decodeBackup :: J.Json -> Either String MC6Backup
decodeBackup json = case J.toObject json of
  Nothing -> Left "Expected JSON object"
  Just obj -> do
    schemaVersion <- lookupInt "schemaVersion" obj
    dumpType      <- lookupStr "dumpType" obj
    deviceModel   <- lookupInt "deviceModel" obj
    description   <- lookupStr "description" obj
    downloadDate  <- lookupStr "downloadDate" obj
    hash          <- lookupNum "hash" obj
    dataJson      <- note "Missing field: data" $ Object.lookup "data" obj
    backupData    <- decodeBackupData dataJson
    pure { schemaVersion, dumpType, deviceModel, description, downloadDate, hash, backupData }

-- | Serialize a complete MC6 backup to Json.
-- | Key order matches real MC6 Editor output.
encodeBackup :: MC6Backup -> J.Json
encodeBackup b = J.fromObject $ Object.fromFoldable
  [ "schemaVersion" /\ J.fromNumber (Int.toNumber b.schemaVersion)
  , "dumpType"      /\ J.fromString b.dumpType
  , "deviceModel"   /\ J.fromNumber (Int.toNumber b.deviceModel)
  , "downloadDate"  /\ J.fromString b.downloadDate
  , "hash"          /\ J.fromNumber b.hash
  , "data"          /\ encodeBackupData b.backupData
  , "description"   /\ J.fromString b.description
  ]

-- | Merge config banks into a real backup, preserving all other data.
-- | For each config bank, replaces the matching bank (by bankNumber) in the
-- | backup's bankArray. Preserves bankMsgArray, expPresetArray, and all
-- | non-matching banks and controller_settings untouched.
mergeBanks :: Array MC6NativeBank -> MC6Backup -> MC6Backup
mergeBanks configBanks backup =
  backup { backupData = backup.backupData { bankArray = map mergeBank backup.backupData.bankArray } }
  where
  mergeBank :: MC6FullBank -> MC6FullBank
  mergeBank existing = case Array.find (\b -> b.bankNumber == existing.bankNumber) configBanks of
    Nothing -> existing
    Just config -> existing
      { bankName = config.bankName
      , bankClearToggle = config.bankClearToggle
      , presetArray = padBackupPresets existing.bankNumber
          (map (configPresetToBackup existing.bankNumber) config.presets)
      }

-- | Golden test: parse then re-encode and compare. True if roundtrip is lossless.
roundtripCheck :: J.Json -> Boolean
roundtripCheck json = case decodeBackup json of
  Left _ -> false
  Right backup -> jsonEq json (encodeBackup backup)

-- Internal helpers ------------------------------------------------------------

decodeBackupData :: J.Json -> Either String MC6BackupData
decodeBackupData json = case J.toObject json of
  Nothing -> Left "Expected JSON object for backup data"
  Just obj -> do
    bankArrayJson <- note "Missing field: bankArray" $ Object.lookup "bankArray" obj
    bankArray <- lmap CA.printJsonDecodeError $ CA.decode (CA.array fullBankCodec) bankArrayJson
    controllerSettings <- note "Missing field: controller_settings" $ Object.lookup "controller_settings" obj
    pure { bankArray, controllerSettings }

encodeBackupData :: MC6BackupData -> J.Json
encodeBackupData d = J.fromObject $ Object.fromFoldable
  [ "bankArray"           /\ CA.encode (CA.array fullBankCodec) d.bankArray
  , "controller_settings" /\ d.controllerSettings
  ]

configPresetToBackup :: Int -> MC6Preset -> MC6BackupPreset
configPresetToBackup bankNum p =
  { presetNum: p.presetNum
  , bankNum
  , isExp: false
  , shortName: p.shortName
  , toggleName: p.toggleName
  , longName: p.longName
  , toToggle: p.toToggle
  , toBlink: false
  , toMsgScroll: false
  , toggleGroup: p.toggleGroup
  , msgArray: padMessages p.messages
  }

padBackupPresets :: Int -> Array MC6BackupPreset -> Array MC6BackupPreset
padBackupPresets bankNum presets =
  let padCount = 12 - Array.length presets
  in if padCount > 0
     then presets <> map (emptyBackupPreset bankNum) (Array.range (Array.length presets) 11)
     else Array.take 12 presets

emptyBackupPreset :: Int -> Int -> MC6BackupPreset
emptyBackupPreset bankNum presetNum =
  { presetNum
  , bankNum
  , isExp: false
  , shortName: ""
  , toggleName: ""
  , longName: ""
  , toToggle: false
  , toBlink: false
  , toMsgScroll: false
  , toggleGroup: 0
  , msgArray: map emptyMessage (Array.range 0 15)
  }

padMessages :: Array MC6Message -> Array MC6Message
padMessages msgs =
  let indexed = Array.mapWithIndex (\i m -> m { msgIndex = i }) msgs
      padCount = 16 - Array.length indexed
  in if padCount > 0
     then indexed <> map (\i -> emptyMessage (Array.length indexed + i)) (Array.range 0 (padCount - 1))
     else Array.take 16 indexed

-- JSON lookup helpers (return Either String for top-level decode)

lookupInt :: String -> Object J.Json -> Either String Int
lookupInt key obj = case Object.lookup key obj of
  Nothing -> Left $ "Missing field: " <> key
  Just j -> case J.toNumber j >>= Int.fromNumber of
    Nothing -> Left $ "Expected integer at field: " <> key
    Just n -> Right n

lookupNum :: String -> Object J.Json -> Either String Number
lookupNum key obj = case Object.lookup key obj of
  Nothing -> Left $ "Missing field: " <> key
  Just j -> case J.toNumber j of
    Nothing -> Left $ "Expected number at field: " <> key
    Just n -> Right n

lookupStr :: String -> Object J.Json -> Either String String
lookupStr key obj = case Object.lookup key obj of
  Nothing -> Left $ "Missing field: " <> key
  Just j -> case J.toString j of
    Nothing -> Left $ "Expected string at field: " <> key
    Just s -> Right s

note :: forall a b. a -> Maybe b -> Either a b
note a = case _ of
  Nothing -> Left a
  Just b -> Right b

-- Deep structural JSON comparison (key-order independent for objects)

jsonEq :: J.Json -> J.Json -> Boolean
jsonEq a b = case J.toNull a, J.toNull b of
  Just _, Just _ -> true
  _, _ -> case J.toBoolean a, J.toBoolean b of
    Just x, Just y -> x == y
    _, _ -> case J.toNumber a, J.toNumber b of
      Just x, Just y -> x == y
      _, _ -> case J.toString a, J.toString b of
        Just x, Just y -> x == y
        _, _ -> case J.toArray a, J.toArray b of
          Just xs, Just ys ->
            Array.length xs == Array.length ys
              && all identity (Array.zipWith jsonEq xs ys)
          _, _ -> case J.toObject a, J.toObject b of
            Just oa, Just ob ->
              let ka = Array.sort (Object.keys oa)
                  kb = Array.sort (Object.keys ob)
              in ka == kb
                && all
                    (\k -> case Object.lookup k oa, Object.lookup k ob of
                      Just va, Just vb -> jsonEq va vb
                      _, _ -> false)
                    ka
            _, _ -> false
