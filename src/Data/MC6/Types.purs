module Data.MC6.Types
  ( MC6MsgType(..)
  , MC6Action(..)
  , MC6TogglePosition(..)
  , MC6Model(..)
  , MC6Message
  , MC6Preset
  , MC6NativeBank
  , mc6MsgTypeToInt
  , intToMC6MsgType
  , mc6MsgTypeFromString
  , mc6ActionToInt
  , intToMC6Action
  , mc6ActionFromString
  , mc6ToggleToInt
  , mc6ToggleFromString
  , mc6ModelToInt
  , mc6ModelFromString
  ) where

import Prelude

import Data.Maybe (Maybe(..))

-- | MC6 message type enum — maps to 't' field in backup JSON
data MC6MsgType
  = MsgEmpty              -- 0
  | MsgPC                 -- 1
  | MsgCC                 -- 2
  | MsgNote               -- 3
  | MsgRealtime           -- 4
  | MsgSysEx              -- 5
  | MsgBankUp             -- 6
  | MsgBankDown           -- 7
  | MsgTogglePage         -- 8
  | MsgMidiClock          -- 9
  | MsgDelay              -- 10
  | MsgSetToggle          -- 11
  | MsgEngagePreset       -- 12
  | MsgBankJump           -- 13
  | MsgTriggerMessages    -- 14
  | MsgCCWaveform         -- 15
  | MsgSequencer          -- 16
  | MsgCCScroll           -- 17
  | MsgPCScroll           -- 18
  | MsgStrymonBankUp      -- 19
  | MsgStrymonBankDown    -- 20
  | MsgUtility            -- 21
  | MsgLooperMode         -- 22
  | MsgTogglePreset       -- 23
  | MsgMidiThru           -- 24
  | MsgPCMultiChannel     -- 25
  | MsgMultiEngage        -- 26
  | MsgPresetRename       -- 27
  | MsgRelaySwitch        -- 28
  | MsgML10X              -- 29
  | MsgAxeFXTuner         -- 30
  | MsgKemperTuner        -- 31
  | MsgFractal            -- 32
  | MsgKeystroke          -- 33
  | MsgSongPosition       -- 34
  | MsgSongSelect         -- 35
  | MsgOther Int

derive instance Eq MC6MsgType

mc6MsgTypeToInt :: MC6MsgType -> Int
mc6MsgTypeToInt = case _ of
  MsgEmpty -> 0
  MsgPC -> 1
  MsgCC -> 2
  MsgNote -> 3
  MsgRealtime -> 4
  MsgSysEx -> 5
  MsgBankUp -> 6
  MsgBankDown -> 7
  MsgTogglePage -> 8
  MsgMidiClock -> 9
  MsgDelay -> 10
  MsgSetToggle -> 11
  MsgEngagePreset -> 12
  MsgBankJump -> 13
  MsgTriggerMessages -> 14
  MsgCCWaveform -> 15
  MsgSequencer -> 16
  MsgCCScroll -> 17
  MsgPCScroll -> 18
  MsgStrymonBankUp -> 19
  MsgStrymonBankDown -> 20
  MsgUtility -> 21
  MsgLooperMode -> 22
  MsgTogglePreset -> 23
  MsgMidiThru -> 24
  MsgPCMultiChannel -> 25
  MsgMultiEngage -> 26
  MsgPresetRename -> 27
  MsgRelaySwitch -> 28
  MsgML10X -> 29
  MsgAxeFXTuner -> 30
  MsgKemperTuner -> 31
  MsgFractal -> 32
  MsgKeystroke -> 33
  MsgSongPosition -> 34
  MsgSongSelect -> 35
  MsgOther n -> n

intToMC6MsgType :: Int -> MC6MsgType
intToMC6MsgType = case _ of
  0 -> MsgEmpty
  1 -> MsgPC
  2 -> MsgCC
  3 -> MsgNote
  4 -> MsgRealtime
  5 -> MsgSysEx
  6 -> MsgBankUp
  7 -> MsgBankDown
  8 -> MsgTogglePage
  9 -> MsgMidiClock
  10 -> MsgDelay
  11 -> MsgSetToggle
  12 -> MsgEngagePreset
  13 -> MsgBankJump
  14 -> MsgTriggerMessages
  15 -> MsgCCWaveform
  16 -> MsgSequencer
  17 -> MsgCCScroll
  18 -> MsgPCScroll
  19 -> MsgStrymonBankUp
  20 -> MsgStrymonBankDown
  21 -> MsgUtility
  22 -> MsgLooperMode
  23 -> MsgTogglePreset
  24 -> MsgMidiThru
  25 -> MsgPCMultiChannel
  26 -> MsgMultiEngage
  27 -> MsgPresetRename
  28 -> MsgRelaySwitch
  29 -> MsgML10X
  30 -> MsgAxeFXTuner
  31 -> MsgKemperTuner
  32 -> MsgFractal
  33 -> MsgKeystroke
  34 -> MsgSongPosition
  35 -> MsgSongSelect
  n -> MsgOther n

-- | Decode human-readable message type strings from JSON config
mc6MsgTypeFromString :: String -> Maybe MC6MsgType
mc6MsgTypeFromString = case _ of
  "empty" -> Just MsgEmpty
  "pc" -> Just MsgPC
  "cc" -> Just MsgCC
  "note" -> Just MsgNote
  "bankUp" -> Just MsgBankUp
  "bankDown" -> Just MsgBankDown
  "togglePage" -> Just MsgTogglePage
  "delay" -> Just MsgDelay
  "setToggle" -> Just MsgSetToggle
  "engagePreset" -> Just MsgEngagePreset
  "bankJump" -> Just MsgBankJump
  "ccWaveform" -> Just MsgCCWaveform
  "sequencer" -> Just MsgSequencer
  "ccScroll" -> Just MsgCCScroll
  "pcScroll" -> Just MsgPCScroll
  "utility" -> Just MsgUtility
  "looperMode" -> Just MsgLooperMode
  "togglePreset" -> Just MsgTogglePreset
  _ -> Nothing

-- | MC6 action type enum — maps to 'a' field in backup JSON
data MC6Action
  = ActionNone                  -- 0
  | ActionPress                 -- 1
  | ActionRelease               -- 2
  | ActionLongPress             -- 3
  | ActionLongPressRelease      -- 4
  | ActionDoubleTap             -- 5
  | ActionDoubleTapRelease      -- 6
  | ActionLongDoubleTap         -- 7
  | ActionLongDoubleTapRelease  -- 8
  | ActionReleaseAll            -- 9
  | ActionOnFirstEngage         -- 10
  | ActionOnFirstEngageSendOnly -- 11
  | ActionOnDisengage           -- 12
  | ActionLongPressScroll       -- 13
  | ActionOther Int

derive instance Eq MC6Action

mc6ActionToInt :: MC6Action -> Int
mc6ActionToInt = case _ of
  ActionNone -> 0
  ActionPress -> 1
  ActionRelease -> 2
  ActionLongPress -> 3
  ActionLongPressRelease -> 4
  ActionDoubleTap -> 5
  ActionDoubleTapRelease -> 6
  ActionLongDoubleTap -> 7
  ActionLongDoubleTapRelease -> 8
  ActionReleaseAll -> 9
  ActionOnFirstEngage -> 10
  ActionOnFirstEngageSendOnly -> 11
  ActionOnDisengage -> 12
  ActionLongPressScroll -> 13
  ActionOther n -> n

intToMC6Action :: Int -> MC6Action
intToMC6Action = case _ of
  0 -> ActionNone
  1 -> ActionPress
  2 -> ActionRelease
  3 -> ActionLongPress
  4 -> ActionLongPressRelease
  5 -> ActionDoubleTap
  6 -> ActionDoubleTapRelease
  7 -> ActionLongDoubleTap
  8 -> ActionLongDoubleTapRelease
  9 -> ActionReleaseAll
  10 -> ActionOnFirstEngage
  11 -> ActionOnFirstEngageSendOnly
  12 -> ActionOnDisengage
  13 -> ActionLongPressScroll
  n -> ActionOther n

-- | Decode human-readable action strings from JSON config
mc6ActionFromString :: String -> Maybe MC6Action
mc6ActionFromString = case _ of
  "none" -> Just ActionNone
  "press" -> Just ActionPress
  "release" -> Just ActionRelease
  "longPress" -> Just ActionLongPress
  "longPressRelease" -> Just ActionLongPressRelease
  "doubleTap" -> Just ActionDoubleTap
  "doubleTapRelease" -> Just ActionDoubleTapRelease
  "releaseAll" -> Just ActionReleaseAll
  _ -> Nothing

-- | Toggle position — maps to 'tg' field in backup JSON
data MC6TogglePosition
  = ToggleOff   -- 0
  | ToggleOn    -- 1
  | ToggleBoth  -- 2

derive instance Eq MC6TogglePosition

mc6ToggleToInt :: MC6TogglePosition -> Int
mc6ToggleToInt = case _ of
  ToggleOff -> 0
  ToggleOn -> 1
  ToggleBoth -> 2

mc6ToggleFromString :: String -> MC6TogglePosition
mc6ToggleFromString = case _ of
  "off" -> ToggleOff
  "on" -> ToggleOn
  _ -> ToggleBoth

-- | MC6 message — maps directly to msgArray entries in backup JSON
type MC6Message =
  { msgType :: MC6MsgType
  , channel :: Int           -- 1-16
  , data1 :: Int             -- 0-127
  , data2 :: Int             -- 0-127
  , data3 :: Int
  , data4 :: Int
  , action :: MC6Action
  , togglePosition :: MC6TogglePosition
  , msgIndex :: Int          -- 0-15
  }

-- | MC6 preset — maps to presetArray entries in backup JSON
type MC6Preset =
  { presetNum :: Int         -- 0-11
  , shortName :: String
  , toggleName :: String
  , longName :: String
  , toToggle :: Boolean
  , toggleGroup :: Int
  , messages :: Array MC6Message
  }

-- | MC6 bank — maps to bankArray entries in backup JSON
type MC6NativeBank =
  { bankNumber :: Int        -- 0-29
  , bankName :: String
  , bankClearToggle :: Boolean
  , presets :: Array MC6Preset
  }

-- | Device model — maps to deviceModel field in backup JSON
data MC6Model = MC6MK2 | MC6PRO | MC8 | MC3

derive instance Eq MC6Model

mc6ModelToInt :: MC6Model -> Int
mc6ModelToInt = case _ of
  MC6MK2 -> 3
  MC6PRO -> 4
  MC8 -> 5
  MC3 -> 6

mc6ModelFromString :: String -> Maybe MC6Model
mc6ModelFromString = case _ of
  "mc6mk2" -> Just MC6MK2
  "mc6pro" -> Just MC6PRO
  "mc8" -> Just MC8
  "mc3" -> Just MC3
  _ -> Nothing
