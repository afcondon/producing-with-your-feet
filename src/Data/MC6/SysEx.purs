module Data.MC6.SysEx
  ( sysexConnect
  , sysexDisconnect
  , sysexStartUpload
  , sysexCompleteUpload
  , sysexPresetData
  , sysexClearPreset
  , sysexRequestPresetNames
  , sysexRequestAllPresetNames
  , sysexEditorBankChange
  , sysexRequestFullDump
  , sysexRequestBankDump
  , sysexAcknowledge
  , mc6mk2DeviceId
  , toHexString
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toStringAs, hexadecimal)
import Data.Int.Bits (xor, (.&.))
import Data.MC6.Types (MC6Action(..), MC6Message, MC6MsgType(..), MC6TogglePosition(..), mc6MsgTypeToInt, mc6ActionToInt, mc6ToggleToInt)
import Data.Char (toCharCode)
import Data.String as Str
import Data.String.CodeUnits as SCU

-- | Morningstar manufacturer ID: 00 21 24
manufacturerId :: Array Int
manufacturerId = [0x00, 0x21, 0x24]

-- | MC6 MKII device ID
mc6mk2DeviceId :: Int
mc6mk2DeviceId = 0x03

-- | XOR checksum of all bytes, masked to 7-bit
checksum :: Array Int -> Int
checksum bytes = foldl xor 0 bytes .&. 0x7F

-- | Build a complete SysEx frame.
-- | deviceId -> functionIds (F1-F6) -> payload -> complete message with checksum and F7
sysexFrame :: Int -> Array Int -> Array Int -> Array Int
sysexFrame deviceId funcIds payload =
  let paddedFunc = Array.take 6 (funcIds <> Array.replicate 6 0)
      header = [0xF0] <> manufacturerId <> [deviceId, 0x00] <> paddedFunc <> [0x00, 0x00, 0x00, 0x00]
      body = header <> payload
      cs = checksum body
  in body <> [cs, 0xF7]

-- | Format bytes as hex string for debugging
toHexString :: Array Int -> String
toHexString bytes = Str.joinWith " " (map toHex bytes)
  where
  toHex n =
    let h = toStringAs hexadecimal n
    in if Str.length h < 2 then "0" <> h else h

-- Controller commands (deviceId = 0x00, no editor session required)

-- | Enter editor session
sysexConnect :: Array Int
sysexConnect = sysexFrame 0x00 [0x00, 0x1B] []

-- | Leave editor session
sysexDisconnect :: Array Int
sysexDisconnect = sysexFrame 0x00 [0x00, 0x1C] []

-- Upload protocol (deviceId = 0x03 for MC6MK2, require editor session)
-- Flow: connect → startUpload → [preset data...] → completeUpload → disconnect

-- | Start upload session — MC6 will respond with "ready for next" (F1=7, F2=0, F3=33)
sysexStartUpload :: Array Int
sysexStartUpload = sysexFrame mc6mk2DeviceId [0x07, 0x00, 0x30, 0x00] []

-- | Complete upload — MC6 commits data and responds with (F1=7, F2=0, F3=17)
sysexCompleteUpload :: Array Int
sysexCompleteUpload = sysexFrame mc6mk2DeviceId [0x07, 0x00, 0x31, 0x00] []

-- | Send full preset data via SysEx (F1=7, F2=17).
-- | Must be sent within an upload session (after sysexStartUpload).
-- | bankNum -> presetNum -> shortName -> longName -> toToggle -> messages -> SysEx bytes
-- |
-- | TLV types: 00=header, 01=message (9 bytes x16), 02=short name (8),
-- |   03=toggle name (8), 04=long name (24), 05=config (4)
sysexPresetData :: Int -> Int -> String -> String -> Boolean -> Array MC6Message -> Array Int
sysexPresetData bankNum presetNum shortName longName toToggle messages =
  let funcIds = [0x07, 0x11, presetNum, 0x00, 0x00, 0x00]
      hdr = headerTLV bankNum presetNum
      msgTlvs = Array.concatMap messageTLV (padMessages messages)
      nameTlvs = shortNameTLV shortName <> toggleNameTLV shortName <> longNameTLV longName
      cfg = configTLV toToggle
      payload = hdr <> msgTlvs <> nameTlvs <> cfg
  in sysexFrame mc6mk2DeviceId funcIds payload

-- | Send an empty preset to clear a switch
-- | Ask for one bank's twelve switch names. **F1=0, F2=64, F3=bank.**
-- |
-- | The request this app spent months believing did not exist. `Data.MC6.Read`
-- | records a sweep of the function-code space finding nothing that asks for
-- | bank data, and concluded the device could only be made to *volunteer* — so
-- | reading everything meant walking the MC6 through all thirty banks and
-- | hoping it would talk on the way.
-- |
-- | It is simply there, and always was: Morningstar's own editor calls it
-- | `requestPresetNamesData(bank)`. Read out of the editor's bundle rather than
-- | guessed, so the byte layout is theirs and not our reconstruction of it.
-- |
-- | The device answers with the same `09 01 <bank>` frame it volunteers on
-- | connect, which is why nothing downstream had to change: one decoder, and it
-- | cannot tell whether the frame was offered or asked for.
sysexRequestPresetNames :: Int -> Array Int
sysexRequestPresetNames bank = sysexFrame mc6mk2DeviceId [0x00, 0x40, bank] []

-- | Ask for every bank's switch names at once. **F1=0, F2=43.**
-- |
-- | The editor calls this on connect and after any paste, and it is cheaper than
-- | thirty separate requests — but it is not a substitute for them, because
-- | nothing in the protocol says how many frames it will produce or when it has
-- | finished. Worth asking first and then filling the gaps one bank at a time.
sysexRequestAllPresetNames :: Array Int
sysexRequestAllPresetNames = sysexFrame mc6mk2DeviceId [0x00, 0x2B] []

-- | Put the device on a bank. **F1=0, F2=31, F3=bank, F4=1 for presets.**
-- |
-- | Not needed to read any more, now that banks can simply be asked for. Kept
-- | because it is the honest way to show a page on the hardware while working on
-- | it here, which is a thing worth having and was never the same job as reading.
sysexEditorBankChange :: Int -> Array Int
sysexEditorBankChange bank = sysexFrame mc6mk2DeviceId [0x00, 0x1F, bank, 0x01] []

-- | Ask the device for everything it has. **F1=7, F2=0, F3=51.**
-- |
-- | The device answers with a long run of `F1=2` frames — one per preset, one
-- | per expression preset and one per bank, 450 in total on an MC6 MKII — each
-- | carrying the full message list rather than a label. This is what
-- | Morningstar's backup uses, and it is the only route to knowing what a switch
-- | *does* rather than what it is called.
-- |
-- | Preferred over the per-preset request (`F1=0, F2=29`) despite being far
-- | larger, because that one is named `engagePreset` in the editor and is only
-- | ever fired there in response to a switch the player already pressed. Reading
-- | a whole device with it would mean asking the MC6 to run every preset it
-- | has — three hundred and sixty presses worth of MIDI into the rig. A dump
-- | cannot do that.
sysexRequestFullDump :: Array Int
sysexRequestFullDump = sysexFrame mc6mk2DeviceId [0x07, 0x00, 0x33] []

-- | Ask for one bank — whichever the device is on. **F1=7, F2=0, F3=50.**
-- |
-- | Fifteen frames rather than four hundred and fifty: twelve presets, two
-- | expression presets and the bank's own record. The editor calls this
-- | `bankNewProtocol` and the all-banks request `allBanksNewProtocol`, and the
-- | two differ by one in the third function byte — which is exactly the sort of
-- | neighbouring opcode that returns silence rather than an error when you pick
-- | the wrong one.
sysexRequestBankDump :: Array Int
sysexRequestBankDump = sysexFrame mc6mk2DeviceId [0x07, 0x00, 0x32] []

-- | Acknowledge a frame. **F1=0, F2=127, F3=the checksum we received.**
-- |
-- | Not optional, and this is why a dump request appeared to do nothing: the
-- | device streams hundreds of frames and waits to be told each one landed. The
-- | editor sets `sendAck = true` at construction and acknowledges *every* valid
-- | SysEx frame before it even looks at what the frame was — so this is flow
-- | control, not a courtesy, and without it the device sends one frame and
-- | stops.
-- |
-- | Echoing the checksum back is what identifies which frame is being
-- | acknowledged; there is no sequence number to use instead.
sysexAcknowledge :: Int -> Array Int
sysexAcknowledge receivedChecksum =
  sysexFrame mc6mk2DeviceId [0x00, 0x7F, receivedChecksum] []

sysexClearPreset :: Int -> Int -> Array Int
sysexClearPreset bankNum presetNum =
  sysexPresetData bankNum presetNum "" "" false []

-- TLV encoders — type numbers match MC6 read format

-- | Tag 7F, Type 00: Preset header [bankNum, presetNum, isExp]
headerTLV :: Int -> Int -> Array Int
headerTLV bankNum presetNum = [0x7F, 0x00, 0x03, bankNum, presetNum, 0x00]

-- | Tag 7F, Type 01: Message record (9 bytes)
messageTLV :: MC6Message -> Array Int
messageTLV msg =
  [ 0x7F, 0x01, 0x09
  , msg.msgIndex
  , mc6MsgTypeToInt msg.msgType
  , msg.data1
  , msg.data2
  , msg.data3
  , msg.channel
  , mc6ActionToInt msg.action
  , mc6ToggleToInt msg.togglePosition
  , msg.data4
  ]

-- | Tag 7F, Type 02: Short name (up to 8 chars, space-padded)
shortNameTLV :: String -> Array Int
shortNameTLV name =
  let chars = map toCharCode (SCU.toCharArray (SCU.take 8 name))
      padded = Array.take 8 (chars <> Array.replicate 8 0x20)
  in [0x7F, 0x02, 0x08] <> padded

-- | Tag 7F, Type 03: Toggle name (up to 8 chars, space-padded)
toggleNameTLV :: String -> Array Int
toggleNameTLV name =
  let chars = map toCharCode (SCU.toCharArray (SCU.take 8 name))
      padded = Array.take 8 (chars <> Array.replicate 8 0x20)
  in [0x7F, 0x03, 0x08] <> padded

-- | Tag 7F, Type 04: Long name (up to 24 chars, space-padded)
longNameTLV :: String -> Array Int
longNameTLV name =
  let chars = map toCharCode (SCU.toCharArray (SCU.take 24 name))
      padded = Array.take 24 (chars <> Array.replicate 24 0x20)
  in [0x7F, 0x04, 0x18] <> padded

-- | Tag 7F, Type 05: Preset config — byte 0 is toToggle flag
configTLV :: Boolean -> Array Int
configTLV toToggle = [0x7F, 0x05, 0x04, if toToggle then 0x01 else 0x00, 0x00, 0x00, 0x00]

-- | Pad messages array to exactly 16 slots (MC6 expects all 16)
padMessages :: Array MC6Message -> Array MC6Message
padMessages msgs =
  let existing = Array.length msgs
      pad = if existing < 16
            then map emptyMsg (Array.range existing 15)
            else []
  in Array.take 16 (msgs <> pad)
  where
  emptyMsg idx =
    { msgType: MsgEmpty
    , channel: 1
    , data1: 0
    , data2: 0
    , data3: 0
    , data4: 0
    , action: ActionNone
    , togglePosition: ToggleBoth
    , msgIndex: idx
    }
