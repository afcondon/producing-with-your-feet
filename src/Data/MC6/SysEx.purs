module Data.MC6.SysEx
  ( Ctx
  , Loose
  , Session
  , Upload
  , Reply
  , Frame
  , frameBytes
  , frameLabel
  , labelled
  , sysexConnect
  , sysexDisconnect
  , sysexStartUpload
  , sysexCompleteUpload
  , sysexPresetData
  , sysexBankData
  , sysexSettingsBegin
  , sysexSettingsData
  , sysexSettingsCommit
  , sysexClearPreset
  , sysexRequestPresetNames
  , sysexRequestAllPresetNames
  , sysexEditorBankChange
  , sysexSwitchPressLoad
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

-- | What has to be true of the device before a frame may be sent to it.
-- |
-- | The Morningstar protocol has three of these and the frame itself does not
-- | say which it belongs to — the device simply ignores, in silence, a frame
-- | that arrives out of context. That silence is indistinguishable from a
-- | disconnected cable, so the rule cannot live in a comment: `Frame` is
-- | indexed by its context and `Data.MC6.Wire` is the only way to obtain
-- | permission to send one.
data Ctx

-- | Sendable at any time. Only the two frames addressed to device `0x00` —
-- | opening and closing an editor session — are in this class.
foreign import data Loose :: Ctx

-- | Sendable only between connect and disconnect. Everything addressed to
-- | `0x03`: every question, and the two frames that open and close an upload.
foreign import data Session :: Ctx

-- | Sendable only between start-upload and complete-upload. Preset data, which
-- | a session alone will not accept.
foreign import data Upload :: Ctx

-- | Sent in answer to a frame that arrived, so its legality follows from the
-- | frame it answers rather than from any session we opened.
foreign import data Reply :: Ctx

-- | Bytes on the wire, plus the name to log them under.
-- |
-- | The label belongs to the frame rather than to the call site: a call site
-- | that names its own frames is free to name them wrongly, and did — a
-- | preset-data write was being logged as "diag", "looper", "ctrl" or "all"
-- | depending on which code path built it, so the log said what the app was
-- | doing and not what it sent.
newtype Frame (c :: Ctx) = Frame { label :: String, bytes :: Array Int }

frameBytes :: forall c. Frame c -> Array Int
frameBytes (Frame f) = f.bytes

frameLabel :: forall c. Frame c -> String
frameLabel (Frame f) = f.label

-- | Note down *why* a frame is being sent, without letting the note replace
-- | what the frame is. `labelled "looper" (sysexPresetData …)` logs as
-- | `looper/preset-3-8`.
labelled :: forall c. String -> Frame c -> Frame c
labelled note (Frame f) = Frame (f { label = note <> "/" <> f.label })

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
-- | label -> deviceId -> functionIds (F1-F6) -> payload -> complete message with checksum and F7
-- |
-- | Polymorphic in the context on purpose, and private: each exported frame
-- | below fixes its own, which is the one place the protocol's rule about when
-- | it may be sent is written down.
sysexFrame :: forall c. String -> Int -> Array Int -> Array Int -> Frame c
sysexFrame label deviceId funcIds payload =
  let paddedFunc = Array.take 6 (funcIds <> Array.replicate 6 0)
      header = [0xF0] <> manufacturerId <> [deviceId, 0x00] <> paddedFunc <> [0x00, 0x00, 0x00, 0x00]
      body = header <> payload
      cs = checksum body
  in Frame { label, bytes: body <> [cs, 0xF7] }

-- | Format bytes as hex string for debugging
toHexString :: Array Int -> String
toHexString bytes = Str.joinWith " " (map toHex bytes)
  where
  toHex n =
    let h = toStringAs hexadecimal n
    in if Str.length h < 2 then "0" <> h else h

-- Controller commands (deviceId = 0x00, no editor session required)

-- | Enter editor session
sysexConnect :: Frame Loose
sysexConnect = sysexFrame "connect" 0x00 [0x00, 0x1B] []

-- | Leave editor session
sysexDisconnect :: Frame Loose
sysexDisconnect = sysexFrame "disconnect" 0x00 [0x00, 0x1C] []

-- Upload protocol (deviceId = 0x03 for MC6MK2, require editor session)
-- Flow: connect → startUpload → [preset data...] → completeUpload → disconnect

-- | Start upload session — MC6 will respond with "ready for next" (F1=7, F2=0, F3=33)
sysexStartUpload :: Frame Session
sysexStartUpload = sysexFrame "start-upload" mc6mk2DeviceId [0x07, 0x00, 0x30, 0x00] []

-- | Complete upload — MC6 commits data and responds with (F1=7, F2=0, F3=17)
sysexCompleteUpload :: Frame Session
sysexCompleteUpload = sysexFrame "complete-upload" mc6mk2DeviceId [0x07, 0x00, 0x31, 0x00] []

-- | Send full preset data via SysEx (F1=7, F2=17).
-- | Must be sent within an upload session (after sysexStartUpload).
-- | bankNum -> presetNum -> shortName -> longName -> toToggle -> messages -> SysEx bytes
-- |
-- | TLV types: 00=header, 01=message (9 bytes x16), 02=short name (8),
-- |   03=toggle name (8), 04=long name (24), 05=config (4)
sysexPresetData :: Int -> Int -> String -> String -> Boolean -> Array MC6Message -> Frame Upload
sysexPresetData bankNum presetNum shortName longName toToggle messages =
  let funcIds = [0x07, 0x11, presetNum, 0x00, 0x00, 0x00]
      hdr = headerTLV bankNum presetNum
      msgTlvs = Array.concatMap messageTLV (padMessages messages)
      nameTlvs = shortNameTLV shortName <> toggleNameTLV shortName <> longNameTLV longName
      cfg = configTLV toToggle
      payload = hdr <> msgTlvs <> nameTlvs <> cfg
  in sysexFrame ("preset-" <> show bankNum <> "-" <> show presetNum)
       mc6mk2DeviceId funcIds payload

-- | Write a bank's own properties: its name and its sixteen bank-level
-- | messages. **F1=6, F2=18, F3=bank.**
-- |
-- | The thing `sysexPresetData` could never do. A preset write carries twelve
-- | switches and says nothing about the bank holding them, which is why every
-- | bank this app has ever generated arrived unnamed — and why bank 19 kept
-- | showing "Ableton" long after it had stopped being the Ableton bank.
-- |
-- | Read out of a capture of Morningstar's editor renaming a bank rather than
-- | guessed, and held to that capture byte for byte in the tests. The layout
-- | mirrors a preset almost exactly, which is the sign it was read and not
-- | invented: header TLV carrying the bank number, sixteen nine-byte message
-- | TLVs, then the name.
-- |
-- | **The name field is twenty-four bytes**, not the sixteen the longest name
-- | on this device happens to be. Worth having measured rather than inferred:
-- | refusing at sixteen would have refused names the device accepts.
sysexBankData :: Int -> String -> Array MC6Message -> Frame Session
sysexBankData bankNum name messages =
  let funcIds = [0x06, 0x12, bankNum, 0x00, 0x00, 0x00]
      -- The bank number twice, in F3 and again in the payload, exactly as a
      -- preset write repeats it. Belt and braces are theirs, not ours.
      hdr = [0x7F, 0x00, 0x01, bankNum]
      -- Two bytes the editor sends as zero. Not named, because a capture of one
      -- rename does not say what they are, and `bankClearToggle` is only the
      -- most likely candidate rather than a known one.
      flags = [0x7F, 0x01, 0x02, 0x00, 0x00]
      msgTlvs = Array.concatMap bankMessageTLV (padBankMessages messages)
      payload = hdr <> flags <> msgTlvs <> bankNameTLV name
  in sysexFrame ("bank-" <> show bankNum) mc6mk2DeviceId funcIds payload

-- | Pad bank messages to sixteen, with **`ToggleOff` in the empty slots**.
-- |
-- | A separate function from `padMessages` for one byte. The shared version
-- | fills empties with `ToggleBoth`, and the editor's own bank write uses
-- | `ToggleOff` — the only difference between our frame and theirs across all
-- | 246 bytes, and found only because the frame was compared to a capture
-- | rather than reasoned about.
-- |
-- | The preset path is left alone deliberately. Its empty-slot toggle byte has
-- | never been checked against an editor capture, and presets currently work;
-- | changing both on the evidence of one would be spending a confirmed fix on
-- | an unconfirmed guess.
padBankMessages :: Array MC6Message -> Array MC6Message
padBankMessages msgs =
  let existing = Array.length msgs
      pad = if existing < 16 then map emptyBankMsg (Array.range existing 15) else []
  in Array.take 16 (msgs <> pad)
  where
  emptyBankMsg idx =
    { msgType: MsgEmpty, channel: 1
    , data1: 0, data2: 0, data3: 0, data4: 0
    , action: ActionNone, togglePosition: ToggleOff, msgIndex: idx
    }

-- | Tag 7F, Type 03 in a bank frame: the bank name, 24 bytes, space-padded.
-- |
-- | Type 03 means the toggle name in a *preset* frame and the bank name here.
-- | The tag numbering is per-frame, not global, which is exactly the kind of
-- | thing that would have been assumed wrong if this had been reasoned about
-- | instead of captured.
bankNameTLV :: String -> Array Int
bankNameTLV name =
  let chars = map toCharCode (SCU.toCharArray (SCU.take 24 name))
      padded = Array.take 24 (chars <> Array.replicate 24 0x20)
  in [0x7F, 0x03, 0x18] <> padded

-- | Tag 7F, Type 02 in a bank frame: one bank-level message, nine bytes.
bankMessageTLV :: MC6Message -> Array Int
bankMessageTLV msg =
  [ 0x7F, 0x02, 0x09
  , msg.msgIndex
  , msg.data1
  , msg.data2
  , msg.data3
  , mc6MsgTypeToInt msg.msgType
  , msg.channel
  , mc6ActionToInt msg.action
  , mc6ToggleToInt msg.togglePosition
  , msg.data4
  ]

-- | The controller-settings write, in three frames: **`04 00` begin, `04 02`
-- | the payload, `04 01` commit.**
-- |
-- | Mirrors the preset upload's `07 00 30` / `07 00 31` bracket, and mirrors it
-- | closely enough that the pattern is worth stating: on this device a write is
-- | bracketed and a read is not.
-- |
-- | `04 02` carries **the same thirty-two byte payload that `03 21` returns** —
-- | the read code and the write code differ, the payload does not. That is what
-- | makes settings writable at all: whatever `Data.MC6.Settings` decodes can be
-- | handed straight back.
sysexSettingsBegin :: Frame Session
sysexSettingsBegin = sysexFrame "settings-begin" mc6mk2DeviceId [0x04, 0x00] []

sysexSettingsData :: Array Int -> Frame Session
sysexSettingsData payload =
  sysexFrame "settings-data" mc6mk2DeviceId [0x04, 0x02] payload

sysexSettingsCommit :: Frame Session
sysexSettingsCommit = sysexFrame "settings-commit" mc6mk2DeviceId [0x04, 0x01] []

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
sysexRequestPresetNames :: Int -> Frame Session
sysexRequestPresetNames bank =
  sysexFrame ("names-bank-" <> show bank) mc6mk2DeviceId [0x00, 0x40, bank] []

-- | Ask for every bank's switch names at once. **F1=0, F2=43.**
-- |
-- | The editor calls this on connect and after any paste, and it is cheaper than
-- | thirty separate requests — but it is not a substitute for them, because
-- | nothing in the protocol says how many frames it will produce or when it has
-- | finished. Worth asking first and then filling the gaps one bank at a time.
sysexRequestAllPresetNames :: Frame Session
sysexRequestAllPresetNames = sysexFrame "names-all" mc6mk2DeviceId [0x00, 0x2B] []

-- | Put the device on a bank. **F1=0, F2=31, F3=bank, F4=1 for presets.**
-- |
-- | Not needed to read any more, now that banks can simply be asked for. Kept
-- | because it is the honest way to show a page on the hardware while working on
-- | it here, which is a thing worth having and was never the same job as reading.
-- | Tested at device `0x00` too — the number connect and disconnect use, and
-- | so the one a controller command would use, since `F1=0` is otherwise the
-- | family of things done to a *running* pedalboard (bank up/down `0,16`/`0,17`,
-- | toggle page `0,33`). It does nothing: no reply, and the device was still on
-- | its previous bank when a session opened immediately afterwards. A session is
-- | required, and Morningstar's editor simply holds one open.
sysexEditorBankChange :: Int -> Frame Session
sysexEditorBankChange bank =
  sysexFrame ("bank-change-" <> show bank) mc6mk2DeviceId [0x00, 0x1F, bank, 0x01] []


-- | Turn "Load Preset Data into Editor using Switch Press" on or off.
-- | **F1=3, F2=49, F3=flag.**
-- |
-- | The reason a held-open editor session is usable at all. With this on — the
-- | factory default — the controller cannot tell a switch press meaning "load
-- | this preset into the editor" from one meaning "engage this preset", so it
-- | *blocks the ambiguous functions while an editor is connected*: bank jump
-- | from the device itself, MIDI clock, and others. Off, presses stop feeding
-- | the editor and everything is unblocked.
-- |
-- | MIDI clock is the one that matters. Holding a session with this on would
-- | stop clock to the rig, silently, in the middle of a performance — and it
-- | would not present as a session problem.
-- |
-- | **The clock half is unverified on this rig** (2026-08-18): the blocked-list
-- | is Morningstar's documentation, not something we have watched happen, and
-- | the only clock consumers here are tap tempo on a few pedals. Worth checking
-- | when that area is next worked on; until then the setting is turned off on
-- | the strength of the manual.
-- |
-- | We can set it but cannot yet read it: the `3/33` reply carries the
-- | controller settings and certainly contains this bit, but which byte is
-- | unknown, so the app restores what it *assumes* was there rather than what
-- | it saw. Comparing two captures with the setting toggled would fix that, and
-- | until then this is the one MC6 value we write blind.
sysexSwitchPressLoad :: Boolean -> Frame Session
sysexSwitchPressLoad on =
  sysexFrame ("switch-press-load-" <> (if on then "on" else "off"))
    mc6mk2DeviceId [0x03, 0x31, if on then 0x01 else 0x00] []

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
sysexRequestFullDump :: Frame Session
sysexRequestFullDump = sysexFrame "dump-all" mc6mk2DeviceId [0x07, 0x00, 0x33] []

-- | Ask for one bank — whichever the device is on. **F1=7, F2=0, F3=50.**
-- |
-- | Fifteen frames rather than four hundred and fifty: twelve presets, two
-- | expression presets and the bank's own record. The editor calls this
-- | `bankNewProtocol` and the all-banks request `allBanksNewProtocol`, and the
-- | two differ by one in the third function byte — which is exactly the sort of
-- | neighbouring opcode that returns silence rather than an error when you pick
-- | the wrong one.
sysexRequestBankDump :: Frame Session
sysexRequestBankDump = sysexFrame "dump-bank" mc6mk2DeviceId [0x07, 0x00, 0x32] []

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
sysexAcknowledge :: Int -> Frame Reply
sysexAcknowledge receivedChecksum =
  sysexFrame "ack" mc6mk2DeviceId [0x00, 0x7F, receivedChecksum] []

sysexClearPreset :: Int -> Int -> Frame Upload
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
