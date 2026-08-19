-- | Asking the MC6 what it contains.
-- |
-- | `SysEx.purs` is the write half of the Morningstar protocol and has been in
-- | use for months. This is the read half, reverse-engineered from a capture of
-- | the official editor's startup handshake
-- | (`test/mc6-editor-handshake-20260816.json`, taken with `static/sniff.html`).
-- |
-- | Why it matters more than a convenience: everything else in this application
-- | fights the fact that pedals cannot report their state (`DESIGN-v2` §2). The
-- | MC6 *can*. This is the one device in the rig where intent can be checked
-- | against reality instead of believed, and until now the app has been writing
-- | to it blind — which is how a generated looper bank came to be sitting on top
-- | of a hand-built one, with the old bank's name still showing.
-- |
-- | On connect the device *volunteers* a dump — controller settings, then every
-- | bank name in one frame, then the twelve switch names of whichever bank it is
-- | currently sitting on. Verified four times over in
-- | `test/mc6-connect-dump-20260816.json`.
-- |
-- | **This file used to claim there was no read request**, on the strength of a
-- | function-code sweep that found nothing asking for bank data — and that claim
-- | cost real work, because it made "read the whole device" mean walking the MC6
-- | through all thirty banks and hoping it spoke on the way.
-- |
-- | There is a read request. `F1=0, F2=64, F3=bank` returns any bank's switch
-- | names with the device sitting still; Morningstar's own editor calls it
-- | `requestPresetNamesData`, and `F1=0, F2=43` asks for all of them at once.
-- | Both are in `Data.MC6.SysEx` now, read out of the editor's own bundle rather
-- | than guessed at.
-- |
-- | Worth keeping as a lesson rather than quietly fixing: a negative result
-- | about somebody else's protocol is a statement about our search, not about
-- | their protocol. The answer was one grep away in shipped JavaScript the whole
-- | time.
-- |
-- | Either way this module only decodes. A requested reply and a volunteered one
-- | are the same `09 01` frame, so nothing here needs to know which it is.
-- |
-- | **Bank numbers here are what the wire uses: 0-based.** Morningstar's editor
-- | displays them 1-based, so wire 19 is the bank the editor calls 20. Three
-- | independent confirmations are recorded in `DESIGN-CONTROLS.md` §7. Getting
-- | this backwards silently writes to a neighbouring bank.
module Data.MC6.Read
  ( MC6Reply(..)
  , decodeReply
  -- | Shared with `Data.MC6.Settings`, which decodes the payloads this module
  -- | only carries. Exported rather than copied: a six-line parser duplicated
  -- | across two modules is a six-line parser that will disagree with itself.
  , tlvs
  , trimAscii
  , replyBank
  ) where

import Prelude

import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits as SCU
import Data.String as Str
import Data.Tuple (Tuple(..))

data MC6Reply
  = BankNames (Array (Tuple Int String))
  | BankSwitches Int (Array String)
  -- | Where the device is standing, said the moment it moves. **F1=6, F2=2**,
  -- | carrying the bank number in TLV 0 and the bank's long name in TLV 3.
  -- |
  -- | This is the answer to "which bank are you on", and it went unread for
  -- | months while the app inferred the same fact from `BankSwitches`, which is
  -- | a list of switch names that happens to name its bank. That inference is
  -- | both slower and weaker: after a bank change the device sends this
  -- | immediately, and the switch names only after the whole controller-settings
  -- | parade — so a confirmation waiting on the names could time out while the
  -- | device had already answered.
  | CurrentBank Int String
  -- | The preset the device has in hand. **F1=6, F2=1**, TLV 0 being the same
  -- | `[bank, preset, isExp]` header `SysEx.sysexPresetData` writes. Arrives
  -- | alongside `CurrentBank` and agrees with it about the bank.
  | CurrentPreset Int Int
  -- | The device saying whether it is in editor mode. **F1=0, F2=125**, F3 being
  -- | 1 on connect and 0 on disconnect.
  -- |
  -- | Worth having decoded because editor mode is not a state we can only infer
  -- | from having asked for it: the device announces it, and a session opened by
  -- | something else — Morningstar's editor in another tab — is a thing we would
  -- | otherwise have no way to notice.
  | EditorMode Boolean
  -- | The controller's own settings, as a flat byte array. **F1=3, F2=33**, sent
  -- | unasked when a session opens — 32 bytes, no TLV framing.
  -- |
  -- | Undecoded on purpose: we know one of these bytes is "load preset data into
  -- | editor using switch press", which the app turns off to hold a session and
  -- | turns back on to release one, and we do not know which byte. Carrying the
  -- | payload whole lets two captures be compared — hold a session, read the
  -- | device, and diff against a capture taken with the setting on — which is
  -- | both how the byte gets identified and, until it is, the only way to check
  -- | that the write landed at all without opening Morningstar's editor.
  -- | A `03 2x` settings frame, with its sub-code and payload intact.
  -- | Decoded by `Data.MC6.Settings`, which is a separate module rather than
  -- | a case here so that this one stays about *frames* — and so that Settings
  -- | can borrow this module's TLV parser without the two importing each other.
  | ControllerSettings Int (Array Int)
  | OtherReply Int Int

derive instance Eq MC6Reply

replyBank :: MC6Reply -> Maybe Int
replyBank = case _ of
  BankSwitches b _ -> Just b
  CurrentBank b _ -> Just b
  CurrentPreset b _ -> Just b
  _ -> Nothing

-- | Decode a frame the MC6 sent us.
-- |
-- | Returns `Nothing` for anything that is not a Morningstar frame at all, and
-- | `OtherReply` for frames whose function code we have not decoded — of which
-- | there are several in the capture (controller settings, omniport config).
-- | Naming them rather than dropping them keeps the unknown visible, which is
-- | the same discipline the bank survey uses.
decodeReply :: Array Int -> Maybe MC6Reply
decodeReply bytes = do
  h0 <- Array.index bytes 0
  m1 <- Array.index bytes 1
  m2 <- Array.index bytes 2
  m3 <- Array.index bytes 3
  f1 <- Array.index bytes 6
  f2 <- Array.index bytes 7
  f3 <- Array.index bytes 8
  if h0 /= 0xF0 || m1 /= 0x00 || m2 /= 0x21 || m3 /= 0x24 then Nothing
  else
    let payload = Array.drop 16 (Array.dropEnd 2 bytes)
    in Just case f1, f2 of
      0x11, 0x05 -> BankNames (map (\(Tuple t v) -> Tuple t (trimAscii v)) (tlvs payload))
      0x09, 0x01 -> BankSwitches f3 (map (\(Tuple _ v) -> trimAscii v) (tlvs payload))
      -- The bank number is in the *payload*, not in F3 — F3 is 0 on both of
      -- these — so they have to be read rather than glanced at.
      0x06, 0x02 -> CurrentBank
        (tlvByte 0 0 payload)
        (maybe "" trimAscii (tlvData 3 payload))
      0x06, 0x01 -> CurrentPreset (tlvByte 0 0 payload) (tlvByte 0 1 payload)
      0x00, 0x7D -> EditorMode (f3 == 1)
      -- The whole family, not just 0x21. Ten frames arrive on connect and
      -- nine of them used to fall through to `OtherReply` and be logged as a
      -- function code, which is how the device's own channel table sat
      -- unread while the app took a channel on the word of a comment.
      0x03, _ | f2 >= 0x20 && f2 <= 0x29 -> ControllerSettings f2 payload
      _, _ -> OtherReply f1 f2

-- | `0x7F <type> <length> <data>`, repeated — byte-identical to the framing
-- | `SysEx.purs` writes, which is why this is a mirror rather than new work.
tlvs :: Array Int -> Array (Tuple Int (Array Int))
tlvs = go []
  where
  go acc p = case Array.index p 0, Array.index p 1, Array.index p 2 of
    Just 0x7F, Just t, Just len ->
      go (Array.snoc acc (Tuple t (Array.slice 3 (3 + len) p))) (Array.drop (3 + len) p)
    _, _, _ -> acc

-- | The data of one TLV, by type.
tlvData :: Int -> Array Int -> Maybe (Array Int)
tlvData wanted payload =
  map (\(Tuple _ v) -> v) (Array.find (\(Tuple t _) -> t == wanted) (tlvs payload))

-- | One byte out of one TLV, defaulting to 0 rather than failing the whole
-- | decode: a truncated frame should cost us that field, not the frame.
tlvByte :: Int -> Int -> Array Int -> Int
tlvByte wanted offset payload =
  fromMaybe 0 (tlvData wanted payload >>= \d -> Array.index d offset)

-- | Names come back space-padded to their field width.
trimAscii :: Array Int -> String
trimAscii = Str.trim <<< SCU.fromCharArray <<< Array.mapMaybe fromCharCode
