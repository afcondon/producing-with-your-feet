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
  , replyBank
  ) where

import Prelude

import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.String as Str
import Data.Tuple (Tuple(..))

data MC6Reply
  = BankNames (Array (Tuple Int String))
  | BankSwitches Int (Array String)
  | OtherReply Int Int

derive instance Eq MC6Reply

replyBank :: MC6Reply -> Maybe Int
replyBank = case _ of
  BankSwitches b _ -> Just b
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

-- | Names come back space-padded to their field width.
trimAscii :: Array Int -> String
trimAscii = Str.trim <<< SCU.fromCharArray <<< Array.mapMaybe fromCharCode
