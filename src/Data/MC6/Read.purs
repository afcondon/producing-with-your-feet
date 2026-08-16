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
-- | **Bank numbers here are what the wire uses: 0-based.** Morningstar's editor
-- | displays them 1-based, so wire 19 is the bank the editor calls 20. Three
-- | independent confirmations are recorded in `DESIGN-CONTROLS.md` §7. Getting
-- | this backwards silently writes to a neighbouring bank.
module Data.MC6.Read
  ( MC6Reply(..)
  , requestBankNames
  , requestBankSwitches
  , decodeReply
  , replyBank
  ) where

import Prelude

import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Foldable (foldl)
import Data.Int.Bits (shr, xor, (.&.))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.String as Str
import Data.Tuple (Tuple(..))

manufacturerId :: Array Int
manufacturerId = [ 0x00, 0x21, 0x24 ]

mc6mk2DeviceId :: Int
mc6mk2DeviceId = 0x03

-- | XOR of every byte before it, masked to seven bits. Verified against the
-- | capture: both sampled reply frames reproduce their own checksum exactly, so
-- | this is the same rule the device uses in both directions.
checksum :: Array Int -> Int
checksum bytes = foldl xor 0 bytes .&. 0x7F

-- | Build a request.
-- |
-- | Differs from `SysEx.sysexFrame` in one respect discovered from the capture:
-- | bytes 14 and 15 carry the **total frame length** as a fourteen-bit value,
-- | most significant seven bits first. Every captured frame agrees with its own
-- | declared length. The upload path leaves those bytes zero and demonstrably
-- | works, so the device does not appear to check them on input — but a request
-- | is a new conversation and there is no reason to send a field wrong when the
-- | correct value is known.
request :: Array Int -> Array Int
request funcIds =
  let padded = Array.take 6 (funcIds <> Array.replicate 6 0)
      -- 16 header + 0 payload + checksum + F7
      total = 18
      body =
        [ 0xF0 ] <> manufacturerId <> [ mc6mk2DeviceId, 0x00 ] <> padded
          <> [ 0x00, 0x00, (total `shr` 7) .&. 0x7F, total .&. 0x7F ]
  in body <> [ checksum body, 0xF7 ]

-- | Every bank name in one frame. The cheapest possible whole-instrument read:
-- | one request labels all thirty cards.
requestBankNames :: Array Int
requestBankNames = request [ 0x11, 0x05 ]

-- | The twelve switch names of one bank. The editor issues this per bank as you
-- | navigate rather than dumping everything, so verification can be incremental.
requestBankSwitches :: Int -> Array Int
requestBankSwitches bank = request [ 0x09, 0x01, bank ]

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
