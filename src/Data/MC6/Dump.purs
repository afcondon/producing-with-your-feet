-- | The whole device, messages and all.
-- |
-- | `Data.MC6.Read` decodes what a connect volunteers: bank names, and the
-- | twelve switch *names* of one bank. Names are enough to see the shape of the
-- | instrument and not enough to reproduce it — a page authored from labels
-- | alone is correct on the face of it and silent underneath, which is exactly
-- | the trap the survey's warning describes.
-- |
-- | This is the other half. `SysEx.sysexRequestFullDump` asks the device to send
-- | everything, and it answers with a long run of `F1=2` frames: one per preset,
-- | one per expression preset, one per bank. Each carries a bank number, a preset
-- | number and the full message list, so what comes back can be *compiled and
-- | sent again* rather than merely displayed.
-- |
-- | ## Why a dump rather than asking per preset
-- |
-- | There is a per-preset request — `F1=0, F2=29` — and Morningstar's editor
-- | calls it `engagePreset`. That name is a warning: the editor only ever fires
-- | it in response to a switch the player has already pressed, and "engage"
-- | plainly suggests the device *runs* the preset. Using it three hundred and
-- | sixty times to read a device would mean transmitting every message the MC6
-- | contains to every pedal in the rig. A dump cannot do that; it is pure
-- | transfer. So the safe route is the bulk one, which is a rare case of the
-- | cheaper thing also being the more dangerous.
-- |
-- | ## The format is one we already write
-- |
-- | The payload is the same `7F <tag> <length> <data>` TLV that
-- | `SysEx.sysexPresetData` emits, so this decoder is a mirror of an encoder
-- | that has been talking to the hardware for months rather than a fresh guess.
-- | Tag numbers and field order were read out of the editor's own bundle.
module Data.MC6.Dump
  ( DumpPreset
  , DumpFrame(..)
  , decodeDumpFrame
  , expectedFrames
  , presetsToBanks
  ) where

import Prelude

import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Map as Map
import Data.MC6.Types (MC6Message, MC6MsgType(..), MC6NativeBank, MC6Preset, intToMC6Action, intToMC6MsgType, intToMC6Toggle)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))

-- | One preset as the device reports it.
-- |
-- | `isExp` is carried rather than filtered out here: an expression preset is
-- | real data about the device and dropping it at the decoder would make it
-- | unrecoverable, where dropping it at the consumer is a decision that can be
-- | revisited.
type DumpPreset =
  { bankNumber :: Int
  , presetNum :: Int
  , isExp :: Boolean
  , shortName :: String
  , toggleName :: String
  , longName :: String
  , toToggle :: Boolean
  , toggleGroup :: Int
  , messages :: Array MC6Message
  }

-- | How many frames a full dump produces on an MC6 MKII.
-- |
-- | Thirty banks of twelve presets, two expression presets and one bank record:
-- | 30 × 15 = 450. Taken from the editor, where it is hardcoded per model, and
-- | it checks out against the arithmetic — which is the only reason to trust a
-- | magic number read out of somebody else's minified code.
-- |
-- | Used as a *finish line*, never as a requirement: a dump that stops at 448 is
-- | still 448 presets more than we had, so the count tells us when to stop
-- | waiting rather than whether to keep what arrived.
expectedFrames :: Int
expectedFrames = 450

-- | What a frame from a dump turned out to be.
-- |
-- | Bank records and the control frames are named rather than dropped because
-- | the useful thing about a dump is knowing when it has *finished*, and the
-- | device says so — `F2=0, F3=2` for all banks, `F3=1` for one. Waiting for
-- | silence instead of for that marker is what truncated the first attempt at
-- | 221 frames of 450.
data DumpFrame
  = DumpPresetFrame DumpPreset
  -- | A bank's own message list. Real data, and not the twelve switches this app
  -- | works in, so it is counted and not yet used.
  | DumpBankFrame Int
  | DumpStarted
  | DumpFinished

derive instance Eq DumpFrame

-- | Decode one frame of a dump.
-- |
-- | **The function code is 7, not 2.** The editor's collector has a `case 2:`
-- | for an older protocol and a `case 7:` for this one, and reading the first
-- | branch cost a run: 221 preset records arrived, every one of them rejected,
-- | and the read then stopped early because its own progress counter had never
-- | moved. Hence `DumpFrame` rather than `Maybe DumpPreset` — a frame that
-- | cannot be turned into a preset is still evidence the device is talking.
-- |
-- | `F1=6, F2=1` is accepted too: that is how the device reports the preset it is
-- | currently sitting on, in exactly this format, and refusing free data because
-- | it arrived unasked would be perverse.
decodeDumpFrame :: Array Int -> Maybe DumpFrame
decodeDumpFrame bytes = do
  h0 <- Array.index bytes 0
  m1 <- Array.index bytes 1
  m2 <- Array.index bytes 2
  m3 <- Array.index bytes 3
  f1 <- Array.index bytes 6
  f2 <- Array.index bytes 7
  let f3 = fromMaybe 0 (Array.index bytes 8)
      payload = Array.drop 16 (Array.dropEnd 2 bytes)
  if h0 /= 0xF0 || m1 /= 0x00 || m2 /= 0x21 || m3 /= 0x24 then Nothing
  else if f1 /= 0x07 && f1 /= 0x06 then Nothing
  else case f2 of
    0x01 -> DumpPresetFrame <$> decodePresetPayload payload
    0x02 -> Just (DumpBankFrame f3)
    0x00 -> case f3 of
      0x00 -> Just DumpStarted
      -- Both completion codes end the wait. Which kind of dump finished is the
      -- device's business; ours is that it has stopped sending.
      0x01 -> Just DumpFinished
      0x02 -> Just DumpFinished
      _ -> Nothing
    _ -> Nothing

-- | Walk the TLV stream, folding each record into the preset.
-- |
-- | A record whose tag we do not know is *skipped by its own length* rather than
-- | abandoning the frame. The MC6 Pro sends colour and shift-name records this
-- | app has no use for, and a decoder that gave up on the first unfamiliar tag
-- | would read nothing from a device it could otherwise read almost all of.
decodePresetPayload :: Array Int -> Maybe DumpPreset
decodePresetPayload payload = go 0 empty
  where
  empty =
    { bankNumber: -1, presetNum: -1, isExp: false
    , shortName: "", toggleName: "", longName: ""
    , toToggle: false, toggleGroup: 0, messages: []
    }

  go i acc = case Array.index payload i of
    Nothing -> if acc.bankNumber < 0 then Nothing else Just (trim acc)
    Just 0x7F -> case Array.index payload (i + 1), Array.index payload (i + 2) of
      Just tag, Just len ->
        let body = Array.slice (i + 3) (i + 3 + len) payload
            next = i + 3 + len
        in go next (apply tag len body acc)
      _, _ -> if acc.bankNumber < 0 then Nothing else Just (trim acc)
    -- Out of step with the framing: keep whatever was already understood rather
    -- than discarding a mostly-good preset over a trailing byte.
    Just _ -> if acc.bankNumber < 0 then Nothing else Just (trim acc)

  apply tag len body acc = case tag of
    0 -> acc
      { bankNumber = at 0 body
      , presetNum = at 1 body
      , isExp = at 2 body /= 0
      }
    1 -> case decodeMessage len body of
      Just msg -> acc { messages = Array.snoc acc.messages msg }
      Nothing -> acc
    2 -> acc { shortName = ascii body }
    3 -> acc { toggleName = ascii body }
    4 -> acc { longName = ascii body }
    5 -> acc { toToggle = at 0 body /= 0, toggleGroup = at 3 body }
    _ -> acc

  at n arr = fromMaybe 0 (Array.index arr n)

  -- A preset always arrives with sixteen message slots, most of them empty:
  -- that is the shape of the record, not a claim that the switch sends sixteen
  -- things. Our own encoder pads the same way, which is how this surfaced.
  --
  -- Only *trailing* empties go. An empty slot between two real messages is
  -- inert on the device but its position is what keeps the ones after it at
  -- their own indices, and closing the gap would silently renumber a preset we
  -- are supposed to be reading rather than editing.
  trim p = p { messages = dropTrailingEmpty p.messages }

dropTrailingEmpty :: Array MC6Message -> Array MC6Message
dropTrailingEmpty msgs = case Array.last msgs of
  Just m | m.msgType == MsgEmpty ->
    dropTrailingEmpty (fromMaybe [] (Array.init msgs))
  _ -> msgs

-- | One message record.
-- |
-- | Two lengths in the wild and the difference is not a suffix: at length 8 there
-- | is no `data4` at all, and at length 9 it arrives *last*, after the toggle
-- | byte rather than beside the other data bytes. Reading the nine-byte form as
-- | "the eight-byte form plus one" would put the channel, action and toggle each
-- | one place out — every message decoded, none of them correct.
decodeMessage :: Int -> Array Int -> Maybe MC6Message
decodeMessage len body =
  let at n = fromMaybe 0 (Array.index body n)
      base =
        { msgIndex: at 0
        , msgType: intToMC6MsgType (at 1)
        , data1: at 2
        , data2: at 3
        , data3: at 4
        , data4: 0
        , channel: at 5
        , action: intToMC6Action (at 6)
        , togglePosition: intToMC6Toggle (at 7)
        }
  in if len == 8 then Just base
     else if len >= 9 then Just (base { data4 = at 8 })
     else Nothing

-- | Names come back space-padded to their field width.
ascii :: Array Int -> String
ascii = Str.trim <<< SCU.fromCharArray <<< Array.mapMaybe fromCharCode

-- | Gather decoded presets into banks, in the shape the rest of the app uses.
-- |
-- | Expression presets are dropped here rather than in the decoder, because this
-- | is where the twelve-switch assumption actually lives. Presets are ordered by
-- | their own number rather than by arrival, since nothing in the protocol
-- | promises an order and a bank whose switches came back shuffled would be
-- | wrong in a way that looks like a device fault.
presetsToBanks :: Array DumpPreset -> Array MC6NativeBank
presetsToBanks presets =
  map toBank (Map.toUnfoldable grouped)
  where
  grouped :: Map.Map Int (Array DumpPreset)
  grouped = Map.fromFoldableWith (<>)
    (map (\p -> Tuple p.bankNumber [ p ]) (Array.filter (not <<< _.isExp) presets))

  toBank (Tuple bankNumber ps) =
    { bankNumber
    , bankName: ""
    , bankClearToggle: false
    , presets: map toPreset (Array.sortWith _.presetNum ps)
    }

  toPreset :: DumpPreset -> MC6Preset
  toPreset p =
    { presetNum: p.presetNum
    , shortName: p.shortName
    , toggleName: p.toggleName
    , longName: p.longName
    , toToggle: p.toToggle
    , toggleGroup: p.toggleGroup
    , messages: p.messages
    }
