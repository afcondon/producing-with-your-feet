-- | The MC6's controller settings, decoded.
-- |
-- | On connect the device volunteers ten frames, `F1=0x03` with `F2` running
-- | `0x20` to `0x29`, carrying everything about itself that is not a preset:
-- | the channel table, the omniport configuration, the waveform and sequencer
-- | engines, the scroll counters, the aux switch ladder, the MIDI event map.
-- | `Data.MC6.Read` has always received these, acknowledged them, and thrown
-- | the payloads away.
-- |
-- | This decodes them. Nothing here required a device: it was written against
-- | `test/mc6-connect-dump-20260816.json`, already in the repo, and every
-- | section was checked field by field against the March backup's
-- | `controller_settings` — two independently produced descriptions of the same
-- | hardware, which is what makes these confirmations rather than readings.
-- |
-- | ## Why it matters more than the features it unlocks
-- |
-- | `sysexPresetData` writes presets and nothing else writes anything. Every
-- | section here can be read and not written, so a factory reset is currently
-- | not reversible by this app — most acutely the omniports, whose two entries
-- | are what make the FS3X switches exist at all. Reading them is the first
-- | half of fixing that, and the half that costs nothing.
-- |
-- | It also removes a class of bug this project keeps hitting. The loop banks
-- | took channel 16 on the strength of a comment saying it was free; the
-- | device's own channel table says otherwise, and nothing in the app could
-- | consult it. Now something can.
-- |
-- | ## Two payload shapes
-- |
-- | Large sections are **TLV** — a run of `7F <index> <length> <bytes>` from
-- | offset 16, the same framing `SysEx.purs` writes. Small ones are **flat**: a
-- | count byte, then that many fixed-width records, then zero padding.
-- |
-- | The count is trusted for how many records to expect and *not* for how many
-- | are there, which is the difference between a decoder and a wish. Where the
-- | frame holds fewer than it claims, `AuxSwitches` reports it — and it does,
-- | on the real capture, by exactly one byte. That discrepancy is left visible
-- | rather than rounded away, because it is either a fact about the protocol we
-- | have not understood or a fact about our offsets, and both are worth knowing.
module Data.MC6.Settings
  ( Section(..)
  , ChannelEntry
  , Omniport
  , WaveformEngine
  , SequencerEngine
  , ScrollCounter
  , AuxSwitches
  , MidiEvent
  , GeneralConfig
  , decodeSection
  , sectionName
  , longPressSetting
  , bankChangeDisplayTimeProbably
  ) where

import Prelude

import Data.Array as Array
import Data.MC6.Read (tlvs, trimAscii)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

-- | One MIDI channel as the device describes it.
-- |
-- | `sendToPort` is an eleven-bit mask of which physical ports a message on
-- | this channel goes out of, carried as two septets. Every channel on this
-- | board reads 2047 — all ports — except channel 16, which reads 2034.
type ChannelEntry =
  { channel :: Int
  , name :: String
  , sendToPort :: Int
  , remap :: Int
  }

-- | An omniport, and what a switch plugged into it sends.
-- |
-- | `portType` 8 is the three-switch mode an FS3X uses; tip, ring and both-at-
-- | once each get a fixed switch number. This is the setting that makes
-- | switches G through L exist, and losing it is what would make a factory
-- | reset unrecoverable.
type Omniport =
  { portNum :: Int
  , portType :: Int
  , tip :: Array Int
  , ring :: Array Int
  , tipRing :: Array Int
  }

type WaveformEngine = { num :: Int, min :: Int, max :: Int, waveform :: Int }

-- | A sixteen-step sequence and how many of its steps are used.
type SequencerEngine = { len :: Int, steps :: Array Int }

type ScrollCounter = { min :: Int, max :: Int, start :: Int }

-- | The resistor-ladder aux switches, plus whether the frame carried as many
-- | as its count byte promised.
type AuxSwitches =
  { switches :: Array { num :: Int, trigger :: Int, f1 :: Int, f2 :: Int }
  , claimed :: Int
  , truncated :: Boolean
  }

-- | One entry of the MIDI event map: a range in, a range out, three flags.
type MidiEvent =
  { numberFrom :: Int, numberTo :: Int
  , channelFrom :: Int, channelTo :: Int
  , typeFrom :: Int, typeTo :: Int
  , valueFrom :: Int, valueTo :: Int
  , flags :: Array Int
  }

-- | The general configuration, as bytes plus the fields we can point at.
-- |
-- | Deliberately not a record of sixteen named fields. The backup names sixteen
-- | settings and this payload is thirty-two bytes; naming them all from one
-- | capture would be inventing a layout, since a 0 or a 1 appears a dozen times
-- | over and matching a field by the uniqueness of its value is guessing with
-- | extra steps. Exactly one field here was placed that way and exactly one
-- | field here turned out to be wrong.
-- |
-- | Pinning the rest costs one experiment each, and the experiment is cheap and
-- | read-only: change one setting in Morningstar's editor, read the device
-- | before and after, and diff. Whichever byte moved is that field. Two reads
-- | minutes apart beat two reads three days apart, which is how offset 13 got
-- | away with being wrong for a while.
-- |
-- | **Not every byte here is the editor's to set.** Offsets 7 and 8 read `8, 8`
-- | on the device, the editor's write put `0, 0` in them, and a read afterwards
-- | showed `8, 8` still. So a settings write is not a blind overwrite of the
-- | block — the device keeps what the writer does not own, which makes writing
-- | this back considerably less frightening than it first looked.
type GeneralConfig = { bytes :: Array Int }

-- | The long-press setting, at **offset 3**, as the device stores it.
-- |
-- | Confirmed by a controlled change rather than by a value match: the setting
-- | was moved from 750 ms to 700 ms in Morningstar's editor with the write
-- | captured, and this byte is the one that moved — 2 in the August read, 4 in
-- | the editor's write, and 4 when the device was read back afterwards.
-- |
-- | **This is not milliseconds.** 4 is 700 ms; one data point does not give the
-- | scale, and inventing one would be inventing a number. Returned raw for that
-- | reason, with the conversion left as the small experiment it is: set two
-- | known values, read twice.
-- |
-- | It used to matter because the app had a hold timer of its own that had to
-- | agree with this one, and between 600 here and 700 there a press meant to
-- | close a recording did nothing visible and stranded it. There is no app-side
-- | timer any more — the device recognises its own gestures
-- | (`Data.Looper.Banks`) — so the two cannot disagree. What this number is
-- | wanted for now is `Engine.looperDeferral`: a long press is reported at the
-- | threshold, so the threshold *is* how late the message is, and the daemon
-- | spends that on the pre-roll ring.
-- |
-- | The **double-tap window is the one still missing**, and it is the one the
-- | common gesture needs. Bounded from above at 414 ms by the gesture probe and
-- | never pinned. Same experiment, one byte along.
-- |
-- | **Offset 13 was the previous answer here and was wrong.** It was placed
-- | because 12 occurs exactly once in the payload and the March backup names a
-- | `longPressTime: 12`. It reads 12 in all three captures — including one
-- | taken with the setting at a different value — so the agreement was a
-- | coincidence. Matching a field by the uniqueness of its value is guessing
-- | with extra steps, which was said at the time and then done anyway.
longPressSetting :: GeneralConfig -> Maybe Int
longPressSetting g = Array.index g.bytes 3

-- | Offset 6, which is 60 in every capture and which the March backup calls
-- | `bankChangeDisplayTime: 60`.
-- |
-- | Left in because it is useful and marked here because it rests on exactly
-- | the evidence that just turned out to be worthless for offset 13: a unique
-- | value agreeing with a name. Believe it no further than that until a
-- | controlled change moves it.
bankChangeDisplayTimeProbably :: GeneralConfig -> Maybe Int
bankChangeDisplayTimeProbably g = Array.index g.bytes 6

-- | A settings section, or an honest refusal to claim one.
data Section
  = MidiChannels (Array ChannelEntry)
  | General GeneralConfig
  -- | `03 22`. The payload is `0, 29`, nine zeros, then 1 to 29, which fits a
  -- | bank ordering and fits nothing else we know of — but the leading pad is
  -- | unexplained, so this is inference and is named as such.
  | BankOrderProbably (Array Int)
  | Omniports (Array Omniport)
  | WaveformEngines (Array WaveformEngine)
  | SequencerEngines (Array SequencerEngine)
  | ScrollCounters (Array ScrollCounter)
  | MidiEvents (Array MidiEvent)
  | AuxLadder AuxSwitches
  -- | A settings frame we do not decode, with its sub-code and payload intact.
  -- | `0x29` is the one that remains; anything a firmware update adds will land
  -- | here too rather than disappearing.
  | UnknownSettings Int (Array Int)

derive instance Eq Section

sectionName :: Section -> String
sectionName = case _ of
  MidiChannels _ -> "MIDI channels"
  General _ -> "general configuration"
  BankOrderProbably _ -> "bank order (probably)"
  Omniports _ -> "omniports"
  WaveformEngines _ -> "waveform engines"
  SequencerEngines _ -> "sequencer engines"
  ScrollCounters _ -> "scroll counters"
  MidiEvents _ -> "MIDI events"
  AuxLadder _ -> "aux switch ladder"
  UnknownSettings f2 _ -> "unknown settings frame 03 " <> show f2

-- | Decode one `03 2x` payload, given its sub-code.
-- |
-- | The payload is what `Data.MC6.Read` already extracts — `drop 16`, `dropEnd
-- | 2` — so this takes over exactly where that leaves off.
decodeSection :: Int -> Array Int -> Section
decodeSection f2 payload = case f2 of
  0x20 -> MidiChannels (channels payload)
  0x21 -> General { bytes: payload }
  0x22 -> BankOrderProbably payload
  0x23 -> Omniports (Array.mapMaybe omniport (counted 11 payload))
  0x24 -> WaveformEngines (Array.mapMaybe waveform (counted 4 payload))
  0x25 -> SequencerEngines (sequencers payload)
  0x26 -> ScrollCounters (Array.mapMaybe scroll (counted 3 payload))
  0x27 -> MidiEvents (Array.mapMaybe midiEvent (map (\(Tuple _ v) -> v) (tlvs payload)))
  0x28 -> AuxLadder (auxLadder payload)
  _ -> UnknownSettings f2 payload

-- | Sixteen names, then sixteen port masks. The trailing sixteen sixteen-byte
-- | TLVs are not understood and are not pretended to be; they are simply not
-- | read, which leaves them where they are rather than half-claimed.
channels :: Array Int -> Array ChannelEntry
channels payload =
  Array.mapWithIndex entry (Array.take 16 (map (\(Tuple _ v) -> v) (tlvs payload)))
  where
  masks = Array.slice 16 32 (map (\(Tuple _ v) -> v) (tlvs payload))

  entry i name =
    { channel: i + 1
    , name: trimAscii name
    -- Two septets, high first, after a leading zero. 15 * 128 + 127 is 2047,
    -- which is what the backup calls `sendToPort` for every channel but one.
    , sendToPort: case Array.index masks i of
        Just m -> 128 * at 1 m + at 2 m
        Nothing -> 0
    , remap: fromMaybe 0 (Array.index masks i >>= \m -> Array.index m 3)
    }

  at n m = fromMaybe 0 (Array.index m n)

omniport :: Array Int -> Maybe Omniport
omniport r = do
  portNum <- Array.index r 0
  portType <- Array.index r 1
  pure { portNum, portType
       , tip: Array.slice 2 5 r, ring: Array.slice 5 8 r, tipRing: Array.slice 8 11 r }

waveform :: Array Int -> Maybe WaveformEngine
waveform r = do
  num <- Array.index r 0
  mn <- Array.index r 1
  mx <- Array.index r 2
  wf <- Array.index r 3
  pure { num, min: mn, max: mx, waveform: wf }

-- | Not a fixed-width record: each engine is a length byte followed by sixteen
-- | steps, and the payload opens with a count and one byte this decoder cannot
-- | place. Taking the steps from the end of each 17-byte group is what makes
-- | engine 0 match the backup's array byte for byte.
sequencers :: Array Int -> Array SequencerEngine
sequencers payload = case Array.uncons payload of
  Nothing -> []
  Just { head: n, tail } ->
    Array.mapMaybe engine (Array.take n (chunk 17 (Array.drop 1 tail)))
  where
  engine g = do
    len <- Array.index g 0
    pure { len, steps: Array.take 16 (Array.drop 1 g) }

scroll :: Array Int -> Maybe ScrollCounter
scroll r = do
  mn <- Array.index r 0
  mx <- Array.index r 1
  st <- Array.index r 2
  pure { min: mn, max: mx, start: st }

midiEvent :: Array Int -> Maybe MidiEvent
midiEvent r = do
  numberFrom <- Array.index r 0
  numberTo <- Array.index r 1
  channelFrom <- Array.index r 2
  channelTo <- Array.index r 3
  typeFrom <- Array.index r 4
  typeTo <- Array.index r 5
  valueFrom <- Array.index r 6
  valueTo <- Array.index r 7
  pure { numberFrom, numberTo, channelFrom, channelTo
       , typeFrom, typeTo, valueFrom, valueTo, flags: Array.slice 8 11 r }

-- | Reports what it found *and* what it was promised, because on the real
-- | capture those differ by one byte and rounding that away would turn an
-- | unexplained fact into a silent one.
auxLadder :: Array Int -> AuxSwitches
auxLadder payload = case Array.uncons payload of
  Nothing -> { switches: [], claimed: 0, truncated: false }
  Just { head: claimed, tail } ->
    let got = Array.mapMaybe one (Array.take claimed (chunk 4 tail))
    in { switches: got, claimed, truncated: Array.length got < claimed }
  where
  one r = do
    num <- Array.index r 0
    trigger <- Array.index r 1
    f1 <- Array.index r 2
    f2 <- Array.index r 3
    pure { num, trigger, f1, f2 }

-- | A count byte, then that many fixed-width records. The count bounds the
-- | result, so the zero padding that follows a short section does not become a
-- | run of empty records.
counted :: Int -> Array Int -> Array (Array Int)
counted width payload = case Array.uncons payload of
  Nothing -> []
  Just { head: n, tail } -> Array.take n (chunk width tail)

-- | Only whole records. A trailing partial one is not a record, and treating it
-- | as one is how a decoder starts returning zeros it was never told.
chunk :: Int -> Array Int -> Array (Array Int)
chunk width = go []
  where
  go acc xs
    | Array.length xs < width = acc
    | otherwise = go (Array.snoc acc (Array.take width xs)) (Array.drop width xs)
