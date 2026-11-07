-- | Itajara as a pedal on this app's board: who it is, its channel, and the
-- | MC6 bank that drives it.
-- |
-- | The looper is a pedal in the registry like any other (`DESIGN-LOOPER` §2).
-- | The single thing that distinguishes it is transport: its CCs go to a
-- | WebSocket rather than a MIDI port. So `SetValue` asks this module whether a
-- | pedal is Itajara, and itajara's own table (`Data.Looper.PedalCC`) what the
-- | CC names.
-- |
-- | Everything follows from that. The MC6 sends channel 13 exactly as it sends
-- | channel 15 for Habit; the app relays it to `SetValue`; and the assignment
-- | UI, board presets, the Twister and the donut view all work unchanged
-- | because none of them ever knew what a CC talked to.
-- |
-- | **This app kept its own copy of that table until 2026-11-07**, typed over
-- | its `CC` and `MidiValue`. It went when the looper's page left for Quadrat
-- | (`quadrat-the-sampler.md`, phase 4): one table, in the looper's repo, read
-- | by every surface that speaks to the looper in CCs.
module Data.Looper
  ( itajaraId
  , itajaraChannel
  , isItajara
  , looperBank
  ) where

import Prelude

import Data.Array as Array
import Data.Looper.PedalCC as PedalCC
import Helm.MC6.Message as MC6Msg
import Helm.MC6.Types (MC6Action(..))
import Helm.MC6.ControlBank (ControlBank, ControlBankSwitch, ccMomentaryMessages, ccToggleMessages)
import Data.Pedal (PedalId(..))

itajaraId :: PedalId
itajaraId = PedalId "itajara"

-- | Free channels are 9, 13 and 16 — the pedals hold 2–8, 10–12, 14 and 15,
-- | and channel 1 is the app's own MC6 board-recall relay. The looper's repo
-- | holds the number, so the bank written here and the table that reads it
-- | cannot disagree.
itajaraChannel :: Int
itajaraChannel = PedalCC.channel

isItajara :: PedalId -> Boolean
isItajara pid = pid == itajaraId

-- | A starter MC6 bank, on Itajara's own channel.
-- |
-- | Not the design — the design is that you assign whatever you like from the
-- | surface above and change your mind often. This is the bank that lets you
-- | stomp today, and it deliberately covers only what the engine implements.
-- |
-- | Momentary for the gestures, so the release message is harmlessly ignored.
-- | Native toggle for click and monitor, so the MC6's alternating 127/0 lands
-- | on the explicit `k1`/`k0` pair rather than flipping twice per press.
-- |
-- | Presets 0–5 are the MC6's own switches A–F; 6–8 are the first FS3X as
-- | G/H/I; 9–11 would be a second one.
looperBank :: Int -> Int -> ControlBank
looperBank bankNum returnBankNum =
  { id: "itajara"
  , name: "Itajara"
  , description: "Looper transport on channel " <> show itajaraChannel
  , mc6BankNumber: bankNum
  , returnSwitchIndex: 5
  , switches: Array.mapWithIndex switchAt (Array.replicate 12 unit)
  }
  where
  ch = itajaraChannel

  switchAt :: Int -> Unit -> ControlBankSwitch
  switchAt idx _ = case idx of
    0 -> gesture "Rec" "Looper Record/Overdub" 1
    1 -> gesture "Multiply" "Looper Multiply" 2
    2 -> gesture "Take" "Looper Take" 5
    3 -> gesture "Undo" "Looper Undo Layer" 3
    4 -> gesture "Clear" "Looper Clear All" 6
    -- Carries the jump itself. It used to be an empty switch that the
    -- compiler filled in, which meant the generated bank did not say what one
    -- of its six switches did.
    5 -> { label: "< Back", longName: "Back to Board Bank"
         , toToggle: false
         , messages: [ MC6Msg.bankJumpMessage returnBankNum ActionPress ] }
    6 -> latching "Click" "Looper Click" 81
    7 -> latching "Monitor" "Looper Input Monitor" 83
    -- Spread and Shift sit together because they are used together: spread to
    -- make room, shift to decide where in it the bar falls. Dense is beside them
    -- as the way back, which matters more than it sounds — a gesture you cannot
    -- undo with your foot is one you will not try mid-take.
    8 -> gesture "Spread" "Looper Spread One In Two" 10
    9 -> gesture "Shift" "Looper Shift One Slot" 11
    10 -> gesture "Dense" "Looper Sound Every Cycle" 12
    -- On the second FS3X, which may not exist (DESIGN-CONTROLS §10.5). It is the
    -- least urgent of the three erasures underfoot: undo and clear are both on
    -- the MC6 itself, and forgetting the length is a between-takes decision.
    11 -> gesture "Length" "Looper Forget The Length" 13
    -- Written blank rather than left alone, so reprogramming leaves no
    -- stragglers from whatever the bank held before.
    _ -> { label: "", longName: "", toToggle: false, messages: [] }

  gesture label longName theCC =
    { label, longName, toToggle: false, messages: ccMomentaryMessages ch theCC }

  latching label longName theCC =
    { label, longName, toToggle: true, messages: ccToggleMessages ch theCC }
