-- | Itajara's control surface, and the one place that turns a CC into a command.
-- |
-- | The looper is a pedal in the registry like any other (`DESIGN-LOOPER` §2).
-- | The single thing that distinguishes it is transport: its CCs go to a
-- | WebSocket rather than a MIDI port. So `SetValue` asks this module whether a
-- | pedal is Itajara, and if so what the CC means.
-- |
-- | Everything follows from that. The MC6 sends channel 13 exactly as it sends
-- | channel 15 for Habit; the app relays it to `SetValue`; and the assignment
-- | UI, board presets, the Twister and the donut view all work unchanged
-- | because none of them ever knew what a CC talked to.
-- |
-- | The map here is the subset the daemon implements today. Everything else in
-- | the surface is real in the model — it shows on the pedal, it stores in a
-- | board, it can be assigned to a footswitch — and returns `Nothing` here
-- | until the engine grows the feature.
module Data.Looper
  ( itajaraId
  , itajaraChannel
  , isItajara
  , command
  , isMomentary
  , looperBank
  , Dispatch(..)
  ) where

import Prelude

import Data.Array as Array
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..))
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch, ccMomentaryMessages, ccToggleMessages)
import Data.Midi (CC, MidiValue, unCC, unMidiValue)
import Data.Pedal (PedalId(..))

itajaraId :: PedalId
itajaraId = PedalId "itajara"

-- | Free channels are 9, 13 and 16 — the pedals hold 2–8, 10–12, 14 and 15,
-- | and channel 1 is the app's own MC6 board-recall relay.
itajaraChannel :: Int
itajaraChannel = 13

isItajara :: PedalId -> Boolean
isItajara pid = pid == itajaraId

-- | What a CC change should do.
data Dispatch
  -- | Send this string to the daemon.
  = Send String
  -- | In the surface, not yet in the engine. Carries its own name so the log
  -- | says which feature is missing rather than just refusing.
  | NotYetImplemented String
  -- | The release half of a momentary, or a value the daemon does not need.
  | Ignore

derive instance Eq Dispatch

-- | Momentary controls act on 127 and ignore 0, so a footswitch's release
-- | message costs nothing.
-- |
-- | `Component.App` also mirrors the daemon's reported `click` and `monitor`
-- | back into pedal state on every poll: for the things the engine owns, the
-- | snapshot is authoritative and the app follows it rather than the reverse.
command :: CC -> MidiValue -> Dispatch
command theCC val = case unCC theCC of
  -- Transport
  1 -> onPress (Send "r")
  2 -> onPress (Send "x")
  3 -> onPress (Send "u")
  4 -> onPress (NotYetImplemented "redo — undo currently wipes the layer rather than unlinking it")
  5 -> onPress (Send "t")
  6 -> onPress (Send "c")
  7 -> NotYetImplemented "play/stop"
  8 -> NotYetImplemented "global reverse"
  9 -> NotYetImplemented "global half speed"

  -- Source and routing
  20 -> NotYetImplemented "record source select"
  21 -> NotYetImplemented "record width"
  22 -> NotYetImplemented "monitor source select"
  23 -> NotYetImplemented "monitor level"
  24 -> NotYetImplemented "send destination"
  25 -> NotYetImplemented "send source"

  -- Selected layer
  60 -> NotYetImplemented "layer select"
  61 -> onPress (NotYetImplemented "layer next")
  62 -> onPress (NotYetImplemented "layer previous")
  63 -> NotYetImplemented "solo"
  64 -> NotYetImplemented "layer source"
  65 -> NotYetImplemented "layer pan"
  66 -> NotYetImplemented "layer width"
  67 -> NotYetImplemented "layer reverse"
  68 -> NotYetImplemented "layer half speed"

  -- Loops
  70 -> NotYetImplemented "loop select"
  71 -> onPress (NotYetImplemented "loop next")
  72 -> onPress (NotYetImplemented "loop previous")
  73 -> onPress (NotYetImplemented "loop save")
  74 -> onPress (NotYetImplemented "loop load")

  -- Global. The explicit `k1`/`k0` forms rather than the flipping `k`, so a
  -- dropped command cannot leave the app and the engine disagreeing forever.
  80 -> NotYetImplemented "loop level"
  81 -> Send (if on then "k1" else "k0")
  82 -> NotYetImplemented "click level"
  83 -> Send (if on then "m1" else "m0")

  n | n >= 40 && n <= 47 -> NotYetImplemented ("layer " <> show (n - 39) <> " mute")
    | n >= 48 && n <= 55 -> NotYetImplemented ("layer " <> show (n - 47) <> " level")
    | otherwise -> Ignore

  where
  on = unMidiValue val > 63
  onPress d = if on then d else Ignore

-- | Momentary controls are gestures, not state. Without snapping the stored
-- | value back to zero, `Rec` would stay lit on the pedal face forever after
-- | one press, and a board preset would recall a permanent record command.
isMomentary :: CC -> Boolean
isMomentary theCC = case unCC theCC of
  n | n >= 1 && n <= 6 -> true
    | n == 61 || n == 62 -> true
    | n >= 71 && n <= 74 -> true
    | otherwise -> false

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
    -- Written blank rather than left alone, so reprogramming leaves no
    -- stragglers from whatever the bank held before.
    _ -> { label: "", longName: "", toToggle: false, messages: [] }

  gesture label longName theCC =
    { label, longName, toToggle: false, messages: ccMomentaryMessages ch theCC }

  latching label longName theCC =
    { label, longName, toToggle: true, messages: ccToggleMessages ch theCC }
