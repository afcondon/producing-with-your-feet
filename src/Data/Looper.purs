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
import Data.Looper.Banks (Duty(..), Subject(..))
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
  -- | Ask the machine for this, about this loop.
  -- |
  -- | **A `Duty`, not a `Verb`, since 2026-08-25.** This table used to name
  -- | verbs directly and go straight to the socket, which made it a *second
  -- | meaning table* running beside `Data.Looper.Machine` — and the page, which
  -- | is the reference surface, was the half on the wrong side of it. Two
  -- | consequences that were live bugs rather than untidiness:
  -- |
  -- | * it rendered **bare**, so every per-loop command from the page reached
  -- |   whichever loop the *daemon* had selected rather than the focused one.
  -- |   That fault is documented on `Verb.SaveTake`, where it was found and
  -- |   fixed for one verb; it was true of every verb on this table.
  -- | * the two vocabularies had drifted. `Multiply` existed only here, so the
  -- |   page could ask for something the machine had no word for; `redo` and
  -- |   `loop save` were marked unimplemented here while the machine had been
  -- |   doing both for weeks.
  -- |
  -- | So this is now an **addressing** table, not a meaning one: it says which
  -- | duty a CC names, and `Data.Looper.Machine.perform` says what that means.
  -- | It keeps its job because the MC6 assignment UI, board presets and the
  -- | pedal face all index Itajara by CC — `DESIGN-LOOPER` §2 is still right
  -- | that a virtual pedal addressed by CC buys all of that for free.
  = Do Subject Duty
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
-- |
-- | Everything here addresses `Focused`. That is not a shortcut — the pedal
-- | face, the MC6 and a board preset all speak about *the loop in hand*, and
-- | the only surface that names a loop while acting on it is the Twister, which
-- | does not come through this table.
command :: CC -> MidiValue -> Dispatch
command theCC val = case unCC theCC of
  -- Transport
  1 -> onPress (Do Focused RecordLoop)
  2 -> onPress (Do Focused MultiplyLoop)
  3 -> onPress (Do Focused Undo)
  -- **Was marked unimplemented here while the machine was already doing it.**
  -- `Redo` has had a duty and a `y` in `dispatch` since undo started keeping
  -- what it removes; only this table had not heard.
  4 -> onPress (Do Focused Redo)
  5 -> onPress (Do Focused ClaimPast)
  6 -> onPress (Do Focused ClearLoop)
  -- **`Transport` is implemented and this CC still cannot carry it.** CC 7 is
  -- Itajara's `SingleEngage` CC, so the app sends it by itself whenever a board
  -- preset is recalled — and a bypass that stopped or started the focused loop
  -- would be the machine confidently doing something nobody asked for. Stop/Go
  -- is reachable from the MC6 loop page, from the Twister and from the page's
  -- own button; it is this *CC* that is spoken for, not the duty.
  7 -> NotYetImplemented "play/stop on CC 7 — that CC is the pedal's engage"
  8 -> NotYetImplemented "global reverse"
  9 -> NotYetImplemented "global half speed"

  -- The second multiply. Where `x` asks "how many bars of this?", these ask
  -- "how often?" and answer by leaving room: the layer keeps its length and the
  -- loop grows around it. Structural rather than recorded, so they cost no bars
  -- and Dense puts it back.
  10 -> onPress (Do Focused (SpreadLoop 2))
  11 -> onPress (Do Focused RotateLoop)
  12 -> onPress (Do Focused DenseLoop)

  -- Undo keeps the length on purpose, so there has to be a way to let go of it.
  -- Three erasures, deliberately separate: undo a layer, forget the length,
  -- clear both.
  13 -> onPress (Do Focused ForgetLength)

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
  -- The other one this table had marked missing. `w` writes the layers out and
  -- the daemon answers with where it put them.
  73 -> onPress (Do Focused SaveTake)
  74 -> onPress (NotYetImplemented "loop load")

  -- Global. **Set from the value, never flipped.** These two are the CCs an MC6
  -- *native toggle* lands on — 127 and 0 on alternate presses — so the value is
  -- the instruction. `ClickToggle` is the duty for a surface with no value to
  -- send; this is the one with.
  80 -> NotYetImplemented "loop level"
  81 -> Do Focused (Click on)
  83 -> Do Focused (Monitor on)
  82 -> NotYetImplemented "click level"

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
    | n >= 10 && n <= 13 -> true
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
