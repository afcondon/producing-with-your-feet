-- | What a gesture means, given what the loops are doing.
-- |
-- | The second half of the input path. `Data.Looper.Gestures` is a `Mealy` and
-- | has memory; this has none, and that is the interesting part.
-- |
-- | ## It holds no state, because the engine already does
-- |
-- | A looper's phase — empty, recording, playing — is not something this app
-- | should model. The daemon reports it thirty times a second and the app has
-- | always taken that as authoritative (`Foreign.LooperSocket`: "the app holds
-- | only what the daemon last reported; it never models the engine itself").
-- |
-- | So the machine's own state is only what nothing else knows: whether a
-- | second tap is still possible, and how long a switch has been down. That is
-- | the recogniser's, and it is all of it. Here there is a pure function from
-- | (gesture, engine truth) to actions — which means it can be tested by
-- | enumeration, and cannot drift out of step with the engine, because it has
-- | nothing of its own to drift with.
-- |
-- | The same argument as the MC6: *the device is compiled output, the store is
-- | the truth.* Here the daemon is the store.
-- |
-- | ## Gaps are named, never substituted
-- |
-- | The plan wants a tap on a playing loop to **pause** it. The engine has no
-- | play or stop: `r` on a playing loop starts an overdub, and there is no
-- | other transport command in `dispatch`. So a tap there yields `Unavailable`
-- | with the missing thing named, rather than overdubbing — which would be the
-- | machine confidently doing something the player did not ask for, the exact
-- | failure this project keeps meeting.
-- |
-- | `Unavailable` is `Refused` from the Glassbox spec, earning its place before
-- | the library exists: an outcome that is neither a command nor silence, and
-- | that the display can show.
module Data.Looper.Machine
  ( Action(..)
  , Rig
  , act
  , describe
  ) where

import Prelude

import Data.Array as Array
import Data.Looper.Banks (BankSlot(..), loopSwitches)
import Data.Looper.Gestures (Gesture(..))
import Data.Maybe (Maybe(..))
import Foreign.LooperSocket (LoopState)

-- | What the app should do about a gesture.
data Action
  -- | A command string for the daemon's socket — the same strings the console
  -- | takes, so a footswitch and a typed command cannot come to mean different
  -- | things.
  = Command String
  -- | Ask the MC6 to show a bank. A courtesy: audio never waits on it.
  | ShowBank BankSlot
  -- | Remember which loop was last touched, so the config bank knows its
  -- | subject. App state, not the machine's.
  | Focus Int
  -- | Understood, and impossible for a stated reason.
  | Unavailable String
  -- | Understood, and deliberately nothing. Distinct from `Unavailable`: this
  -- | is a press whose job the MC6 does by itself.
  | Handled String

derive instance Eq Action

-- | Everything the machine is allowed to know.
type Rig =
  { loops :: Array LoopState
  -- | The loop the config bank acts on: the last one touched.
  , focus :: Int
  }

-- | The whole meaning table.
act :: Rig -> Gesture -> Array Action
act rig = case _ of

  Tap LoopBank i | i < loopSwitches -> Array.cons (Focus i) (onTap i (loopAt rig i))
  DoubleTap LoopBank i | i < loopSwitches -> Array.cons (Focus i) (onDouble i (loopAt rig i))
  -- The MC6 jumps to the config bank on its own long press; all this has to do
  -- is agree about which loop that bank is now talking about.
  Hold LoopBank i | i < loopSwitches ->
    [ Focus i, Handled ("configuring loop " <> show (i + 1)) ]

  Tap LoopBank 6 -> [ Handled "back to the board bank" ]
  -- Stop all, as six commands rather than one. There is no all-loops form in
  -- the daemon's dispatch, and inventing one for a gesture that is not
  -- sample-critical would be protocol for its own sake — these land within a
  -- millisecond of each other, and stopping is not a downbeat.
  Tap LoopBank 7 -> map (\i -> Command (cmd i "h0")) (Array.range 0 (loopSwitches - 1))
  Tap LoopBank 8 -> [ Command (cmd rig.focus "u") ]
  Tap LoopBank 9 -> [ Command (cmd rig.focus "c") ]
  Tap LoopBank 10 -> [ Command "w" ]
  Tap LoopBank 11 -> [ Command "k" ]

  -- The config bank is real on the device and not yet wired here. Naming each
  -- press beats swallowing it: a switch that reports "not wired" is debuggable,
  -- and one that does nothing is indistinguishable from a broken cable.
  Tap ConfigBank i -> [ Unavailable ("config switch " <> show i <> " is not wired yet") ]
  Tap slot i -> [ Unavailable (show' slot <> " switch " <> show i <> " is not wired yet") ]

  DoubleTap slot i -> [ Handled ("double tap on " <> show' slot <> " " <> show i) ]
  Hold slot i -> [ Handled ("hold on " <> show' slot <> " " <> show i) ]


-- | A tap on a loop switch, given what that loop is doing.
-- |
-- | Note that every branch is a *state the daemon reported*, not one we kept.
onTap :: Int -> Maybe LoopState -> Array Action
onTap i = case _ of
  Nothing -> [ Unavailable ("loop " <> show (i + 1) <> " is not in the snapshot") ]
  Just st -> case st.state of
    -- Empty: start. Quantised loops answer with "starts on the grid in N s",
    -- which is why the display shows a countdown rather than nothing.
    "idle" | st.layers == 0 -> [ Command (cmd i "r") ]
    -- Recording the first layer: close it. This is the one place the app drives
    -- a bank change, because only the app knows this was the second press.
    "recordingFirst" -> [ Command (cmd i "r"), ShowBank ConfigBank ]
    "overdubbing" -> [ Command (cmd i "r") ]
    "multiplying" -> [ Command (cmd i "r") ]
    -- Stop and start. Explicit `h0`/`h1` rather than the flipping `h`, so a
    -- dropped command cannot leave the app and the engine disagreeing about
    -- something a stopped loop makes invisible by definition.
    "playing" | st.muted -> [ Command (cmd i "h1") ]
    "playing" -> [ Command (cmd i "h0") ]
    _ | st.layers > 0 && st.muted -> [ Command (cmd i "h1") ]
    _ | st.layers > 0 -> [ Command (cmd i "h0") ]
    _ -> [ Unavailable ("loop " <> show (i + 1) <> " has nothing to play") ]

-- | A double tap: overdub, per the plan.
onDouble :: Int -> Maybe LoopState -> Array Action
onDouble i = case _ of
  Nothing -> [ Unavailable ("loop " <> show (i + 1) <> " is not in the snapshot") ]
  Just st -> case st.state of
    -- Overdub. A stopped loop is brought back first, because overdubbing onto
    -- something you cannot hear is a way to record a mistake twice.
    "playing" | st.muted -> [ Command (cmd i "h1"), Command (cmd i "r") ]
    "playing" -> [ Command (cmd i "r") ]
    "idle" | st.layers > 0 && st.muted -> [ Command (cmd i "h1"), Command (cmd i "r") ]
    "idle" | st.layers > 0 -> [ Command (cmd i "r") ]
    -- Double-tapping an empty loop is a single tap said twice; the first one
    -- already started it and the second would close a loop a fifth of a second
    -- long. Refusing is kinder than obeying.
    _ -> [ Handled "already recording" ]

-- | The daemon's loop-prefixed command form: `3r` is "record on loop 3".
cmd :: Int -> String -> String
cmd i c = show i <> c

loopAt :: Rig -> Int -> Maybe LoopState
loopAt rig i = Array.index rig.loops i

show' :: BankSlot -> String
show' = case _ of
  LoopBank -> "loops"
  ConfigBank -> "config"
  QuantiseBank -> "quantise"
  SpeedBank -> "speed"
  ChanceBank -> "chance"
  PanBank -> "pan"

-- | One line about what an action did, for the display and the log.
-- |
-- | Every action produces one, including the refusals — a press that leaves no
-- | trace anywhere is the thing this whole surface exists to prevent.
describe :: Action -> String
describe = case _ of
  Command c -> "→ " <> c
  ShowBank slot -> "showing the " <> show' slot <> " bank"
  Focus i -> "loop " <> show (i + 1)
  Unavailable why -> why
  Handled what -> what
