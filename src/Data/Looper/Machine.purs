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
import Data.Looper.Banks as LB
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
-- |
-- | **Keyed by what the switch is for, never by which switch it is.** This used
-- | to be a set of per-bank tables indexed by a switch number, running in
-- | parallel with the labels in `Data.Looper.Banks` and joined to them by
-- | nothing but that number. The layout said switch 9 was "Clear"; this said
-- | switch 9 sent `c`; moving Clear would have left a switch labelled one thing
-- | and doing another, and nothing would have failed to compile.
-- |
-- | Now a press is resolved to a `Duty` first and the meaning is a total
-- | function of that. The label on the pedal, the words on screen and the
-- | command on the wire are three renderings of one value, so they cannot
-- | disagree — and a new duty is a compile error here until it is given a
-- | meaning, rather than a switch that silently does nothing.
act :: Rig -> Gesture -> Array Action
act rig = case _ of

  Tap slot i -> case LB.dutyAt slot i of
    Just d -> onDuty rig slot d
    Nothing -> [ missing slot i ]

  DoubleTap slot i -> case LB.dutyAt slot i of
    Just (LB.SelectLoop n) -> Array.cons (Focus n) (onDouble n (loopAt rig n))
    Just d -> [ Handled ("double tap on " <> LB.dutyLabel d) ]
    Nothing -> [ missing slot i ]

  -- The MC6 jumps to the config bank on its own long press; all this has to do
  -- is agree about which loop that bank is now talking about.
  Hold slot i -> case LB.dutyAt slot i of
    Just (LB.SelectLoop n) -> [ Focus n, Handled ("configuring loop " <> show (n + 1)) ]
    Just d -> [ Handled ("hold on " <> LB.dutyLabel d) ]
    Nothing -> [ missing slot i ]

missing :: BankSlot -> Int -> Action
missing slot i = Unavailable (show' slot <> " switch " <> show i <> " has nothing on it")

-- | What a tap means, given what the switch is for.
-- |
-- | Total over `Duty`, which is the point: adding a switch to a bank cannot
-- | leave it doing nothing, because the compiler asks what it means.
onDuty :: Rig -> BankSlot -> LB.Duty -> Array Action
onDuty rig slot = case _ of
  LB.SelectLoop i -> Array.cons (Focus i) (onTap i (loopAt rig i))

  -- Navigation the MC6 performs itself, from the jumps it was programmed with.
  -- The app only has to agree that it happened.
  LB.Enter to -> [ Handled ("showing " <> show' to) ]
  LB.Back _ -> [ Handled "out" ]

  -- Stop all, as six commands rather than one. There is no all-loops form in
  -- the daemon's dispatch, and inventing one for a gesture that is not
  -- sample-critical would be protocol for its own sake — these land within a
  -- millisecond of each other, and stopping is not a downbeat.
  LB.StopAll -> map (\i -> Command (cmd i "h0")) (Array.range 0 (loopSwitches - 1))
  LB.Undo -> [ Command (cmd rig.focus "u") ]
  LB.ClearLoop -> [ Command (cmd rig.focus "c") ]
  LB.SaveTake -> [ Command "w" ]
  LB.ClickToggle -> [ Command "k" ]

  LB.Reverse -> [ Command (cmd rig.focus "rev") ]
  LB.Pendulum -> [ Command (cmd rig.focus "pend") ]

  LB.Free -> [ Command (cmd rig.focus "g0") ]
  -- **The engine's grid is the anchor loop's cycle, not a bar**, decided when
  -- quantised close landed: tempo gives a bar's length but not where the bar
  -- falls, so until the frame-to-wall-clock join exists no loop can be put on
  -- "bar 1". The flag is real; the count is a promise, and saying so is more
  -- use than four switches that quietly all mean the same thing.
  LB.Grid _ ->
    [ Command (cmd rig.focus "g1")
    , Handled "on the grid — bar counts need the frame-to-bar join"
    ]

  LB.Rate r -> [ Command (cmd rig.focus "sp" <> show r) ]
  LB.Place p -> [ Command (cmd rig.focus "pan" <> show p) ]

  LB.NotYet what why -> [ Unavailable (what <> ": " <> why) ]
  LB.Nothing_ -> [ Unavailable (show' slot <> " has nothing on that switch") ]

-- | Whether closing a loop should send the board to the config bank.
-- |
-- | The plan wants this, and calls it the only genuinely app-driven bank change
-- | — only the app knows a press was the *second* one. It is also the one thing
-- | here that has been actively unpleasant in use, and the reason is worth
-- | keeping rather than fixing quietly:
-- |
-- | **A courtesy that lands somewhere useless is not a courtesy.** With the
-- | config bank unwired, closing a loop moved the board to a page where every
-- | switch answers "not wired yet" — so the next thing the player does with
-- | their foot does nothing, after every single loop they record. The MC6's own
-- | "< Loops" switch is the way back, which is fine once you know and baffling
-- | until you do.
-- |
-- | Off until the config bank does something. Deliberately a flag with an
-- | explanation rather than deleted code: the behaviour is right, its
-- | precondition simply is not met yet.
jumpToConfigOnClose :: Boolean
jumpToConfigOnClose = false

-- | A tap on a loop switch, given what that loop is doing.
-- |
-- | Note that every branch is a *state the daemon reported*, not one we kept.
onTap :: Int -> Maybe LoopState -> Array Action
onTap i = case _ of
  Nothing -> [ Unavailable ("loop " <> show (i + 1) <> " is not in the snapshot") ]
  Just st
    -- Something is being recorded: close it. Asked first because these are
    -- claims about what is happening now, and they outrank what is stored.
    | st.state == "recordingFirst" ->
        Array.cons (Command (cmd i "r"))
          (if jumpToConfigOnClose then [ ShowBank ConfigBank ] else [])
    | st.state == "overdubbing" -> [ Command (cmd i "r") ]
    | st.state == "multiplying" -> [ Command (cmd i "r") ]

    -- **Empty is a fact about layers, not about state.**
    --
    -- Undo removes a layer and deliberately keeps the loop's length — so the
    -- next take lands on the same grid and the click stays at the tempo you
    -- found. Undo the last layer and the loop is `layers: 0` with a length and
    -- a state still reading `playing`. This used to test emptiness as
    -- `state == "idle" && layers == 0`, so it saw a playing loop, offered stop,
    -- and toggled silence on silence for ever: a loop undone to nothing could
    -- not be recorded into again from the board.
    --
    -- Quantised loops answer this with "starts on the grid in N s", which is
    -- why the display shows a countdown rather than nothing.
    | st.layers == 0 -> [ Command (cmd i "r") ]

    -- Stop and start. Explicit `h0`/`h1` rather than the flipping `h`, so a
    -- dropped command cannot leave the app and the engine disagreeing about
    -- something a stopped loop makes invisible by definition.
    | st.muted -> [ Command (cmd i "h1") ]
    | otherwise -> [ Command (cmd i "h0") ]

-- | A double tap: overdub, per the plan.
onDouble :: Int -> Maybe LoopState -> Array Action
onDouble i = case _ of
  Nothing -> [ Unavailable ("loop " <> show (i + 1) <> " is not in the snapshot") ]
  Just st
    -- Double-tapping an empty loop is a single tap said twice: the first
    -- already started it, and the second would close a loop a fifth of a
    -- second long. Refusing is kinder than obeying.
    | st.layers == 0 -> [ Handled "already recording" ]
    -- Overdub. A stopped loop is brought back first, because overdubbing onto
    -- something you cannot hear is a way to record a mistake twice.
    | st.muted -> [ Command (cmd i "h1"), Command (cmd i "r") ]
    | otherwise -> [ Command (cmd i "r") ]

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
