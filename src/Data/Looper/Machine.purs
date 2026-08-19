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
import Data.Looper.Banks (BankSlot(..), loopSwitches, mc6OwnSwitches)
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

  -- **The toolbar, before any per-bank table.** G to L mean the same six things
  -- on every bank in the family, so they are answered in one place and the
  -- bank is not consulted — which is the code saying the same thing the layout
  -- says, rather than six tables that happen to agree.
  Tap _ i | i >= mc6OwnSwitches -> toolbar rig.focus i

  -- The config family. Everything here acts on `focus` — the loop last touched,
  -- which is what a hold on a loop switch sets. One config bank for six loops
  -- only works because the press that got here said which loop it meant.
  Tap ConfigBank i -> config rig.focus i
  Tap QuantiseBank i -> quantise rig.focus i
  Tap SpeedBank i -> speed rig.focus i
  Tap PanBank i -> pan rig.focus i
  Tap slot i -> [ Unavailable (show' slot <> " switch " <> show i <> " is not wired yet") ]

  DoubleTap slot i -> [ Handled ("double tap on " <> show' slot <> " " <> show i) ]
  Hold slot i -> [ Handled ("hold on " <> show' slot <> " " <> show i) ]


-- | The six unmarked switches, which do the same thing from anywhere.
-- |
-- | **Because a footswitch with no label is remembered as a position.** A
-- | switch that clears a loop on one page and sets an end-state on the next
-- | cannot be learned: you would have to know which bank you were on before you
-- | could know what your foot was about to do. That is precisely what a
-- | footswitch is for avoiding, and it is why this function does not take a
-- | bank — there is nothing it could usefully do with one.
-- |
-- | All six act on the focused loop or on everything, so all six are meaningful
-- | from any depth in the family. That is not a coincidence; it is the test for
-- | whether something belongs up here at all.
toolbar :: Int -> Int -> Array Action
toolbar f = case _ of
  -- The MC6 makes the jump itself, from the table it was programmed with.
  6 -> [ Handled "out" ]
  -- Stop all, as six commands rather than one. There is no all-loops form in
  -- the daemon's dispatch, and inventing one for a gesture that is not
  -- sample-critical would be protocol for its own sake — these land within a
  -- millisecond of each other, and stopping is not a downbeat.
  7 -> map (\i -> Command (cmd i "h0")) (Array.range 0 (loopSwitches - 1))
  8 -> [ Command (cmd f "u") ]
  9 -> [ Command (cmd f "c") ]
  10 -> [ Command "w" ]
  11 -> [ Command "k" ]
  i -> [ Unavailable ("switch " <> show i <> " is not on the toolbar") ]

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

-- | The config bank, against the focused loop.
-- |
-- | Three of the twelve are engine features that exist, three are navigation
-- | the MC6 does itself, and the rest name what they are waiting for. Speed and
-- | chance are the two that need real work — interpolation and a callback-safe
-- | RNG — and saying so is more use than a switch that shrugs.
config :: Int -> Int -> Array Action
config f = case _ of
  0 -> [ Handled "quantise: pick a grid" ]
  1 -> [ Handled "speed: pick a rate" ]
  2 -> [ Unavailable "chance needs a random source in the audio callback" ]
  3 -> [ Handled "pan: pick a placement" ]
  4 -> [ Command (cmd f "rev") ]
  -- Forward then back, so a cycle takes twice as long. It came free with speed,
  -- being a triangle where a plain loop is a sawtooth — the fold happens at the
  -- same place the wrap already did.
  5 -> [ Command (cmd f "pend") ]
  i -> [ Unavailable ("config switch " <> show i <> " is not wired yet") ]

-- | The quantise bank.
-- |
-- | **The engine's grid is the anchor loop's cycle, not a bar**, which is a
-- | decision made when quantised close landed: tempo gives a bar's length but
-- | not where the bar falls, so until the frame-to-wall-clock join exists no
-- | loop can be put on "bar 1". So `g` is a boolean and the bar counts on this
-- | bank have nothing to select yet. Free and Grid are real; the rest say what
-- | they are waiting for rather than quietly all meaning the same thing.
quantise :: Int -> Int -> Array Action
quantise f = case _ of
  0 -> [ Command (cmd f "g0") ]
  i | i >= 1 && i <= 4 ->
      [ Command (cmd f "g1")
      , Handled "on the grid — bar counts need the frame-to-bar join"
      ]
  5 -> [ Handled "back to loop config" ]
  i -> [ Unavailable ("quantise switch " <> show i <> " is not wired yet") ]

-- | The speed bank: five rates forward on the top row, the same five backwards
-- | on the bottom.
-- |
-- | **Direction is the sign, not a second control.** The engine keeps one
-- | `speed` and reads backwards off its sign, so `Rev 1/2` is one press that
-- | says both things rather than two that have to be pressed in the right
-- | order. That also means the top row is not "forward" so much as "positive":
-- | pressing `x 1` on a reversed loop turns it round, which is what the label
-- | says and what a player expects from a row of absolute settings.
-- |
-- | Recording is refused while a loop is at a speed — the input arrives at rate
-- | one and the grid is moving under it — so the daemon answers a press of
-- | record with the reason rather than doing something nobody asked for.
speed :: Int -> Int -> Array Action
speed f = case _ of
  0 -> [ rate f 0.25 ]
  1 -> [ rate f 0.5 ]
  2 -> [ rate f 1.0 ]
  3 -> [ rate f 1.5 ]
  4 -> [ rate f 2.0 ]
  5 -> [ Handled "back to loop config" ]
  i -> [ Unavailable ("speed switch " <> show i <> " is not wired yet") ]
  where
  rate i v = Command (cmd i "sp" <> show v)

-- | The pan bank: ten placements across the field, and two ways back.
-- |
-- | Equal-power in the engine, so moving a loop off centre does not make it
-- | quieter — which matters when six of them are being placed against each
-- | other rather than one being auditioned alone.
pan :: Int -> Int -> Array Action
pan f = case _ of
  0 -> [ place f 0 ]
  1 -> [ place f 32 ]
  2 -> [ place f 64 ]
  3 -> [ place f 96 ]
  4 -> [ place f 127 ]
  5 -> [ Handled "back to loop config" ]
  i -> [ Unavailable ("pan switch " <> show i <> " is not wired yet") ]
  where
  place i v = Command (cmd i "pan" <> show v)

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
