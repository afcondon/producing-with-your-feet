-- | What a gesture means, given what the loops are doing.
-- |
-- | The whole input path, now that there is nothing in front of it. There used
-- | to be a `Data.Looper.Gestures` — a `Mealy` transducer timing switch edges
-- | into taps, doubles and holds — and it is gone: the MC6 does that itself, and
-- | does it without the three failure modes an app-side recogniser has (see
-- | `Data.Looper.Banks`). A press arrives already knowing which gesture it was.
-- |
-- | ## It holds no state at all
-- |
-- | A looper's phase — empty, recording, playing — is not something this app
-- | should model. The daemon reports it thirty times a second and the app has
-- | always taken that as authoritative (`Foreign.LooperSocket`: "the app holds
-- | only what the daemon last reported; it never models the engine itself").
-- |
-- | The recogniser held the only state anything here ever had — whether a second
-- | tap was still possible, and how long a switch had been down — and the device
-- | now holds it instead. What is left is a pure function from (gesture, engine
-- | truth) to actions, which can be tested by enumeration and cannot drift out
-- | of step with the engine, because it has nothing of its own to drift with.
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
  , perform
  , describe
  ) where

import Prelude

import Data.Array as Array
import Data.Looper.Banks (BankSlot(..), Subject(..), SwitchGesture, nLoops)
import Data.Looper.Banks as LB
import Data.Looper.Verb as Verb
import Data.Maybe (Maybe(..), maybe)
import Foreign.LooperSocket (LoopState)
import Foreign.LooperSocket as Looper

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
-- |
-- | All of it read from the daemon's snapshot, none of it kept here. The two
-- | global flags were added 2026-08-25 and they close a wart that
-- | `Data.Looper.Verb` used to describe at length on a verb since deleted: it had
-- | no way to compute `Click (not current)`, so the click was the one thing
-- | still sent as a flip rather than a set. It carries them now, so it sets.
type Rig =
  { loops :: Array LoopState
  -- | The loop the config bank acts on: the last one touched.
  , focus :: Int
  -- | The metronome and input monitoring. Global in the engine rather than per
  -- | loop, and addressed without a leading digit.
  , click :: Boolean
  , monitor :: Boolean
  -- | The level a sound has to reach before a level-armed loop starts, in
  -- | decibels. Rig-wide for the same reason as those two, and reported by the
  -- | daemon since 2026-08-25 because a control that sets it has to be able to
  -- | show it.
  , armDb :: Number
  -- | What a launch waits for, in beats: `-1` a bar, `0` none. Rig-wide, so it
  -- | sits here beside the click and the arm threshold rather than on a loop.
  , launchQ :: Int
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
-- | **One decoder of three.** `perform` is the meaning; this is the MC6's way
-- | in, and the page and the Twister have their own. See `DESIGN-TWISTER` §4.
act :: Rig -> SwitchGesture -> Array Action
act rig p = case LB.dutiesAt p.slot p.switch of
  Nothing -> [ missing p.slot p.switch ]
  Just s -> case p.gesture, LB.dutyFor p.gesture s of

    -- **All three go the same way now**, which they did not when a loop switch
    -- carried a different verb on each. A double no longer means overdub and a
    -- hold no longer opens the config bank, because both have a switch of their
    -- own on `LoopPage` with their name printed on it — so the gesture chooses
    -- which duty the switch is showing, and the duty decides everything else.
    -- The MC6 always speaks about the focused loop: six switches cannot name
    -- eight loops as well as saying what to do to one.
    _, Just d -> perform rig Focused d

    -- **The board and this table have fallen out of step.** A gesture only
    -- arrives because the device was programmed to send it, and the device is
    -- programmed from this table — so an unbound one means the board is running
    -- an older upload. Said out loud rather than swallowed: that is exactly the
    -- disagreement a press should be able to report.
    g, Nothing ->
      [ Unavailable
          ( "the board sent a " <> LB.gestureName g <> " on "
              <> LB.dutyLabel s.tap
              <> ", which this bank does not carry — reprogram the MC6"
          )
      ]

missing :: BankSlot -> Int -> Action
missing slot i = Unavailable (show' slot <> " switch " <> show i <> " has nothing on it")

-- | **What a duty means. The only way to the socket.**
-- |
-- | Total over `Duty`, which is the point: adding a duty cannot leave a control
-- | doing nothing, because the compiler asks what it means. It used to be
-- | `onDuty`, reachable only through an MC6 bank and a switch number, which is
-- | one layer too deep — the page and the Twister have no bank and no switch,
-- | and while this was buried they used a second meaning table instead.
-- |
-- | The `Subject` says which loop, and says it out loud. Every per-loop command
-- | below goes through `cmd i`, which is `Verb.at` — there is deliberately no
-- | path here that sends an unprefixed per-loop verb, because the daemon then
-- | applies it to a selection nothing in this app writes.
perform :: Rig -> Subject -> LB.Duty -> Array Action
perform rig subject = case _ of
  -- **Selecting a loop does nothing to it.** The MC6 opens its page from the
  -- jump this table put on the switch; all the app has to do is agree whose
  -- page it now is.
  --
  -- Doing something as well was considered and rejected. Recording an empty
  -- loop on the way in would save a press, but the same switch would then stop
  -- a playing one — so you could not look at a loop without acting on it, which
  -- is exactly the thing this page exists to end.
  LB.SelectLoop n -> [ Focus n, Handled ("loop " <> show (n + 1)) ]

  LB.RecordLoop -> onRecord i (loopAt rig i)
  LB.OverdubLoop -> onOverdub i (loopAt rig i)
  LB.Transport -> onTransport i (loopAt rig i)

  -- The mode and the gesture in one press. `lev1` before `r`, so the record
  -- that follows finds the loop already listening and goes to ARMED rather than
  -- straight to a first take.
  LB.ArmLoop -> case loopAt rig i of
    Nothing -> [ notInSnapshot i ]
    Just st
      | st.armed -> [ Handled ("loop " <> show (i + 1) <> " is already listening") ]
      | Looper.phaseOf st /= Looper.Idle ->
          [ Unavailable ("loop " <> show (i + 1) <> " is busy — close it first") ]
      | otherwise ->
          [ Command (cmd i (Verb.LevelArm true)), Command (cmd i Verb.Record) ]

  -- Navigation the MC6 performs itself, from the jumps it was programmed with.
  -- The app only has to agree that it happened.
  LB.Enter to -> [ Handled ("showing " <> show' to) ]
  LB.Back _ -> [ Handled "out" ]

  -- Stop all, as six commands rather than one. There is no all-loops form in
  -- the daemon's dispatch, and inventing one for a gesture that is not
  -- sample-critical would be protocol for its own sake — these land within a
  -- millisecond of each other, and stopping is not a downbeat.
  -- **Only the loops that have something to stop.**
  --
  -- Muting an empty loop does nothing you can hear and leaves it silenced for
  -- whatever is recorded into it next — so Stop All used to reach across the
  -- whole set and disarm every slot the player had not touched yet. Skipping
  -- the empties keeps the gesture meaning what it says.
  LB.StopAll -> map (\n -> Command (cmd n (Verb.Sounding false))) (sounding rig)
  LB.Undo -> [ Command (cmd i Verb.Undo) ]
  LB.ClearLoop -> [ Command (cmd i Verb.Clear) ]
  -- **Addressed to the focused loop, like everything else on this page.**
  --
  -- It used to go unprefixed. Without a leading digit the daemon applies a
  -- command to *its* selection — `sh.sel()` — and that field is written by
  -- nothing on the six-loop surface: the app tracks focus itself, in `Rig`, and
  -- has never told the daemon about it. So the daemon's selection has read zero
  -- since this page was built, and Save Take wrote loop 1's layers to disk
  -- whatever the board was focused on. It reported success, because saving
  -- loop 1 *is* a success.
  --
  -- The same family as the display bug that cost two sessions — a loop index
  -- read from somewhere nothing updates — and the same fix: say which loop,
  -- every time. The daemon puts it better than this comment can: "selection
  -- that only some callers depend on is a mode, and a mode that a footswitch
  -- could fall out of step with is the thing this design is trying not to
  -- have."
  LB.SaveTake -> [ Command (cmd i (Verb.SaveTake "")) ]
  -- **Set, not flip — and `Rig` is why it can be.**
  --
  -- The metronome is global, `sh.click` rather than `lp.click`, so the daemon
  -- never consults its selection for it and a loop index would be noise. It is
  -- still unprefixed for that reason. What changed is the form: this sent the
  -- flipping `k` for as long as `Rig` carried only the loops and the focus, and
  -- `Verb.ClickToggle` carried a paragraph explaining why the one rule the
  -- vocabulary has was broken here. `Rig` carries the flags now, so it sets,
  -- and the verb it apologised for has been deleted.
  LB.ClickToggle -> perform rig subject (LB.Click (not rig.click))
  LB.MonitorToggle -> perform rig subject (LB.Monitor (not rig.monitor))

  -- Unprefixed on purpose: both are `sh.` fields in the engine, not `lp.` ones,
  -- so the daemon never consults its selection for them and a loop index would
  -- be noise. The only two per-*rig* commands the app sends.
  LB.Click on -> [ Command (Verb.render (Verb.Click on)) ]
  LB.Monitor on -> [ Command (Verb.render (Verb.Monitor on)) ]

  -- **Set, never flip.** All four of these read the current value out of the
  -- snapshot and send the explicit form, for the reason the daemon spells out
  -- on `k` and `h`: a client that flips drifts out of step the first time a
  -- command is dropped and never recovers. The app has the engine's own answer
  -- thirty times a second, so there is no excuse for asking it to guess.
  LB.Reverse -> [ Command (cmd i (Verb.Reversed (not (is _.reverse)))) ]
  LB.Pendulum -> [ Command (cmd i (Verb.Pendulum (not (is _.pendulum)))) ]
  LB.OneShot -> [ Command (cmd i (Verb.OneShot (not (is _.oneShot)))) ]
  LB.LevelArm -> [ Command (cmd i (Verb.LevelArm (not (is _.levelArm)))) ]

  -- Chance is a value, not a toggle, so the switch steps rather than flips —
  -- but it is the same principle one step further: the next rung is computed
  -- from what the engine last reported, not counted here and not counted on the
  -- device. A scroll counter on the MC6 would keep its own position, and the
  -- device is the one thing in this rig that cannot be told it is wrong.
  -- **Delegated, not duplicated.** The step reads the engine's value, finds the
  -- next rung and hands off to the value duty — so a footswitch and a Twister
  -- knob reach the socket by the same line of code and cannot come to disagree
  -- about what chance means. The ladder is a rendering of the parameter for a
  -- surface that can only press; the value is the parameter.
  LB.StepChance ->
    let next = LB.stepChance (maybe 1.0 _.chance (loopAt rig i))
    in perform rig subject (LB.Chance next)
       <> [ Handled ("loop " <> show (i + 1) <> " plays " <> LB.chanceWord next) ]

  -- The same ladder machinery, and the same reason for it: the value is on the
  -- engine, so the step is computed from what the engine said rather than
  -- counted anywhere that could fall out of step with it.
  LB.StepFade ->
    let next = LB.stepFade (maybe 0.0 _.fadeMs (loopAt rig i))
    in perform rig subject (LB.Fade next)
       <> [ Handled ("loop " <> show (i + 1) <> " wraps " <> LB.fadeWord next) ]

  LB.StepDecay ->
    let next = LB.stepDecay (maybe 0.0 _.decayDb (loopAt rig i))
    in perform rig subject (LB.Decay next)
       <> [ Handled ("loop " <> show (i + 1) <> " decays " <> LB.decayWord next) ]

  -- The one thing a pedal cannot do. With no loop yet it claims the daemon's
  -- default of the last few seconds; with one running it claims the last
  -- complete cycle, which lands on the grid because the fill is addressed in
  -- output frames.
  LB.ClaimPast -> [ Command (cmd i Verb.ClaimPast) ]
  LB.Redo -> [ Command (cmd i Verb.Redo) ]
  LB.StartAll -> map (\n -> Command (cmd n (Verb.Sounding true))) (sounding rig)
  LB.ClearAll -> map (\n -> Command (cmd n Verb.Clear)) (Array.range 0 (nLoops - 1))

  LB.Free -> perform rig subject (LB.OnGrid false)
  LB.OnGrid on -> [ Command (cmd i (Verb.OnGrid on)) ]
  LB.GridToggle -> perform rig subject (LB.OnGrid (not (is _.quant)))
  -- **The engine's grid is the anchor loop's cycle, not a bar**, decided when
  -- quantised close landed: tempo gives a bar's length but not where the bar
  -- falls, so until the frame-to-wall-clock join exists no loop can be put on
  -- "bar 1". The flag is real; the count is a promise, and saying so is more
  -- use than four switches that quietly all mean the same thing.
  LB.Grid _ ->
    perform rig subject (LB.OnGrid true)
      <> [ Handled "on the grid — bar counts need the frame-to-bar join" ]

  LB.Rate r -> [ Command (cmd i (Verb.Rate r)) ]
  LB.Place p -> [ Command (cmd i (Verb.Place p)) ]
  LB.Level db -> [ Command (cmd i (Verb.Level db)) ]

  -- Rig-wide, so unprefixed — the third of the three, with the click and the
  -- input monitor.
  LB.ArmLevel db -> [ Command (Verb.render (Verb.ArmLevel db)) ]

  -- The same set-never-flip pair as the click: the toggle reads the snapshot
  -- and delegates to the value.
  LB.RevoxToggle -> perform rig subject (LB.Revox (not (is _.revox)))
  LB.Revox on -> [ Command (cmd i (Verb.Revox on)) ]
  LB.Feedback db -> [ Command (cmd i (Verb.Feedback db)) ]
  LB.Tone hz -> [ Command (cmd i (Verb.Tone hz)) ]
  -- Zero is not a tape. The knob's bottom end means "no tape here", which is
  -- not a command — it is the absence of one, and sending `blank0` would have
  -- the daemon refuse a length nobody asked for.
  LB.Blank secs
    | secs <= 0.0 -> []
    | otherwise -> [ Command (cmd i (Verb.Blank secs)) ]

  -- **The undo stack as a position.** The difference between where the knob is
  -- and where the engine says it is, spent as that many steps in the right
  -- direction. Nothing is remembered here: `have` comes from the snapshot, so a
  -- layer removed by a footswitch moves the knob rather than confusing it.
  -- **Nothing to scrub on a tape.** A Revox pass wrote over what was there, so
  -- there is no layer to take back and no version of it kept anywhere. Refused
  -- by name rather than silently doing nothing, which is the difference between
  -- a mode and a broken knob.
  LB.Layers _ | is _.revox ->
    [ Unavailable ("loop " <> show (i + 1) <> " is a tape — undo went with the layers") ]
  LB.Layers want ->
    let have = maybe 0 _.layers (loopAt rig i)
        step = want - have
    in if step == 0 then []
       else if step < 0
         then Array.replicate (negate step) (Command (cmd i Verb.Undo))
           <> [ Handled ("loop " <> show (i + 1) <> ": " <> show want <> " layers") ]
         else Array.replicate step (Command (cmd i Verb.Redo))
           <> [ Handled ("loop " <> show (i + 1) <> ": " <> show want <> " layers") ]
  LB.Chance p -> [ Command (cmd i (Verb.Chance p)) ]
  LB.Fade ms -> [ Command (cmd i (Verb.Fade ms)) ]
  LB.Decay db -> [ Command (cmd i (Verb.Decay db)) ]

  -- The four the CC table had and this one did not, until the page was moved
  -- onto the machine. `x` is the one that mattered: the reference surface could
  -- ask for a multiply and the machine had no word for it.
  LB.MultiplyLoop -> [ Command (cmd i Verb.Multiply) ]
  LB.SpreadLoop n -> [ Command (cmd i (Verb.Spread n)) ]

  -- **The three that make a loop a number of bars.** All per-loop and all going
  -- through `cmd i` like everything else here; the daemon decides which of the
  -- three things `len` means from the loop's own state, because it is the only
  -- one that knows whether the loop is empty, is the anchor, or has a clock.
  LB.SetBars n -> [ Command (cmd i (Verb.Bars n)) ]
  LB.Every n -> [ Command (cmd i (Verb.Spread n)) ]
  LB.PlaceAt n -> [ Command (cmd i (Verb.Place' n)) ]

  -- Rig-wide, so it goes bare rather than through `cmd i` — the same shape as
  -- the click and the arm threshold, and the daemon ignores a loop prefix on it
  -- anyway. Sent through `Command` unprefixed so nothing here has to pick a
  -- loop for a setting that has none.
  LB.Launch n -> [ Command (Verb.render (Verb.LaunchQ n)) ]
  LB.RotateLoop -> [ Command (cmd i Verb.Rotate) ]
  LB.DenseLoop -> [ Command (cmd i Verb.Dense) ]
  LB.ForgetLength -> [ Command (cmd i Verb.ForgetLength) ]

  LB.NotYet what why -> [ Unavailable (what <> ": " <> why) ]
  -- No longer names the bank it came from: a duty has no bank, and the two
  -- callers that do have one — `act` above, for a switch with no duties at all —
  -- say so themselves.
  LB.Nothing_ -> [ Unavailable "nothing is on that control" ]
  where
  -- **Where the subject becomes an index, and the only place it does.**
  i = case subject of
    Focused -> rig.focus
    OnLoop n -> n
  is field = maybe false field (loopAt rig i)


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

-- | Record: open a take, close one, or take back a wait.
-- |
-- | **One command, because the engine has one command.** `r` toggles the write
-- | head — it opens a first recording, closes it, opens and closes an overdub,
-- | and cancels a loop that is listening. This used to be four of the seven
-- | branches of a tap on a loop switch, and separating them here would be
-- | separating something the daemon does not separate.
-- |
-- | Note every branch is a *state the daemon reported*, not one we kept.
onRecord :: Int -> Maybe LoopState -> Array Action
onRecord i = case _ of
  Nothing -> [ notInSnapshot i ]
  Just st
    -- Writing now: close it. Asked first because these are claims about what is
    -- happening at this instant, and they outrank what is stored.
    | Looper.isWriting st ->
        Array.cons (Command (cmd i Verb.Record))
          (if jumpToConfigOnClose then [ ShowBank ConfigBank ] else [])

    -- Waiting for a sound that may never come. A press has to be able to take
    -- that back, or a listening loop holds the one converter the rig has and
    -- locks out the other five.
    | st.armed -> [ Command (cmd i Verb.Record), Handled ("loop " <> show (i + 1) <> " stopped listening") ]

    -- **A muted loop is brought back before it is written to.**
    --
    -- Recording into something you cannot hear is the failure `onOverdub` has
    -- always guarded against, and it reaches Record by a route nobody looked
    -- at: Stop All mutes every loop *including the empty ones*, so a stop
    -- anywhere in the set arms a trap in all six. The next take then records
    -- perfectly and silently, and the only evidence is a waveform in a slot
    -- drawn the colour of "stopped".
    --
    -- The daemon anticipated exactly this for `c` — "an empty loop that is
    -- still silenced would refuse to record audibly for a reason nothing on
    -- screen could explain" — and the same sentence is true of `r`. Pressing
    -- Record means *I am working on this loop*, and working on something
    -- inaudible is never what was meant.
    | st.muted -> [ Command (cmd i (Verb.Sounding true)), Command (cmd i Verb.Record) ]

    -- **Empty is a fact about layers, not about state.**
    --
    -- Undo removes a layer and deliberately keeps the loop's length — so the
    -- next take lands on the same grid and the click stays at the tempo you
    -- found. Undo the last layer and the loop is `layers: 0` with a length and
    -- a state still reading `playing`. Testing emptiness as
    -- `state == "idle" && layers == 0` saw a playing loop and offered stop, so a
    -- loop undone to nothing could not be recorded into again from the board.
    | otherwise -> [ Command (cmd i Verb.Record) ]

-- | Overdub: another pass over what is there, audible first.
onOverdub :: Int -> Maybe LoopState -> Array Action
onOverdub i = case _ of
  Nothing -> [ notInSnapshot i ]
  Just st
    | Looper.isWriting st -> [ Command (cmd i Verb.Record), Handled ("closed loop " <> show (i + 1)) ]
    -- Nothing to go over yet. Starting a first take here would be this switch
    -- quietly becoming Record, which is the switch next to it.
    | st.layers == 0 ->
        [ Unavailable ("loop " <> show (i + 1) <> " is empty — record it first") ]
    -- A stopped loop is brought back first, because overdubbing onto something
    -- you cannot hear is a way to record a mistake twice.
    | st.muted -> [ Command (cmd i (Verb.Sounding true)), Command (cmd i Verb.Record) ]
    | otherwise -> [ Command (cmd i Verb.Record) ]

-- | Stop it, start it, or fire it.
-- |
-- | Explicit `h0`/`h1` rather than the flipping `h`, for the reason the daemon
-- | spells out: a client that flips drifts out of step the first time a command
-- | is dropped and never recovers — and a stopped loop makes the disagreement
-- | invisible by definition.
onTransport :: Int -> Maybe LoopState -> Array Action
onTransport i = case _ of
  Nothing -> [ notInSnapshot i ]
  Just st
    | st.layers == 0 ->
        [ Unavailable ("loop " <> show (i + 1) <> " has nothing to play") ]

    -- **A one-shot has no stopped and playing to move between.** It is silent
    -- between passes by definition, so firing is the only thing this switch
    -- could mean — and that is why the mode rides in the snapshot rather than
    -- being remembered here: what the switch does turns on a fact only the
    -- engine holds, and the app has to know it before the foot lands.
    | st.oneShot -> [ Command (cmd i Verb.Fire) ]

    | st.muted -> [ Command (cmd i (Verb.Sounding true)) ]
    | otherwise -> [ Command (cmd i (Verb.Sounding false)) ]

notInSnapshot :: Int -> Action
notInSnapshot i = Unavailable ("loop " <> show (i + 1) <> " is not in the snapshot")

-- | Which loops have anything in them, for the gestures that act on all of
-- | them at once.
-- |
-- | Read from the snapshot rather than assumed, like everything else here: a
-- | loop is empty when the *engine* says it has no layers, not when this app
-- | last thought so.
sounding :: Rig -> Array Int
sounding rig =
  Array.filter (\i -> maybe false (\st -> st.layers > 0) (loopAt rig i))
    (Array.range 0 (nLoops - 1))

-- | The daemon's loop-prefixed command form: `3r` is "record on loop 3".
-- |
-- | A thin alias for `Verb.at`, kept because every call site here reads better
-- | as `cmd i Verb.Record` than as a qualified name twice over.
cmd :: Int -> Verb.Verb -> String
cmd = Verb.at

loopAt :: Rig -> Int -> Maybe LoopState
loopAt rig i = Array.index rig.loops i

show' :: BankSlot -> String
show' = case _ of
  LoopBank -> "loops"
  LoopPage -> "the loop"
  ConfigBank -> "config"
  QuantiseBank -> "quantise"
  SpeedBank -> "speed"
  ModesBank -> "modes"
  PanBank -> "pan"

-- | One line about what an action did, for the display and the log.
-- |
-- | Every action produces one, including the refusals — a press that leaves no
-- | trace anywhere is the thing this whole surface exists to prevent.
describe :: Action -> String
describe = case _ of
  -- **Named, not echoed.** This printed the wire, and the wire counts loops
  -- from zero while every surface a human reads counts from one — so asking
  -- for four bars on Loop 2 logged `→ 1len4`, which is correct and reads as
  -- loop one. A log that says the right thing in the wrong dialect is
  -- indistinguishable from a bug and costs the same to chase.
  --
  -- The wire is still shown, because it is what to paste into the console when
  -- something needs settling — it just no longer leads with a number that
  -- means something else.
  Command c -> case Verb.addressed c of
    Just r -> "loop " <> show (r.loop + 1) <> " · " <> r.verb
    Nothing -> "→ " <> c
  ShowBank slot -> "showing the " <> show' slot <> " bank"
  Focus i -> "loop " <> show (i + 1)
  Unavailable why -> why
  Handled what -> what
