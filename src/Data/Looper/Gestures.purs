-- | Turning footswitch presses into gestures, as a stream transducer.
-- |
-- | The MC6 sends one CC at 127 when a switch goes down and one at 0 when it
-- | comes up (`Data.Looper.Banks`). Tap, double-tap and hold are not three
-- | messages: they are one stream of downs and ups with timestamps, and telling
-- | them apart needs memory of what happened before. That is a
-- | **`Mealy`** — `purescript-ports/purescript-machines`, Kmett's `machines`
-- | ported — and using it here rather than hand-rolling an accumulator is the
-- | point as much as the result.
-- |
-- | ```purescript
-- | newtype Mealy i o = Mealy (i -> Tuple (Mealy i o) o)
-- | ```
-- |
-- | A machine that, given an input, yields an output and *the next machine*.
-- | Held in app state and stepped on each event, which is exactly the shape of
-- | the problem: the recogniser has state, the state is nobody else's business,
-- | and it never touches the engine.
-- |
-- | ## Why waiting is free, and only sometimes
-- |
-- | You cannot know a tap is not the first of a double-tap without waiting out
-- | the window. For a looper that is a tax on the most common action — except
-- | that when the action is quantised to the next boundary anyway, **the wait
-- | costs nothing so long as it resolves before the boundary arrives**. So the
-- | window is not a UX constant; it is a function of the grid, which is why
-- | this takes its thresholds as an argument rather than defining them.
-- |
-- | ## Two clocks that have to agree
-- |
-- | A hold is the one gesture the MC6 resolves by itself, jumping to the config
-- | bank at its own `longPressTime`. This recogniser must reach the same
-- | conclusion at the same moment, so it arms on the **down** rather than
-- | measuring at the up — and the threshold is read off the device
-- | (`Data.MC6.Settings`, `03 21` offset 3) rather than agreed by hand.
-- |
-- | Because it decides on a timer rather than on the release, it also does not
-- | matter whether the device suppresses the release message after a long
-- | press. That question never has to be answered.
-- |
-- | ## Ticks are inputs
-- |
-- | A gesture can complete when *nothing* happens — a tap becomes a tap by the
-- | double-tap window expiring. A transducer only moves when fed, so time is an
-- | input like any other: the app feeds `Tick` from the poll it already runs at
-- | 10 Hz, and the machine emits whatever that instant resolved.
module Data.Looper.Gestures
  ( Event(..)
  , Gesture(..)
  , Thresholds
  , Recogniser
  , recogniser
  , feed
  ) where

import Prelude

import Data.Array as Array
import Data.Looper.Banks (BankSlot, SwitchPress)
import Data.Machine.Mealy (Mealy, stepMealy, unfoldMealy)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | What the app feeds in: a switch moving, or time passing.
-- |
-- | Milliseconds are the app's own clock (`performance.now`-ish), not the
-- | engine's frames. The recogniser never converts between them; the grid
-- | arrives already in milliseconds via `Thresholds`.
data Event
  = Down SwitchPress Number
  | Up SwitchPress Number
  | Tick Number

derive instance Eq Event

-- | What comes out. One press produces exactly one of these, eventually.
-- |
-- | Each carries **when the foot went down**, which is not the same as when the
-- | gesture was recognised and is the number a looper actually needs. A tap
-- | cannot be known to be a tap until the double-tap window expires, so a
-- | recognition is always a few hundred milliseconds after the press — and an
-- | engine told the recognition time records a loop that much longer than it
-- | was played. Nothing in the sound says so, because overdubs are modular
-- | against whatever length the loop ended up with.
-- |
-- | So the moment travels with the gesture and the engine is told how late its
-- | command is (`Data.Looper.Machine`, and `@ms` in the daemon's dispatch).
-- | Which is what makes the double-tap window free: it still delays the
-- | *response*, and no longer changes the *recording*.
data Gesture
  = Tap BankSlot Int Number
  | DoubleTap BankSlot Int Number
  -- | Emitted at the threshold, not at the release — see the module header.
  -- | Its moment is the press, so a hold is timed from where it began.
  | Hold BankSlot Int Number

derive instance Eq Gesture

-- | How long to wait, in milliseconds.
-- |
-- | `holdMs` should be whatever the device says its long-press time is, so the
-- | app and the board change their minds together. `doubleTapMs` is the one
-- | that wants to follow the grid: long enough to catch a second tap, short
-- | enough to resolve before the boundary the action is waiting for anyway.
type Thresholds = { holdMs :: Number, doubleTapMs :: Number }

-- | The machine, with its state hidden — which is `Mealy`'s whole character.
type Recogniser = Mealy Event (Array Gesture)

-- | What the recogniser remembers, which is only ever about one switch.
-- |
-- | One switch at a time is a real simplification and a defensible one: these
-- | are feet. Two switches genuinely at once is a chord, not two gestures, and
-- | a chord is a different feature rather than an edge case of this one.
type Held =
  { slot :: BankSlot
  , switch :: Int
  , downAt :: Number
  -- | True once the hold has fired, so the release does not also produce a tap.
  , resolved :: Boolean
  }

type Waiting =
  { slot :: BankSlot
  , switch :: Int
  , upAt :: Number
  -- | When the press began, carried through the wait so the gesture can be
  -- | dated from the foot rather than from the moment we stopped waiting.
  , downAt :: Number
  }

type Memory = { held :: Maybe Held, waiting :: Maybe Waiting }

start :: Memory
start = { held: Nothing, waiting: Nothing }

-- | A fresh recogniser.
recogniser :: Thresholds -> Recogniser
recogniser th = unfoldMealy start (step th)

-- | Step it, keeping the next machine.
-- |
-- | A thin wrapper so callers do not have to import `machines` to use this —
-- | the library is an implementation choice here, not part of the interface.
feed :: Recogniser -> Event -> Tuple Recogniser (Array Gesture)
feed = stepMealy

-- | The transition. Pure, total, and the only place a gesture is decided.
step :: Thresholds -> Memory -> Event -> Tuple Memory (Array Gesture)
step th mem = case _ of

  Down p t ->
    -- A press while another waits its double-tap window: if it is the same
    -- switch, that is the second tap and the wait is over.
    case mem.waiting of
      Just w | sameAs w p.slot p.switch && t - w.upAt <= th.doubleTapMs ->
        -- Dated from the FIRST press of the pair: that is when the player
        -- committed to the gesture, and the second only confirmed which one.
        Tuple { held: Nothing, waiting: Nothing } [ DoubleTap p.slot p.switch w.downAt ]
      -- A different switch cancels the wait *as a tap*, because the first press
      -- was a tap — it just had not been allowed to say so yet. Dropping it
      -- would lose a press the player made.
      Just w ->
        Tuple { held: Just (hold p t), waiting: Nothing } [ Tap w.slot w.switch w.downAt ]
      Nothing ->
        Tuple (mem { held = Just (hold p t) }) []

  Up p t ->
    case mem.held of
      -- The hold already fired at the threshold; the release says nothing.
      Just h | h.resolved -> Tuple (mem { held = Nothing }) []
      Just h | sameAs h p.slot p.switch ->
        Tuple
          { held: Nothing
          , waiting: Just { slot: p.slot, switch: p.switch, upAt: t, downAt: h.downAt }
          } []
      -- An up for something we never saw go down. Reachable after a reload, or
      -- if a down was lost; forgetting it is right, inventing a tap is not.
      _ -> Tuple (mem { held = Nothing }) []

  Tick t ->
    let
      -- The hold fires here rather than on release, so it lands at the same
      -- instant the MC6's own long-press does.
      holdNow = case mem.held of
        Just h | not h.resolved && t - h.downAt >= th.holdMs ->
          Just (Tuple (h { resolved = true }) (Hold h.slot h.switch h.downAt))
        _ -> Nothing

      -- Nobody pressed again in time, so the earlier press was a tap.
      tapNow = case mem.waiting of
        Just w | t - w.upAt > th.doubleTapMs -> Just (Tap w.slot w.switch w.downAt)
        _ -> Nothing

      held' = case holdNow of
        Just (Tuple h _) -> Just h
        Nothing -> mem.held

      waiting' = case tapNow of
        Just _ -> Nothing
        Nothing -> mem.waiting
    in
      Tuple { held: held', waiting: waiting' }
        (Array.catMaybes [ map (\(Tuple _ g) -> g) holdNow, tapNow ])

  where
  hold p t = { slot: p.slot, switch: p.switch, downAt: t, resolved: false }

sameAs :: forall r. { slot :: BankSlot, switch :: Int | r } -> BankSlot -> Int -> Boolean
sameAs r slot switch = r.slot == slot && r.switch == switch
