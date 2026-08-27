-- | The looper on the Midifighter Twister — the third surface, and the second
-- | decoder.
-- |
-- | ## What this is not
-- |
-- | It is **not** a mapping from encoders to CCs. Every other pedal in this app
-- | reaches the Twister that way, through `Data.Twister.TwisterMapping` and
-- | `Pedals.<Name>.twister`, and Itajara is the one pedal whose `twister` field
-- | is still `Nothing` — deliberately. That route ends at `Data.Looper.command`,
-- | which is an *addressing* table: it says which duty a CC names. The Twister
-- | has no CCs to address and no reason to invent any, so it names duties
-- | directly.
-- |
-- | The rule that produces is worth stating, because filling in that field
-- | would look like a five-minute job and would quietly install a fourth path
-- | to the daemon: **every surface decodes to a `Duty` and nothing decodes to a
-- | command.** `Data.Looper.Machine.perform` is the only way to the socket.
-- |
-- | ## What the shape of the device buys
-- |
-- | Three things the MC6 cannot do, and the layout below is mostly a
-- | consequence of them:
-- |
-- | * **Sixty-four controls**, so every duty has one of its own — which means
-- |   no gestures, which means **nothing waits out a recognition window**. The
-- |   MC6 sends a tap a few hundred milliseconds after the foot moved and the
-- |   app has to hand the lateness to the daemon to undo (`deferralOf`, `@ms`).
-- |   A press here is a press: `late` is zero, honestly.
-- | * **A press and a turn on the same physical control.** A loop's encoder is
-- |   both its selector and its knob. Nothing on a pedalboard is shaped like
-- |   that.
-- * **Colour, instantly.** The MC6's labels cost an editor session — the better
-- |   part of a second — so its layout has to be static and the *screen* names
-- |   the loop. One CC changes an encoder's hue, so this layout can say what is
-- |   happening while it happens.
-- |
-- | ## It holds no value of its own
-- |
-- | Every ring position below is computed from the daemon's snapshot, never
-- | from what was last sent and never from a position the device kept. That is
-- | the property that separates an encoder from the MC6's scroll counters,
-- | which `Data.Looper.Banks` rejected for exactly this reason: *"a device that
-- | keeps state is the one thing here that cannot be told it is wrong."* An
-- | encoder can be told. It is told thirty times a second.
module Data.Looper.Twister
  ( Param(..)
  , paramLabel
  , paramRange
  , Tone(..)
  , toneName
  , hue
  , Light(..)
  , Flag(..)
  , flagName
  , RingSource(..)
  , Control
  , controlAt
  , pressedAt
  , turnedAt
  , fromKnob
  , toKnob
  , rateLadder
  , rateSteps
  , Led
  , leds
  , pager
  , pagerRing
  , pagerIndex
  , pageFor
  , pageTone
  , pageStep
  , pages'
  , launchLadder
  , maxBars
  , Cell
  , Page
  , pages
  , phaseKey
  ) where

import Prelude

import Data.Array as Array
import Data.Int (round, toNumber)
import Data.Looper.Banks (Duty(..), Subject(..), dutyLabel, dutyName, nLoops)
import Data.Looper.Machine (Rig)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Data.Twister (Knob, encodersPerBank)
import Foreign.LooperSocket (LayerShape, LoopPhase(..), LoopState, allPhases, phaseName, phaseOf)

-- | A parameter an encoder can hold.
-- |
-- | Exactly the parameters the daemon takes a number for, and no others — a
-- | knob that moved a value nothing reads would be the worst kind of control,
-- | one that looks like it worked. (This said "five, and there is deliberately
-- | no `PLevel`" until the engine grew `vol`; the list has grown three times
-- | since and the rule is what survived.)
data Param
  = PTape
  | PLayers
  | PLevel
  | PRate
  | PPlace
  | PFade
  | PDecay
  | PChance
  -- | The tape's two, which had no control on either hardware surface until the
  -- | four-page layout — only sliders on the web page, while the mode they
  -- | configure sat on an encoder. Revox is the one mode with no undo, which
  -- | makes it the worst one to have to look away to adjust.
  | PFeedback
  | PTone
  -- | **Length, in bars, and how the material lands in it.** Three numbers that
  -- | used to be two gestures: `SpreadLoop` set how often *and* grew the loop,
  -- | so they could not be set apart. See `Data.Looper.Banks.SetBars` for the
  -- | three things `PBars` means depending on what the loop already is.
  | PBars
  | PEvery
  | POn
  -- | What a launch waits for, in beats. **The only parameter here that is not
  -- | about a loop**, which is why its ring is a `RigValue` — see `RingSource`.
  | PLaunch

derive instance Eq Param

-- | The word for a parameter, and the span of the knob that carries it.
-- |
-- | **The range is computed from the constants below, never typed twice.** A
-- | cheat sheet that claimed a knob went to 250 ms while `fadeTop` said 200
-- | would be worse than no cheat sheet: it would be believed.
paramLabel :: Param -> String
paramLabel = case _ of
  PTape -> "tape"
  PLayers -> "layers"
  PLevel -> "level"
  PRate -> "speed"
  PPlace -> "pan"
  PFade -> "fade"
  PDecay -> "decay"
  PChance -> "chance"
  PFeedback -> "leaves"
  PTone -> "keeps"
  PBars -> "bars"
  PEvery -> "every"
  POn -> "on"
  PLaunch -> "launch"

-- | The position a knob should be able to find without looking, if it has one.
-- |
-- | **A detent in software, because the device's is in its own configuration.**
-- | The Midifighter can draw a bipolar ring from the centre outwards and can be
-- | given a physical detent, and both are settings in the Midifighter Utility
-- | rather than anything reachable over MIDI — and this app deliberately keeps
-- | no device configuration (`DESIGN-TWISTER` §11 rule 6). What it can do is
-- | make centre *stick*: anything within a step or two of the middle lands
-- | exactly in the middle, so a pan you meant to be centred is centred rather
-- | than one off it.
-- |
-- | Pan is the only one. Speed had one too, until it was quantised: a knob
-- | whose steps are wide enough to feel does not need a detent bolted on, and
-- | its middle step *is* stopped. See `rateOf`.
homePosition :: Param -> Maybe Int
homePosition = case _ of
  PPlace -> Just 64
  _ -> Nothing

-- | How far from home still counts as home. Two steps out of 128 — enough to
-- | catch a knob that stopped just past the middle, small enough that it cannot
-- | swallow a deliberate nudge off it.
detentWidth :: Int
detentWidth = 2

-- | `Data.Ord.abs` needs a `Ring` and `Int`'s is not in scope here for one use.
intAbs :: Int -> Int
intAbs n = if n < 0 then negate n else n

detented :: Param -> Int -> Int
detented p v = case homePosition p of
  Just home | intAbs (v - home) <= detentWidth -> home
  _ -> v

paramRange :: Param -> String
paramRange = case _ of
  PTape -> "none to " <> show (round tapeTop) <> " s, threaded empty"
  PLayers -> "none to " <> show maxLayers <> ", one step a layer"
  PLevel -> "silent to full, -12 dB at half travel"
  PRate -> "×4 back, through stopped, to ×4 forward — in fifths and octaves"
  PPlace -> "hard left to hard right, and centre sticks"
  PFade -> "0 to " <> show (round fadeTop) <> " ms"
  PDecay -> "hold at the top, down to −" <> show (round decayLaw.floorDb) <> " dB a pass"
  PChance -> "never to always"
  PFeedback -> "a pass leaves nothing to everything, −"
                 <> show (round (negate feedbackFloor)) <> " dB to 0"
  PTone -> show (round (toneFloor / 1000.0)) <> " kHz to all of it"
  PBars -> "1 to " <> show maxBars <> " bars — sizes an empty loop, resizes a full one"
  PEvery -> "every time round, to once in " <> show maxBars
  POn -> "which of those it lands on; wraps"
  PLaunch -> "none, a beat up to eight bars, or the bar"

-- | What lights the encoder's ring.
-- | **Two constructors, and there used to be three.** `Playhead` drew a loop's
-- | position round the ring at snapshot rate and had to go: on this device the
-- | ring *is* the encoder's value, so anything written to it is written into
-- | the number the next touch will send. A ring on an encoder that also carries
-- | a value belongs to that value and to nothing else.
data RingSource
  = NoRing
  -- | The parameter's own value, read from the snapshot. Which also keeps the
  -- | device's own idea of where the encoder is standing equal to the engine's,
  -- | every poll — the thing that makes a nudge harmless rather than arbitrary.
  | Value Param
  -- | Which page is showing. The one ring whose value is a fact about the app
  -- | rather than about the engine, which is why `leds` fills it in — that is
  -- | the function that knows the page.
  | PageRing
  -- | A value that belongs to the **rig** rather than to any loop.
  -- |
  -- | `Value` reads the snapshot's loop; there was nothing here that could read
  -- | the rig, because until the launch quantise every ring on this surface was
  -- | about one loop. Resolved in `leds`, which is where the `Rig` is in scope —
  -- | `ringOf` takes a `LoopState` and deliberately still does, so the eight
  -- | loop encoders cannot accidentally start reading something global.
  | RigValue Param

derive instance Eq RingSource

-- | What colours it.
data Light
  = Dark
  -- | A fixed colour: this control is always this one, because what it does
  -- | never changes.
  | Steady Tone
  -- | The subject loop's phase. Recording is not playing is not empty, and on a
  -- | controller with no text that is the whole of what colour is for.
  | Phase
  -- | This control's own colour when the flag is on, dark when it is off.
  | Lit Flag Tone

derive instance Eq Light

-- | The per-loop booleans an encoder can show. A closed set rather than a
-- | function in the record, so `Control` stays a value that can be compared and
-- | tested.
-- | **The per-loop booleans, and two that are not.** `FClick` and `FMonitor`
-- | are facts about the rig rather than about a loop, which is why `flagOn`
-- | cannot answer them from a `LoopState` and `leds` fills them in from the
-- | `Rig` — the same seam `RigValue` opened for the launch knob.
data Flag = FReverse | FPendulum | FOneShot | FLevelArm | FGrid | FRevox
  | FClick | FMonitor

derive instance Eq Flag

flagName :: Flag -> String
flagName = case _ of
  FReverse -> "reversed"
  FPendulum -> "pendulum"
  FOneShot -> "one-shot"
  FLevelArm -> "listening"
  FGrid -> "on the grid"
  FRevox -> "a tape"
  FClick -> "the click is on"
  FMonitor -> "the input is monitored"

-- | One encoder: what it is about, what a press means, what a turn means, and
-- | what it shows.
type Control =
  { subject :: Subject
  , press :: Maybe Duty
  , turn :: Maybe Param
  , ring :: RingSource
  , light :: Light
  -- | Whether this control's press is its way *home* — back to the parameter's
  -- | resting value — rather than an act of its own. Only the card reads it,
  -- | and only to choose a phrase: "back to unity" says something different
  -- | from "flip direction" and both are presses on a knob.
  , home :: Boolean
  -- | **The pager, and the one control that asks nothing of the looper.**
  -- |
  -- | A `Duty` is a thing you can ask of Itajara, and turning a page is not one
  -- | — no MC6 switch could ever mean it. So it is flagged here rather than
  -- | given a duty, which keeps the vocabulary honest: `perform` stays the only
  -- | route to the socket and this never reaches it.
  -- |
  -- | Turn picks the page and press goes home to the loops, because the turn is
  -- | an absolute position and pages are what a position is good at — which is
  -- | also why a third page costs nothing here.
  , pager :: Boolean
  }

blank :: Control
blank =
  { subject: Focused, press: Nothing, turn: Nothing
  , ring: NoRing, light: Dark, home: false, pager: false
  }

-- | The pager, in the bottom-right corner of every page: the corner a hand can
-- | find without looking, and the same corner on all of them so it is one
-- | control rather than one per page.
pager :: Control
pager = blank { ring = PageRing, light = Steady Teal, pager = true }

-- | The palette, as a type rather than seven loose integers.
-- |
-- | **So that the legend on screen and the light on the device cannot
-- | disagree.** A cheat sheet that named its own colours would be a second
-- | table, and the second table is the one that rots — the same argument
-- | `Data.Looper.Banks` makes about switch labels. `Component.Looper.TwisterMap`
-- | prints `toneName`; `leds` sends `hue`; both start here.
data Tone = Red | Orange | Yellow | Green | Teal | Blue | Violet

derive instance Eq Tone

-- | Wheel positions, taken from the range the twelve pedals already pick theirs
-- | from. **Approximate and unverified**: nothing here has been seen on the
-- | hardware, and the phase colours are the ones that most want an eye on them
-- | — `DESIGN-TWISTER` §12. Zero is the device's own "off".
hue :: Tone -> Int
hue = case _ of
  Red -> 78
  Orange -> 68
  Yellow -> 58
  Green -> 40
  Teal -> 30
  Blue -> 12
  Violet -> 100

toneName :: Tone -> String
toneName = case _ of
  Red -> "red"
  Orange -> "orange"
  Yellow -> "yellow"
  Green -> "green"
  Teal -> "teal"
  Blue -> "blue"
  Violet -> "violet"

-- | What one encoder carries.
-- |
-- | **Four pages, cut by the kind of act rather than by the subject.**
-- |
-- | It was two: the loops, and *everything about the loop in hand*. That second
-- | one was not a function, it was a drawer — a fader you ride, a crossfade you
-- | set between takes and a mode you choose once a session, all on one page
-- | because they were all about the same loop. Sorting them by *when you reach
-- | for them* is what produced these four, and it left the third page nobody
-- | could think of a tenant for lying in the leftovers.
-- |
-- |     Loops    what is sounding, and opening the write head
-- |     The set  the eight against each other — where each sits, whether it runs
-- |     Shape    the loop in hand, while you play it
-- |     Set up   the loop in hand, before and between takes
-- |
-- | The set is the **transpose** of Shape: one parameter across every loop
-- | where Shape is every parameter of one loop. Its eight encoders are in the
-- | same eight positions as the Loops page, so which knob is which loop is
-- | learned once and only the verb underneath changes.
-- |
-- | Four is also what the pager's travel allows — `pageStep` at 32 puts four
-- | bands in 127 — so this fills the surface rather than reserving part of it.
controlAt :: Knob -> Control
controlAt k = case k.bank of
  0 -> loopsBank k.index
  1 -> theSetBank k.index
  2 -> shapeBank k.index
  3 -> setUpBank k.index
  _ -> blank

-- | The eight loops, then the eight verbs that act on whichever is in hand.
-- |
-- | The loops fill the top two rows exactly, which is why there are eight of
-- | them: the count came from this grid rather than from the pedal. Each is
-- | **both** the selector and the knob — press to take it in hand, turn to set
-- | how often it sounds, without the turn stealing focus from anything.
loopsBank :: Int -> Control
loopsBank i
  | i < nLoops =
      blank
      { subject = OnLoop i
      , press = Just (SelectLoop i)
      -- **Level, and this is the control the whole surface turns on.**
      --
      -- It was chance, on the argument that chance was the closest thing the
      -- engine had to a level. Then the device answered: *it is virtually
      -- impossible to press one of these without rotating it a little on the
      -- way down.* So the value under a loop's press is the value a press will
      -- nudge — and a nudge that changes how often a loop sounds is a nudge you
      -- cannot hear until the pass it eats. A nudge to the level you hear
      -- immediately and correct without thinking.
      --
      -- The engine grew `vol` for this. See `Component.App.handleEncoderTurn`
      -- for the other half: a turn is withheld until it is clear it was not
      -- part of a press.
      , turn = Just PLevel
      -- **The level, and it has to be.** This was the playhead — a turning ring
      -- per loop, at snapshot rate, and much the prettiest thing on the surface.
      -- It cost a session: on the Twister the ring *is* the encoder's value, so
      -- writing a playhead into it was writing a playhead into the value the
      -- next touch would send. A loop selected for recording went silent,
      -- because the press nudged the encoder and the encoder said whatever the
      -- playhead had left there — near zero at the top of a cycle.
      --
      -- So a ring on an encoder that also carries a value belongs to that value
      -- and to nothing else. The playhead is on the Looper page, where it can
      -- be a playhead without also being an instruction. `DESIGN-TWISTER` §16
      -- asked which of the two should have the ring; the hardware answered.
      , ring = Value PLevel
      , light = Phase
      }
  | otherwise = case i of
      8 -> verb RecordLoop Red
      9 -> verb OverdubLoop Orange
      10 -> verb Transport Green
      -- **Arm is back, and it is the fourth member of this row.**
      --
      -- It came off in favour of Revox on the argument that `ArmLoop` is
      -- `lev1` then `r` — the mode plus the gesture — and the mode was already
      -- on the surface as Listen, one press away. That was two page turns away
      -- as well, which the argument did not count, and it made the most
      -- time-critical gesture in the rig the slowest thing on the controller.
      -- Listen keeps its place on Set up, as the mode it is.
      --
      -- The row now matches the MC6's own loop page switch for switch —
      -- Record, Overdub, Stop/Go, Arm — so the two surfaces stop disagreeing
      -- about what the write head is. Each wears the colour of the phase it
      -- produces, which is the phase key doing double duty: red is recording,
      -- orange is overdubbing, green is playing, violet is armed.
      --
      -- Revox moved to Set up, beside the two knobs that say what a tape pass
      -- does. It is a mode, and it is the only mode whose parameters were
      -- reachable from nowhere but a slider on a web page.
      11 -> verb ArmLoop Violet
      -- Undo and Redo were two cells doing one job. The stack is an axis, and
      -- this device reports absolute positions, so it is a knob: turn down to
      -- undo, up to redo, ring shows how deep you are. Press still undoes one,
      -- for when it is a gesture rather than a scrub.
      12 -> knob PLayers Undo Blue
      -- Red rather than violet. It shared violet with the Revox flag that used
      -- to sit two cells up — a destructive verb and a mode wearing one colour
      -- on a surface whose whole case for colour is that it is taken in rather
      -- than read. Record is red as well and they are the two that change what
      -- is on the tape; nothing else on the page is.
      13 -> verb ClearLoop Red
      -- The one thing a pedal cannot do, and the one thing a *hand* is worst
      -- placed to remember to do — so it gets a control on both surfaces.
      14 -> verb ClaimPast Yellow
      15 -> pager
      _ -> blank

-- | **The eight loops against each other**: where each one sits, and whether it
-- | is running.
-- |
-- | The transpose of `shapeBank` — one parameter across every loop, where that
-- | page is every parameter of one loop. Pan is the parameter because placing
-- | loops is inherently comparative: you are listening to where the *others*
-- | are, which is exactly what a page of one loop cannot help you with.
-- |
-- | **The same eight positions as the Loops page**, deliberately. Loop 3 is the
-- | same knob on both, so which encoder is which loop is learned once and only
-- | the verb underneath changes. Turn is pan and press is stop-or-go, so the
-- | page is a mixer: the two things you do to a set of loops while deciding
-- | what the set is.
-- |
-- | The press-nudge is harmless here for the same reason it is under a level: a
-- | pan you did not mean is one you hear at once and correct without thinking.
-- | It would not be harmless under chance.
theSetBank :: Int -> Control
theSetBank i
  | i < nLoops =
      blank
      { subject = OnLoop i
      , press = Just Transport
      , turn = Just PPlace
      , ring = Value PPlace
      , light = Phase
      }
  | otherwise = case i of
      -- **The panic row, and it had no hand-reachable control at all.** Stop
      -- all and Start all live on the MC6's global row and nowhere else, so
      -- with the looper holding this controller there was no way to stop
      -- everything without reaching for a foot.
      --
      -- On the bottom row beside the pager rather than directly under the
      -- loops: these are the only three controls on the page that are not
      -- *a* loop, and the row the pager already sits on is the page's row for
      -- things that are about all of it.
      -- **The rig's own quantise, on the rig's own page.** It was going to go
      -- on Set up, and Set up is about the loop in hand — a global on a
      -- per-loop page is how the old page two became a drawer. This page's
      -- subject is already "all of them".
      --
      -- Its ring is a `RigValue`: the only knob on the surface that reads
      -- something other than a loop.
      8 -> blank
            { turn = Just PLaunch
            , ring = RigValue PLaunch
            , light = Steady Teal
            }
      -- **The click, on the surface at last.** It was on the MC6's global row
      -- and on a web button and nowhere a hand on this controller could reach —
      -- which was survivable while the click followed a recorded loop, and is
      -- not now that it follows the grid and ticks beats before anything has
      -- been recorded. Counting yourself in is the first move of the first
      -- take, and it needed a mouse.
      --
      -- `Lit` rather than `Steady`, because whether the click is on is a fact
      -- about the rig you want to see from across the room.
      9 -> blank { press = Just ClickToggle, light = Lit FClick Teal }
      10 -> blank { press = Just MonitorToggle, light = Lit FMonitor Blue }
      12 -> verb StopAll Blue
      13 -> verb StartAll Green
      14 -> verb ClearAll Red
      15 -> pager
      _ -> blank

-- | **The loop in hand, while you are playing it.**
-- |
-- | Everything here is either continuous or instantly reversible, which is the
-- | test for belonging on this page rather than on Set up. Spread, shift and
-- | dense pass it on the vocabulary's own words — *structural, instant and
-- | reversible; it records nothing* — and Save take passes it because the thing
-- | you do with a phrase you like is save it while it is still true.
-- |
-- | Level is not here and pan is not here: both are per-loop and both have a
-- | page where all eight are visible at once, which is the better place to set
-- | either. This page is for what only makes sense one loop at a time.
-- |
-- | **No start or end trim yet.** They belong here and the daemon has no verb
-- | for either; a knob that moved nothing would be exactly the failure this
-- | surface exists to avoid, so the row stays empty until the engine grows
-- | them. Empty is honest and a page has no obligation to be full.
shapeBank :: Int -> Control
shapeBank = case _ of
  -- **Speed carries its own direction now**, so Reverse has no cell: the
  -- daemon takes ±0.125 to ±4 and the sign *is* the direction, which made a
  -- separate flag a second spelling of a number's sign. Centre is stopped and
  -- the press is unity — two positions the hand can find, one by feel and one
  -- by pressing. See `rateOf`.
  0 -> knob PRate (Rate 1.0) Teal
  1 -> knob PDecay (Decay 0.0) Orange
  2 -> knob PChance (Chance 1.0) Yellow

  -- Spread to make room, shift to decide where in it the bar falls, dense as
  -- the way back. **Presses rather than knobs**, and for a stated reason: the
  -- snapshot reports no per-loop spread, so a spread knob would hold a position
  -- nothing could correct — the one thing this surface is not allowed to do.
  -- It wants to be a knob and cannot be one yet.
  -- **Multiply, at last on a surface a hand can reach.** It was on the CC table
  -- and so on a web button, on no MC6 bank and no encoder — a verb the
  -- vocabulary had and no hand could send. It is here rather than on Set up
  -- because it is not a length *setting*, it is a length *performance*: press
  -- once to open, play across as many cycles as you want, press again. The
  -- write head is open the whole time, which is why pressing it feels like an
  -- overdub — it is one, that also lengthens the loop.
  --
  -- Its declarative twin is `bars` on Set up: name the number instead of
  -- playing it. Both are worth having — you count when you know and you play
  -- when you do not.
  4 -> verb MultiplyLoop Orange
  5 -> verb RotateLoop Violet
  6 -> verb DenseLoop Violet
  -- Blue rather than violet. It sat in a row of four identical violets, and a
  -- colour four things share says nothing — this one is the Atlantis seam, not
  -- a structural edit, and it is the only cell here that leaves the rig.
  7 -> verb SaveTake Blue

  15 -> pager
  _ -> blank

-- | **The loop in hand, before and between takes.**
-- |
-- | The modes and the settings — what a loop *will* do, decided when you are
-- | not mid-phrase. Fade is here rather than on Shape because a crossfade is a
-- | property of the join, chosen once and then forgotten; grid and one-shot
-- | because they change what a press will mean, which is not a thing to
-- | discover by turning something.
-- |
-- | **Listen is the mode Arm is the gesture of.** They were both on the surface
-- | and the duplication was the reason Arm came off it; keeping the persistent
-- | flag here and the one-press gesture on the Loops page is the division that
-- | makes them two things rather than two spellings.
-- |
-- | The bottom row is the tape, whole: the mode, what a pass leaves of what was
-- | under it, and how much top it keeps. Those last two had **no control on
-- | either hardware surface** — only sliders on a web page, for the one mode in
-- | the rig that has no undo.
setUpBank :: Int -> Control
setUpBank = case _ of
  -- `Free` is not here: `GridToggle` turns the grid off as well as on, and the
  -- third of the three erasures had nowhere else to live.
  0 -> flagged GridToggle FGrid Teal
  1 -> flagged OneShot FOneShot Yellow
  2 -> flagged LevelArm FLevelArm Green
  -- Back from the MC6-only list. It is a once-a-session mode, which is an
  -- argument for putting it on the page where once-a-session modes live rather
  -- than an argument for it having no knob at all.
  3 -> flagged Pendulum FPendulum Violet

  -- **Length, and how the material lands in it.** Three knobs where there were
  -- two presses, and the two presses could not be told apart: `SpreadLoop` set
  -- how often a layer sounded *and* grew the loop by the same factor, so "how
  -- long is this" and "how often does this sound" were one gesture.
  --
  -- Apart, they are the thing this was asked for: record a bar, make the loop
  -- four, put the bar on the third of them. `bars` is the length, `every` is
  -- how often, `on` is which slot — and the waveform draws the answer, which is
  -- a picture of where the sound is rather than a sentence about how often it
  -- happens.
  --
  -- `bars` on an empty loop is the other half: size it first and the recording
  -- closes itself, so the second press stops being part of the gesture.
  4 -> knob PBars (SetBars 1) Teal
  5 -> knob PEvery (Every 1) Violet
  6 -> knob POn (PlaceAt 1) Violet
  -- Displaced by those three, and it belongs here anyway: forgetting a length
  -- is a between-takes decision, and it is the way back from having declared
  -- one.
  7 -> verb ForgetLength Blue

  -- **Press is the mode; turn threads the tape.**
  --
  -- They belong on one control because they are one idea: a tape is a loop of a
  -- chosen length that you play onto, and choosing the length is how you start.
  -- Everywhere else in this app a loop gets its length by being recorded, which
  -- is exactly what Revox does not do.
  --
  -- Turning is refused by the daemon once the loop has anything in it, so this
  -- cannot resize a take by accident — and the ring still reads the loop's real
  -- length, which is worth seeing either way.
  8 -> blank
        { press = Just RevoxToggle
        , turn = Just PTape
        , ring = Value PTape
        , light = Lit FRevox Violet
        }
  9 -> knob PFeedback (Feedback 0.0) Red
  10 -> knob PTone (Tone toneCeil) Teal
  11 -> knob PFade (Fade 0.0) Green

  15 -> pager
  _ -> blank

verb :: Duty -> Tone -> Control
verb d tone = blank { press = Just d, light = Steady tone }

flagged :: Duty -> Flag -> Tone -> Control
flagged d f tone = blank { press = Just d, light = Lit f tone }

knob :: Param -> Duty -> Tone -> Control
knob p rest tone =
  blank { press = Just rest, turn = Just p, ring = Value p
        , light = Steady tone, home = true }

-- | What a press means, if anything.
pressedAt :: Knob -> Maybe (Tuple Subject Duty)
pressedAt k =
  let c = controlAt k
  in map (Tuple c.subject) c.press

-- | What a turn means, if anything. The value is the encoder's absolute
-- | position, 0–127.
turnedAt :: Knob -> Int -> Maybe (Tuple Subject Duty)
turnedAt k v =
  let c = controlAt k
  in map (\p -> Tuple c.subject (fromKnob p v)) c.turn

-- | **The only place a knob position becomes a value**, and the counterpart of
-- | `Data.Looper.Verb.render`: one direction, one function, no second spelling.
-- |
-- | Every scale has its home at a detent the hand can find. Speed is
-- | exponential with unity at the centre, because the useful resolution is near
-- | one and the range is five octaves; pan is the identity, because the
-- | daemon's own units are already 0–127 with 64 centre.
fromKnob :: Param -> Int -> Duty
fromKnob p v = case p of
  -- Whole seconds, because that is the unit you think a loop length in, and
  -- because the daemon rounds it to the grid when the loop is quantised — only
  -- it knows where the grid is.
  PTape -> Blank (toNumber (round (toNumber (clamp 0 127 v) / 127.0 * tapeTop)))
  PLayers -> Layers (round (toNumber (clamp 0 127 v) / 127.0 * toNumber maxLayers))
  PLevel -> Level (round1 (levelOf (clamp 0 127 v)))
  PRate -> Rate (rateOf v)
  PPlace -> Place (detented p (clamp 0 127 v))
  PFade -> Fade (toNumber (round (toNumber (clamp 0 127 v) / 127.0 * fadeTop)))
  -- **Full at the top and counting down**, the same way round as the level.
  -- It used to be the other way — zero at the bottom, more decay as you turned
  -- up — which put the knob's rest position at the far left and meant the first
  -- thing a turn did was take material away. A control whose home is at one end
  -- and whose effect is destruction is one you turn by accident.
  PDecay -> Decay (round1 (decibelsAt decayLaw (clamp 0 127 v)))
  PChance -> Chance (round3 (toNumber (clamp 0 127 v) / 127.0))
  PFeedback -> Feedback (round1 (feedbackFloor + toNumber (clamp 0 127 v) / 127.0 * negate feedbackFloor))
  PTone -> Tone (toNumber (round (toneFloor + toNumber (clamp 0 127 v) / 127.0 * (toneCeil - toneFloor))))
  PBars -> SetBars (stepOf maxBars v)
  PEvery -> Every (stepOf maxBars v)
  -- One-based on the wire, because that is how the daemon counts slots back to
  -- you — and it wraps *there* rather than being clamped here, since how many
  -- slots there are depends on `PEvery` and this is deliberately a pure
  -- function of a position.
  POn -> PlaceAt (stepOf maxBars v)
  PLaunch -> Launch (fromMaybe (-1) (Array.index launchLadder (bandOf (Array.length launchLadder) v)))

-- | Where a value sits on the ring — the inverse of `fromKnob`, and the reason
-- | the device never has to remember anything.
toKnob :: Param -> LoopState -> Int
toKnob p st = clamp 0 127 $ case p of
  -- The loop's actual length, so the knob shows what is threaded — including
  -- for a loop that was *recorded* rather than threaded, where turning would be
  -- refused but the reading is still the truth.
  PTape -> round (st.loopSecs / tapeTop * 127.0)
  PLayers -> round (toNumber st.layers / toNumber maxLayers * 127.0)
  PLevel -> levelRing st.volDb
  -- **Composed, because the snapshot keeps the two halves apart.** `speed` is a
  -- magnitude and the direction is `reverse`, so reading the ring off `speed`
  -- alone drew a loop running backwards at half speed exactly like one running
  -- forwards at half speed. It had been doing that for as long as the knob has
  -- existed, and it was invisible while the knob could only ask for one sign.
  PRate -> rateRing (if st.reverse then negate st.speed else st.speed)
  PPlace -> st.pan
  PFade -> round (st.fadeMs / fadeTop * 127.0)
  PDecay -> positionAt decayLaw st.decayDb
  PChance -> round (st.chance * 127.0)
  PFeedback -> round ((st.fbDb - feedbackFloor) / negate feedbackFloor * 127.0)
  PTone -> round ((st.toneHz - toneFloor) / (toneCeil - toneFloor) * 127.0)
  -- Zero means nobody has said, and a loop nobody has measured is one bar.
  PBars -> stepRing maxBars (max 1 st.cycles)
  PEvery -> stepRing maxBars (max 1 (newestOf _.period st))
  POn -> stepRing maxBars (max 1 (newestOf _.phase st + 1))
  -- Not about a loop; `leds` reads it from the rig. See `RingSource`.
  PLaunch -> 0

-- | Silence at the bottom of the travel and unity at the top, with a **fader
-- | law** rather than a straight line.
-- |
-- | This was linear in decibels for half a day and the note said so, arguing
-- | that a two-segment taper was two chances to get the inverse wrong.
-- | Andrew, having turned it: *"it seems to drop VERY quickly as you turn the
-- | knob."* Which is what a straight line from 0 to -60 does — half the travel
-- | sits below -30 dB where nothing is audible, so all the useful range is
-- | crammed into the top few degrees.
-- |
-- | So it bends where a real fader bends. The top half of the travel spends
-- | itself on the first 12 dB, which is where mixing happens; the bottom half
-- | covers the remaining 48 down to silence, which is where fading out happens.
-- |
-- | ```
-- |   127  ────────  0 dB      full
-- |    96  ────────  -6 dB
-- |    64  ────────  -12 dB    half travel
-- |    32  ────────  -36 dB
-- |     0  ────────  silent
-- | ```
-- |
-- | The objection to bending it was real and the answer is a test rather than a
-- | straight line: `toKnob` is the exact inverse of this, and the suite walks
-- | all 128 positions to prove the round trip lands within one step.
-- |
-- | Position 0 is silence outright rather than -60 dB, because a fader that
-- | cannot reach zero is a fader you do not trust — and because the daemon
-- | reads -60 as a real zero for the same reason.
-- | A fader law: full at the top, bending at half travel, silence at the
-- | bottom. **One pair of functions, used by both knobs that need one**, so
-- | there is exactly one place to get the inverse right.
-- |
-- | `knee` is the position it bends at, `kneeDb` how far down it has come by
-- | then, and `floorDb` where the bottom of the travel lands.
type Law = { knee :: Int, kneeDb :: Number, floorDb :: Number }

decibelsAt :: Law -> Int -> Number
decibelsAt law v
  | v <= 0 = negate law.floorDb
  | v >= 127 = 0.0
  | v >= law.knee =
      negate (law.kneeDb * toNumber (127 - v) / toNumber (127 - law.knee))
  | otherwise =
      negate law.kneeDb
        - (law.floorDb - law.kneeDb) * toNumber (law.knee - v) / toNumber law.knee

-- | The exact inverse, which is what lets the device be told where the knob is
-- | standing instead of remembering it.
positionAt :: Law -> Number -> Int
positionAt law db
  | db <= negate law.floorDb = 0
  | db >= 0.0 = 127
  | db >= negate law.kneeDb =
      127 - round (toNumber (127 - law.knee) * negate db / law.kneeDb)
  | otherwise =
      law.knee
        - round (toNumber law.knee * (negate db - law.kneeDb) / (law.floorDb - law.kneeDb))

-- | The level fader, bent **twice** now, and both times by turning it.
-- |
-- | It was linear in decibels: half the travel below -30 dB where nothing is
-- | audible. Andrew: *"it seems to drop VERY quickly as you turn the knob."*
-- | So it got a knee at half travel and 12 dB above it — and the same report
-- | came back: *"dropping to very very quiet even just with half a turn."*
-- |
-- | Which it was. Twelve down at half travel is a quarter of the amplitude, and
-- | the knee was in the wrong place besides: putting it at the middle spends
-- | half the knob on the top 12 dB and half on the remaining 48, so the useful
-- | mixing range and the fade-to-nothing range got the same room. Mixing needs
-- | more of the knob than fading does, because fading is a gesture and mixing
-- | is a decision.
-- |
-- | Three quarters of the travel now spend themselves on the first 9 dB, which
-- | puts **half a turn at -6 dB** — half the amplitude, the number a fader is
-- | expected to give you there:
-- |
-- | ```
-- |   127  ────────  0 dB      full
-- |    96  ────────  -3 dB
-- |    64  ────────  -6 dB     half travel
-- |    32  ────────  -9 dB     the knee
-- |    16  ────────  -35 dB
-- |     0  ────────  silent
-- | ```
-- |
-- | The bottom quarter is steep on purpose: everything below -9 dB is a loop
-- | going away, and the whole of that is one gesture rather than a set of
-- | values you pick between.
levelLaw :: Law
levelLaw = { knee: 32, kneeDb: 9.0, floorDb: decayFloor }

-- | **Decay, and the range is the ladder's rather than the daemon's.**
-- |
-- | `decayLadder` is this project's own opinion about which values are worth
-- | reaching — hold, -1, -3, -6, -12 — and the knob used to span the daemon's
-- | whole 0 to -60. So every musically useful value sat in the top fifth of the
-- | travel and anything below it was "gone within a pass or two": Andrew,
-- | turning it, *"as soon as you turn it the loop goes silent."*
-- |
-- | Twelve down is the bottom now, matching the ladder and the MC6 switch that
-- | steps it. The console can still ask for -60; a knob does not have to be
-- | able to reach everything a command can.
decayLaw :: Law
decayLaw = { knee: 64, kneeDb: 3.0, floorDb: 12.0 }

levelOf :: Int -> Number
levelOf = decibelsAt levelLaw

levelRing :: Number -> Int
levelRing = positionAt levelLaw

-- | How many layers a loop can hold. **Mirrors `MAX_LAYERS` in the engine**;
-- | the snapshot carries the authoritative number as `maxLayers` if this ever
-- | needs checking rather than trusting.
maxLayers :: Int
maxLayers = 8

-- | The longest tape the knob offers, in seconds.
-- |
-- | Thirty because that is `--max-secs`' default and the daemon refuses past
-- | it; a knob whose top end is a refusal is a knob with a dead corner.
tapeTop :: Number
tapeTop = 30.0

-- | Two hundred milliseconds rather than the daemon's five hundred: past a
-- | tenth of a second a wrap fade is not a join any more, so spending three
-- | fifths of the knob on it would be spending it on the part nobody turns to.
fadeTop :: Number
fadeTop = 200.0

decayFloor :: Number
decayFloor = 60.0

-- | Three octaves down and two up, with unity exactly at the centre detent.
-- |
-- | Asymmetric on purpose: half speed is a thing you want often and quarter
-- | speed is a texture, while four times is already mostly aliasing — so the
-- | knob spends more of itself below one than above. The daemon refuses
-- | anything outside 0.125 to 4 either way, so both ends stop where it stops.
-- | **Stopped at the centre, the sign is the direction, and it steps.**
-- |
-- | The knob ran ×0.125 to ×4 with unity in the middle, and `Reverse` was a
-- | separate cell. Two things were wrong with that. The ring could not show
-- | which way round a loop was — see `toKnob` — and the surface spent a cell on
-- | a fact the parameter already carries: the daemon takes ±0.125 to ±4 and
-- | *the sign is the direction*, which `Data.Looper.Verb.Rate` has said all
-- | along. Reverse was a second spelling of a number's sign.
-- |
-- | So it is bipolar, which is how the Chase Bliss pedals and the Count to 5 do
-- | it and it reads immediately: centre is stopped, either way out is faster,
-- | left is backwards.
-- |
-- | **And it steps, for the same reason those pedals do.** A continuous speed
-- | is a continuous *transposition*, so every position between the useful ones
-- | is a loop out of tune with the rest of the rig — which on a knob you turn
-- | while playing is not a setting, it is a wrong note you have to hunt back
-- | out of. `rateLadder` is every ratio between an eighth and four that is a
-- | product of octaves and fifths, so any position the knob can reach is in
-- | key with every other one.
-- |
-- | Adjacent steps are alternately a fifth and a fourth, which is the same
-- | statement: a fourth is a fifth inverted, and the whole ladder is
-- | `2^a · 3^b`. Nothing here is tempered — these are the just ratios, because
-- | a resampled loop transposes by exactly the ratio you resampled it at and
-- | there is nothing to temper against.
-- |
-- | **The steps are the detent.** Twenty-three of them across 128 units is five
-- | and a half units each, which is wide enough to find and to sit in, so the
-- | centre needs no special case: the middle step is stopped and it is as wide
-- | as any other.
-- |
-- | **This asks one thing of the daemon**: `sp0` is refused today, since `Rate`
-- | is ±0.125 to ±4 with nothing in between. Until it takes zero the middle
-- | step is a request the engine declines — which the ack path says out loud,
-- | rather than the knob pretending.
-- |
-- | **Held is not stopped.** A loop at speed zero is still playing: it has not
-- | given up its place in the phase-locked set and it is not muted, where
-- | `Transport` silences one and keeps its position. The two look alike from
-- | outside and are not, so `phaseTone` gives held a colour of its own.
rateLadder :: Array Number
rateLadder =
  [ 0.125, 0.167, 0.25, 0.333, 0.5, 0.667, 1.0, 1.5, 2.0, 3.0, 4.0 ]

-- | The ladder both ways round with stopped in the middle: what the knob can
-- | reach, left to right.
rateSteps :: Array Number
rateSteps =
  map negate (Array.reverse rateLadder) <> [ 0.0 ] <> rateLadder

rateOf :: Int -> Number
rateOf v = fromMaybe 0.0 (Array.index rateSteps (rateBand v))

-- | Which step a position falls in. Equal bands, so no step is easier to reach
-- | than its neighbours — the ladder is already uneven in ratio and making it
-- | uneven in travel as well would be two kinds of irregular at once.
rateBand :: Int -> Int
rateBand v =
  clamp 0 (Array.length rateSteps - 1)
    (clamp 0 127 v * Array.length rateSteps / 128)

rateRing :: Number -> Int
rateRing s = bandCentre (nearestStep s)
  where
  bandCentre i =
    clamp 0 127
      (round ((toNumber i + 0.5) * 128.0 / toNumber (Array.length rateSteps)))
  -- The engine is free to be at a speed no step names — another client, or a
  -- daemon that rounded — so the ring shows the step it is nearest rather than
  -- refusing to show anything.
  nearestStep x =
    Array.foldl
      (\best i -> if closer i best then i else best)
      0
      (Array.range 0 (Array.length rateSteps - 1))
    where
    closer i best = gap i < gap best
    gap i = numAbs (fromMaybe 0.0 (Array.index rateSteps i) - x)

-- | **The newest layer's is the loop's**, for the two controls that address one
-- | layer rather than a loop: `sparse` and its friends in the daemon act on the
-- | last layer laid down, so the ring has to read the same one or the knob would
-- | show a number nothing it sends could change.
newestOf :: (LayerShape -> Int) -> LoopState -> Int
newestOf f st = maybe 0 f (Array.last st.shapes)

-- | How many bars a loop may be, and how sparsely a layer may sound.
-- |
-- | **The encoder's limit rather than the engine's**, and the same number the
-- | daemon uses. Nothing would struggle with 64 of either; a Midifighter
-- | encoder over 64 steps is two units a step, and this hardware moves an
-- | encoder when you press it — which is measured, not guessed. Thirty-two
-- | gives four units a step and is already the tight end. The console can ask
-- | for more than a knob can reach, as it can with decay.
maxBars :: Int
maxBars = 32

-- | Equal bands across the travel, one per step, and the value is one-based.
stepOf :: Int -> Int -> Int
stepOf n v = bandOf n v + 1

-- | Which band a position falls in, for any knob with `n` equal steps.
bandOf :: Int -> Int -> Int
bandOf n v = clamp 0 (n - 1) (clamp 0 127 v * n / 128)

-- | The middle of the band a one-based step owns — the inverse of `stepOf`, and
-- | mid-band so a step is somewhere to sit rather than an edge to fall off.
stepRing :: Int -> Int -> Int
stepRing n step =
  clamp 0 127 (round ((toNumber (clamp 1 n step) - 0.5) * 128.0 / toNumber n))

-- | What a launch can wait for, in beats: none, then the useful subdivisions
-- | and multiples, then the bar.
-- |
-- | **The bar sits at the top rather than among the numbers**, and is spelled
-- | `-1` rather than `4`, because a bar is what the metre says it is — in 3/4
-- | it is three beats — and a ladder that spelled it four would be right in one
-- | time signature and quietly wrong in every other.
launchLadder :: Array Int
launchLadder = [ 0, 1, 2, 3, 4, 6, 8, 12, 16, 32, -1 ]

launchRing :: Int -> Int
launchRing q =
  let i = fromMaybe (Array.length launchLadder - 1) (Array.elemIndex q launchLadder)
  in stepRing (Array.length launchLadder) (i + 1)

-- | The slowest magnitude the daemon accepts either side of zero, and so the
-- | bottom rung of the ladder. Anything under it is stopped.
rateFloor :: Number
rateFloor = 0.125

-- | What a tape pass may leave of what was under it, and how much top it keeps.
-- | Both match the sliders these two knobs replace on the Looper page.
feedbackFloor :: Number
feedbackFloor = -24.0

toneFloor :: Number
toneFloor = 1000.0

toneCeil :: Number
toneCeil = 20000.0

numAbs :: Number -> Number
numAbs x = if x < 0.0 then negate x else x

-- | A loop that is turning and not advancing.
held :: LoopState -> Boolean
held st = numAbs st.speed < rateFloor

round3 :: Number -> Number
round3 x = toNumber (round (x * 1000.0)) / 1000.0

round1 :: Number -> Number
round1 x = toNumber (round (x * 10.0)) / 10.0

-- | One encoder's lights, by position on the page.
-- |
-- | **No bank in it, deliberately.** Which CC block these are written to is a
-- | question about where the *device* is, and this module has no business
-- | knowing that — `Component.App` addresses them, because it is the thing that
-- | holds both facts: the page the app is showing and the block the device is
-- | on. They are usually the same and are allowed not to be.
-- | `ringHeld` marks a ring the **device** owns: the app may read where it is
-- | but must not write it, or it would be moving the knob under the hand
-- | turning it. Only the pager is one. See `pagerRing`.
type Led = { index :: Int, ring :: Int, hue :: Int, ringHeld :: Boolean }

-- | What the sixteen encoders should look like on one page, given what the
-- | daemon says.
-- |
-- | One page rather than all four. Writing every bank was the earlier design
-- | and it only made sense while the device owned the paging: it kept the pages
-- | you were not looking at correct for the moment you switched. Now the app
-- | switches, and it redraws when it does — so lighting a block nobody is
-- | looking at is sixteen messages a frame spent on nothing.
leds :: Rig -> Int -> Array Led
leds rig bank = do
  index <- Array.range 0 (encodersPerBank - 1)
  let c = controlAt { bank, index }
      loop = Array.index rig.loops (subjectIndex rig c.subject)
  pure
    { index
    -- The pager's ring is the one the app must not touch — it IS the page
    -- selector, and the position is the device's. Reported as 0 so it never
    -- enters the diff; `pagerRing` is written deliberately elsewhere.
    , ring: case c.ring of
        PageRing -> 0
        RigValue p -> rigKnob p rig
        _ -> maybe 0 (ringOf c) loop
    , ringHeld: c.pager
    -- The colour is the app's to say, and says the page.
    , hue: if c.pager then hue (pageTone bank)
           else case c.light of
             -- The two rig-wide flags, answered from the rig. Everything else
             -- on this surface is about a loop and reads one.
             Lit FClick t -> if rig.click then hue t else 0
             Lit FMonitor t -> if rig.monitor then hue t else 0
             _ -> maybe (dark c) (hueOf c) loop
    }
  where
  dark c = case c.light of
    Steady t -> hue t
    _ -> 0

ringOf :: Control -> LoopState -> Int
ringOf c st = case c.ring of
  NoRing -> 0
  PageRing -> 0
  RigValue _ -> 0
  Value p -> toKnob p st

-- | Where a rig-wide value stands on its knob. The counterpart of `toKnob` for
-- | the one parameter that is not about a loop.
rigKnob :: Param -> Rig -> Int
rigKnob p rig = case p of
  PLaunch -> launchRing rig.launchQ
  _ -> 0

-- | Where the encoder stands for a given page, when the *app* is the one moving
-- | it.
-- |
-- | **The pager reads its own absolute position, and the device owns that
-- | position.** Two designs were tried and both were wrong in the same way —
-- | they had the app rewriting the ring, which meant the app deciding where
-- | "here" was and the knob arguing with it:
-- |
-- | * *Position over the whole travel.* Two pages meant sweeping half the
-- |   encoder, and a third page made each band narrower rather than the gesture
-- |   smaller.
-- | * *A step from a parked position.* Parked at the page's own end there was
-- |   no travel left to turn into (forward-wrap was unreachable on hardware);
-- |   parked in the middle the ring had to be rewritten after every change, so
-- |   one direction cost a full notch and the other cost one unit.
-- |
-- | So the ring is written **only** when the app moves the page by itself —
-- | from the card's "turn to this page", or the reset that comes with taking
-- | focus. A turn is read where it lands and nothing is written back. Bands are
-- | a fixed `pageStep` wide, so a page is always the same angle however many
-- | pages there are, and the travel above the last band is simply unused.
pagerRing :: Int -> Int
pagerRing p = clamp 0 127 (p * pageStep)

-- | Which encoder the pager is, found rather than restated.
-- |
-- | It is the bottom-right corner on every page and that is written down in the
-- | layout tables; a second `15` here would be a second thing to keep true, and
-- | this file has already been wrong that way once about the side buttons.
pagerIndex :: Int
pagerIndex = fromMaybe (encodersPerBank - 1)
  (Array.find (\i -> (controlAt { bank: 0, index: i }).pager)
     (Array.range 0 (encodersPerBank - 1)))

-- | The pager's colour on each page, which says the same thing as the ring in
-- | a way you do not have to read.
-- |
-- | Two indicators for the one fact you must not be wrong about, and they
-- | cannot disagree: both are computed from the page being drawn.
pageTone :: Int -> Tone
pageTone p = case p `mod` pages' of
  0 -> Teal
  1 -> Violet
  2 -> Yellow
  _ -> Blue

-- | How far the encoder turns for one page.
-- |
-- | **A right angle**, if a revolution is the full 0-127 sweep. It was three
-- | units for a while — about five degrees — and the page changed when a hand
-- | brushed the knob.
-- |
-- | A fixed width rather than the travel divided by the page count, which is
-- | the point: a page costs another 32 units of travel instead of making every
-- | band narrower. Four pages start at 0, 32, 64 and 96, so the travel is now
-- | spent exactly — a fifth would have to make the gesture smaller, which is
-- | the trade this constant exists to refuse.
pageStep :: Int
pageStep = 32

-- | Which page the pager is pointing at.
-- |
-- | **Clamped, not wrapped.** Turning past the last page does nothing, which is
-- | what a knob with a physical end should do — and it means the gesture is
-- | reversible by turning back exactly as far, which a wrap is not. The travel
-- | above the last band is dead on purpose.
pageFor :: Int -> Int
pageFor v = clamp 0 (pages' - 1) (v / pageStep)

-- | How many pages the looper surface uses.
-- |
-- | Four, which is also as many as the pager's travel holds at `pageStep` — so
-- | the surface is now full rather than reserving room. A fifth would be an
-- | argument about the band width, not about the device: since the app owns
-- | paging outright (`Data.Twister.deviceBank`) the four *blocks* stopped being
-- | the constraint.
pages' :: Int
pages' = 4

hueOf :: Control -> LoopState -> Int
hueOf c st = case c.light of
  Dark -> 0
  Steady t -> hue t
  Phase -> maybe 0 hue (phaseTone st)
  -- Off is dark rather than a second colour. Two hues would need reading; lit
  -- and unlit can be taken in at a glance, which is the whole job of a
  -- controller you are not looking at.
  Lit f t -> if flagOn f st then hue t else 0

-- | A phase, in colour — `Nothing` for an empty slot, which is dark.
-- |
-- | The six states `LoopPhase` closes, and the reason it was closed: a missed
-- | case here is a wrong colour on a knob, which reads exactly like a loop doing
-- | something it is not.
phaseTone :: LoopState -> Maybe Tone
phaseTone st = case phaseOf st of
  RecordingFirst -> Just Red
  Overdubbing -> Just Orange
  Multiplying -> Just Yellow
  Armed -> Just Violet
  -- Three renderings of one phase, because `Playing` says only that the loop is
  -- turning — muted, skipping and now *held* are all orthogonal to it. Held is
  -- a loop at speed zero: still in the phase-locked set, still not muted, and
  -- simply not advancing. It looks like stopped from outside and is not, which
  -- is the whole reason it has a colour of its own.
  Playing -> Just (if st.muted then Blue else if held st then Teal else Green)
  -- Empty and idle are not the same thing to a player: one has material in it
  -- and is stopped, the other has nothing. The slot colour says so and this
  -- should too.
  Idle -> if st.layers == 0 then Nothing else Just Blue

flagOn :: Flag -> LoopState -> Boolean
flagOn f st = case f of
  FReverse -> st.reverse
  FPendulum -> st.pendulum
  FOneShot -> st.oneShot
  FLevelArm -> st.levelArm
  FGrid -> st.quant
  FRevox -> st.revox
  -- Not about a loop; `leds` answers these from the rig. A loop cannot know
  -- whether the click is on, and pretending it can would put the answer in the
  -- one place that is guaranteed to be wrong.
  FClick -> false
  FMonitor -> false

subjectIndex :: Rig -> Subject -> Int
subjectIndex rig = case _ of
  Focused -> rig.focus
  OnLoop i -> i

-- | The layout as words, for the cheat sheet at the bottom of the Looper page.
-- |
-- | **Generated from `controlAt`, which is the whole point.** A printed layout
-- | typed out beside the table it describes is the classic second table: right
-- | when written, wrong the first time a control moves, and wrong in the one
-- | place nobody thinks to check because it is only documentation. Every word
-- | below comes from the vocabulary — `dutyLabel`, `dutyName`, `paramLabel`,
-- | `toneName` — so a control that moves takes its own description with it.
-- |
-- | The same argument `Data.Looper.Banks` makes about `auxLegend`, and it was
-- | learned the same way: three times in one day a screen restated something
-- | that module already knew, and each copy was right when written.
type Cell =
  { index :: Int
  -- | What it is, in the largest type on the card.
  , name :: String
  -- | What a press does, and what a turn does. `Nothing` for a control that
  -- | does not do that at all — a dark cell has neither.
  , press :: Maybe String
  , turn :: Maybe String
  -- | What the light and the ring are telling you.
  , shows :: Maybe String
  -- | The colour it should be, as a word. `Nothing` where the colour is not
  -- | fixed — the loop encoders take theirs from the phase.
  , tone :: Maybe String
  }

type Page = { bank :: Int, name :: String, note :: String, cells :: Array Cell }

pages :: Array Page
pages =
  [ { bank: 0
    , name: "Loops"
    , note: "The eight loops, then the write head for whichever is in hand. Every press is at press-down: nothing here waits out a window, because nothing here needs a gesture."
    , cells: cellsOf 0
    }
  , { bank: 1
    , name: "The set"
    , note: "The same eight knobs, the same eight loops — turn to place one in the field, press to stop or start it. The transpose of Shape: one parameter across every loop, where that page is every parameter of one."
    , cells: cellsOf 1
    }
  , { bank: 2
    , name: "Shape"
    , note: "The loop in hand, while you play it. Everything here is continuous or instantly reversible; anything you would rather decide between takes is on Set up."
    , cells: cellsOf 2
    }
  , { bank: 3
    , name: "Set up"
    , note: "The loop in hand, before and between takes — the modes, the join, and the tape. What a loop will do, decided when you are not mid-phrase."
    , cells: cellsOf 3
    }
  ]

cellsOf :: Int -> Array Cell
cellsOf bank = map (cellAt bank) (Array.range 0 (encodersPerBank - 1))

cellAt :: Int -> Int -> Cell
cellAt bank index =
  let c = controlAt { bank, index }
  in if c.pager then
    { index
    , name: "page"
    , press: Just "back to the loops"
    , turn: Just ("which page — " <> show pages' <> " of them, a quarter turn each")
    , shows: Just "where it stands IS the page; the colour says so too"
    , tone: toneOf c.light
    }
  -- **A loop is described by whose it is, not by what is on it.** This matched
  -- on `SelectLoop` and hard-coded "its level", which was true of the one page
  -- that had loop encoders. The set page has eight more with a different verb
  -- and a different knob, and the old shape would have printed them as
  -- parameters with a way home.
  else case c.subject of
    OnLoop i ->
      { index
      , name: "Loop " <> show (i + 1)
      , press: case c.press of
          Just (SelectLoop _) -> Just "take it in hand"
          mp -> map dutyName mp
      , turn: map (\p -> paramLabel p <> " — " <> paramRange p) c.turn
      , shows: Just "colour is what it is doing; the ring is the value under your hand"
      , tone: Nothing
      }
    Focused -> case c.press, c.turn of
      -- A knob: the parameter names the control. Whether the press is its way
      -- home or an act of its own is `c.home`, because on this surface it is
      -- now both — speed's press is unity, and speed's press could as easily
      -- have been a direction flip.
      mp, Just p ->
        { index
        , name: paramLabel p
        , press: map (\d -> (if c.home then "back to " else "") <> dutyName d) mp
        , turn: Just (paramRange p)
        , shows: Just "the engine's own value"
        , tone: toneOf c.light
        }
      Just d, Nothing ->
        { index
        , name: dutyLabel d
        , press: Just (dutyName d)
        , turn: Nothing
        , shows: case c.light of
            Lit f _ -> Just ("lit when " <> flagName f)
            _ -> Nothing
        , tone: toneOf c.light
        }
      Nothing, Nothing ->
        { index, name: "", press: Nothing, turn: Nothing, shows: Nothing, tone: Nothing }

toneOf :: Light -> Maybe String
toneOf = case _ of
  Steady t -> Just (toneName t)
  Lit _ t -> Just (toneName t)
  _ -> Nothing

-- | What each phase colours a loop encoder, as a key for the card.
-- |
-- | Enumerated over `allPhases` rather than listed, so a seventh phase would
-- | appear here rather than being quietly missing from the only place anyone
-- | would look for it.
phaseKey :: Array { phase :: String, tone :: String }
phaseKey = Array.mapMaybe entry allPhases
  where
  entry ph =
    let st = probe ph
    in map (\t -> { phase: phaseName ph, tone: toneName t }) (phaseTone st)
  -- A loop with a layer in it, so `Idle` reads as "stopped with material"
  -- rather than as empty — the two are different colours and the card has to
  -- say which is which.
  probe ph = stub { state = phaseName ph, layers = 1 }

-- | The minimum a `LoopState` can be, for asking `phaseTone` a question. Not
-- | exported: it is a probe, not a fixture.
stub :: LoopState
stub =
  { index: 0, state: "idle", layers: 0, loopFrames: 0, loopSecs: 0.0
  , pos: 0, phase: 0.0, armed: false, recording: false, quant: false
  , muted: false, reverse: false, pan: 64, speed: 1.0, pendulum: false
  , oneShot: false, levelArm: false, firing: false
  , chance: 1.0, skipping: false, fadeMs: 0.0, decayDb: 0.0, volDb: 0.0
  , cycles: 0, revox: false, fbDb: -3.0, toneHz: 6500.0, recEnv: []
  , pendingAt: -1, shapes: []
  }
