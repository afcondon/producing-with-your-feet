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
  , Led
  , leds
  , pager
  , pageRing
  , pageAt
  , pages'
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
import Data.Maybe (Maybe(..), maybe)
import Data.Number (log, pow) as Number
import Data.Tuple (Tuple(..))
import Data.Twister (Knob, encodersPerBank)
import Foreign.LooperSocket (LoopPhase(..), LoopState, allPhases, phaseName, phaseOf)

-- | A parameter an encoder can hold.
-- |
-- | Five, and they are exactly the five the daemon takes a number for. There is
-- | deliberately no `PLevel`: the engine has no per-loop volume, and a knob that
-- | moved a value nothing reads would be the worst kind of control — one that
-- | looks like it worked.
data Param
  = PTape
  | PLayers
  | PLevel
  | PRate
  | PPlace
  | PFade
  | PDecay
  | PChance

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
-- | Two knobs have a middle worth finding: pan, whose centre is centre, and
-- | speed, whose centre is unity.
homePosition :: Param -> Maybe Int
homePosition = case _ of
  PPlace -> Just 64
  PRate -> Just 64
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
  PRate -> "×0.125 to ×4, and unity sticks at the middle"
  PPlace -> "hard left to hard right, and centre sticks"
  PFade -> "0 to " <> show (round fadeTop) <> " ms"
  PDecay -> "hold at the top, down to −" <> show (round decayLaw.floorDb) <> " dB a pass"
  PChance -> "never to always"

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
data Flag = FReverse | FPendulum | FOneShot | FLevelArm | FGrid | FRevox

derive instance Eq Flag

flagName :: Flag -> String
flagName = case _ of
  FReverse -> "reversed"
  FPendulum -> "pendulum"
  FOneShot -> "one-shot"
  FLevelArm -> "listening"
  FGrid -> "on the grid"
  FRevox -> "a tape"

-- | One encoder: what it is about, what a press means, what a turn means, and
-- | what it shows.
type Control =
  { subject :: Subject
  , press :: Maybe Duty
  , turn :: Maybe Param
  , ring :: RingSource
  , light :: Light
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
  , ring: NoRing, light: Dark, pager: false
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
-- | **Bank 1 is the loops; bank 2 is the loop in hand.** Two pages where the
-- | MC6 family needs seven, and the reason is not switch count: four of those
-- | seven — Config, Quantise, Speed and Pan — exist to choose one number from a
-- | short list, which is a knob.
-- |
-- | Banks 3 and 4 are deliberately empty. The obvious tenant is the per-layer
-- | surface, every CC of which is still unimplemented in the engine, and a page
-- | with room in it is better than one that has to be redesigned to admit the
-- | next thing.
controlAt :: Knob -> Control
controlAt k = case k.bank of
  0 -> loopsBank k.index
  1 -> thisLoopBank k.index
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
      -- **Where Arm was.** `ArmLoop` is `lev1` then `r` — the mode plus the
      -- gesture — and the mode itself sits on page 2 as Listen, one press away.
      -- A shortcut to something already on the surface is the first thing to
      -- give up a cell when one is wanted.
      --
      -- Named and refused rather than silently absent: the vocabulary has
      -- `NotYet` for exactly this, so a press answers with what it is waiting
      -- for instead of doing nothing.
      -- **Revox: the loop becomes a tape.** Beside the ordinary overdub because
      -- that is the choice it is — the same gesture by a different mechanism —
      -- and lit while it is on, because a mode that changes what undo means had
      -- better be visible from across the room.
      -- **Press is the mode; turn threads the tape.**
      --
      -- They belong on one control because they are one idea: a tape is a loop
      -- of a chosen length that you play onto, and choosing the length is how
      -- you start. Everywhere else in this app a loop gets its length by being
      -- recorded, which is exactly what Revox does not do.
      --
      -- Turning is refused by the daemon once the loop has anything in it, so
      -- this cannot resize a take by accident — and the ring still reads the
      -- loop's real length, which is worth seeing either way.
      11 ->
        blank
          { press = Just RevoxToggle
          , turn = Just PTape
          , ring = Value PTape
          , light = Lit FRevox Violet
          }
      -- Undo and Redo were two cells doing one job. The stack is an axis, and
      -- this device reports absolute positions, so it is a knob: turn down to
      -- undo, up to redo, ring shows how deep you are. Press still undoes one,
      -- for when it is a gesture rather than a scrub.
      12 -> knob PLayers Undo Blue
      13 -> verb ClearLoop Violet
      -- The one thing a pedal cannot do, and the one thing a *hand* is worst
      -- placed to remember to do — so it gets a control on both surfaces.
      14 -> verb ClaimPast Yellow
      15 -> pager
      _ -> blank

-- | Sixteen controls for the loop in hand: four knobs, then twelve presses.
-- |
-- | The three `Step*` duties do not appear anywhere here. They are the MC6's
-- | rendering of the same parameters — a ladder is what a surface that can only
-- | press does with a number — and `perform` defines them in terms of the
-- | values these knobs send, so the two cannot come to disagree.
-- |
-- | **No start or end trim yet.** They belong on this page and the daemon has
-- | no verb for either; a knob that moved nothing would be exactly the failure
-- | this surface exists to avoid, so the row is spent on the second multiply
-- | instead until the engine grows them.
thisLoopBank :: Int -> Control
thisLoopBank = case _ of
  -- The six knobs, level first because it is the one you reach for.
  0 -> knob PLevel (Level 0.0) Green
  1 -> knob PPlace (Place 64) Blue
  2 -> knob PRate (Rate 1.0) Teal
  3 -> knob PDecay (Decay 0.0) Orange

  4 -> knob PChance (Chance 1.0) Yellow
  5 -> knob PFade (Fade 0.0) Green
  6 -> flagged GridToggle FGrid Teal
  -- `Free` is not here: `GridToggle` turns the grid off as well as on, and the
  -- third of the three erasures had nowhere else to live.
  7 -> verb ForgetLength Blue

  -- Spread, shift and dense sit together because they are used together: spread
  -- to make room, shift to decide where in it the bar falls, dense as the way
  -- back. **Presses rather than knobs**, and for a stated reason: the snapshot
  -- reports no per-loop spread, so a spread knob would be holding a position
  -- nothing could correct — the one thing this surface is not allowed to do.
  8 -> verb (SpreadLoop 2) Violet
  9 -> verb RotateLoop Violet
  10 -> verb DenseLoop Violet
  -- The Atlantis seam: a phrase becomes Tidal material seconds after it is
  -- played. It earns a place on the surface you have hands on.
  11 -> verb SaveTake Violet

  -- Pendulum is gone: it is a mode you set once a session if at all, and the
  -- pager had to live in the same corner on every page. It keeps its switch on
  -- the MC6's config bank and its control on this page's own web card.
  12 -> flagged Reverse FReverse Red
  13 -> flagged OneShot FOneShot Yellow
  -- Press toggles level-arm on this loop. **No turn**: the threshold it listens
  -- against is rig-wide, not this loop's, and a knob here would be eight knobs
  -- quietly writing one value. It is a once-a-session calibration, so it lives
  -- on the page beside the residual latency, where settings of that kind go.
  14 -> flagged LevelArm FLevelArm Green
  15 -> pager

  _ -> blank

verb :: Duty -> Tone -> Control
verb d tone = blank { press = Just d, light = Steady tone }

flagged :: Duty -> Flag -> Tone -> Control
flagged d f tone = blank { press = Just d, light = Lit f tone }

knob :: Param -> Duty -> Tone -> Control
knob p home tone =
  blank { press = Just home, turn = Just p, ring = Value p, light = Steady tone }

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
  PRate -> Rate (round3 (rateOf (detented p v)))
  PPlace -> Place (detented p (clamp 0 127 v))
  PFade -> Fade (toNumber (round (toNumber (clamp 0 127 v) / 127.0 * fadeTop)))
  -- **Full at the top and counting down**, the same way round as the level.
  -- It used to be the other way — zero at the bottom, more decay as you turned
  -- up — which put the knob's rest position at the far left and meant the first
  -- thing a turn did was take material away. A control whose home is at one end
  -- and whose effect is destruction is one you turn by accident.
  PDecay -> Decay (round1 (decibelsAt decayLaw (clamp 0 127 v)))
  PChance -> Chance (round3 (toNumber (clamp 0 127 v) / 127.0))

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
  PRate -> rateRing st.speed
  PPlace -> st.pan
  PFade -> round (st.fadeMs / fadeTop * 127.0)
  PDecay -> positionAt decayLaw st.decayDb
  PChance -> round (st.chance * 127.0)

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

-- | The level fader: the top half spends itself on the first 12 dB, which is
-- | where mixing happens; the bottom half covers the remaining 48 to silence,
-- | which is where fading out happens.
levelLaw :: Law
levelLaw = { knee: 64, kneeDb: 12.0, floorDb: decayFloor }

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
rateOf :: Int -> Number
rateOf v
  | v <= 64 = Number.pow 2.0 (negate 3.0 * toNumber (64 - v) / 64.0)
  | otherwise = Number.pow 2.0 (2.0 * toNumber (v - 64) / 63.0)

rateRing :: Number -> Int
rateRing s
  | s <= 0.0 = 0
  | s <= 1.0 = 64 - round (64.0 * negate (log2 s) / 3.0)
  | otherwise = 64 + round (63.0 * log2 s / 2.0)

log2 :: Number -> Number
log2 x = Number.log x / Number.log 2.0

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
type Led = { index :: Int, ring :: Int, hue :: Int }

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
    -- The pager's ring is the only one the engine has no opinion about, so it
    -- is filled here — this is the function that knows which page it is
    -- drawing.
    , ring: case c.ring of
        PageRing -> pageRing bank
        _ -> maybe 0 (ringOf c) loop
    , hue: maybe (dark c) (hueOf c) loop
    }
  where
  dark c = case c.light of
    Steady t -> hue t
    _ -> 0

ringOf :: Control -> LoopState -> Int
ringOf c st = case c.ring of
  NoRing -> 0
  PageRing -> 0
  Value p -> toKnob p st

-- | Where the pager's ring stands, and the position a turn is read against.
-- |
-- | Coarse on purpose: two pages across 128 steps is 64 steps a page, so the
-- | press-nudge cannot page you by accident. A third page makes it 42 and still
-- | cannot.
pageRing :: Int -> Int
pageRing p = clamp 0 127 (round (toNumber p / toNumber (max 1 (pages' - 1)) * 127.0))

pageAt :: Int -> Int
pageAt v = clamp 0 (pages' - 1)
  (round (toNumber (clamp 0 127 v) / 127.0 * toNumber (pages' - 1)))

-- | How many pages the looper surface uses. Two today; the trim-and-shift page
-- | that `DESIGN-TWISTER` wants next simply raises this, and every pager on
-- | every page rescales itself.
pages' :: Int
pages' = 2

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
  Playing -> Just (if st.muted then Blue else Green)
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
    , note: "The eight loops, then eight verbs for whichever is in hand. Every press is at press-down: nothing here waits out a window, because nothing here needs a gesture."
    , cells: cellsOf 0
    }
  , { bank: 1
    , name: "This loop"
    , note: "Five knobs and eleven presses, all about the focused loop. The rings are the engine's own values, so whoever moved one — a footswitch, the console, another client — the knob agrees within a frame."
    , cells: cellsOf 1
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
    , turn: Just ("which page — " <> show pages' <> " of them")
    , shows: Just "which page you are on"
    , tone: toneOf c.light
    }
  else case c.press, c.turn of
    -- A loop: the one control that is a place and a value at once.
    Just (SelectLoop i), _ ->
      { index
      , name: "Loop " <> show (i + 1)
      , press: Just "take it in hand"
      , turn: Just ("its level — " <> paramRange PLevel)
      , shows: Just "colour is what it is doing; the ring is how loud it is"
      , tone: Nothing
      }
    -- A knob: the parameter names the control, and the press is its way home.
    mp, Just p ->
      { index
      , name: paramLabel p
      , press: map (\d -> "back to " <> dutyName d) mp
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
  , revox: false, fbDb: -3.0, recEnv: []
  , pendingAt: -1, shapes: []
  }
