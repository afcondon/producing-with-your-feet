# The Twister — a second pair of hands on the looper

*2026-08-25. Companion to `DESIGN-LOOPER.md` (what Itajara is) and
`DESIGN-BANKS.md` (how the MC6 is laid out). Where those two disagree with this
one about the control surface, this one is later.*

## Why this document exists

The Midifighter Twister has been wired into this app since long before Itajara
existed. Twelve of the thirteen pedals have a `twister` mapping;
`Pedals.Itajara` is the one with `twister: Nothing`. Filling that field in would
light the Twister up on the looper this afternoon, and it is the wrong thing to
do — for a reason that is invisible from the field itself.

The Twister's existing route is `parseTwisterMsg` → `Engine.Twister` →
`SetValue` → `Data.Looper.command`. That last table is a **second meaning table**
for the looper, running in parallel with the one the footswitches use, and it is
the one that does not address the right loop. Adding the Twister to it would
have installed a third surface on the wrong half of a split the app already has
and has not noticed.

So this document is mostly about the split, and only then about the Twister.

## 1. The load-bearing decision: the page is the instrument

> **The web page is the reference surface. The MC6 and the Twister are both
> optional additional controllers to the commands visible on it.**

Andrew, 2026-08-25. Everything below is downstream.

Three consequences worth stating separately, because they are easy to nod at and
then design against:

- **Nothing may be reachable only from hardware.** If a duty exists, the page
  has it. The MC6 shows what fits underfoot; the Twister shows what fits on a
  4×4; neither can mean anything the page cannot.
- **Every hardware courtesy must tolerate absence.** `Machine.ShowBank` already
  degrades to `"no MC6 output — cannot change bank"`; the same has to be true of
  every LED write, and of any future "put the Twister on bank 2" message.
- **The page is the surface most likely to be *wrong*, because it is the one
  nobody audited.** It is the only one of the three that does not currently go
  through the state machine. See §3.

This supersedes the framing in `DESIGN-LOOPER.md` §12–13, where the control
surface is described from the pedal outward and the foot is treated as the
primary path. The foot is a client.

## 2. Vocabulary

Pinned, because three of these words were doing two jobs each.

| Word | Means |
|---|---|
| **Duty** | A named thing you can ask of the looper — `RecordLoop`, `StepChance`, `Enter ConfigBank`. `Data.Looper.Banks.Duty`. The vocabulary, and the only vocabulary. |
| **Surface** | A thing a human touches: the page, the MC6, the Twister. |
| **Decoder** | The per-surface function from a physical event to a `Duty`. `Banks.decodeSwitch` is the MC6's. |
| **Subject** | Which loop a duty is about — `Focused`, or `Loop i`. New; see §4. |
| **Verb** | What the daemon accepts, as text. `Data.Looper.Verb`. Downstream of everything here. |
| **Bank** | Overloaded and now split: an **MC6 bank** is one of 30 pages of six switches; a **Twister bank** is one of four pages of sixteen encoders. `BankSlot` names the MC6's; Twister banks are numbered. |

## 3. What was true, and what was wrong with it

*Fixed 2026-08-25 — see §13. Kept because the shape of the mistake is the
argument for §4, and because it explains why several things are named the way
they are.*

Two paths, two tables, one of them right:

| Surface | Route | Addresses | Stamps `@late` | Through the machine |
|---|---|---|---|---|
| MC6 (ch 9) | `decodeSwitch` → `Machine.act` → `Duty` | `Verb.at focus` | yes | **yes** |
| Page buttons | `SetValue` → `Data.Looper.command` → `Verb` | **bare** | no | no |
| Any CC on ch 13 | as above | **bare** | no | no |
| Twister | — (`twister: Nothing`) | — | — | — |

`Data.Looper.command` renders bare, so every command from the page reaches
**the daemon's own selection** rather than `st.looperFocus`. This is not a new
discovery; it is written down in `Data.Looper.Verb` on `SaveTake`:

> *Send this with `at`, never bare. Unprefixed it saves the daemon's selected
> loop, and nothing on the six-loop surface writes that field — so it silently
> wrote loop 1 whatever the board was focused on, and said it had succeeded.
> True of every per-loop verb here.*

It was fixed for the one verb it was found on. It is true of the whole table,
and the table is what the *reference surface* uses. That is the actual state of
play: the optional controller is correctly routed and the authoritative one is
not.

**The fix is not to add `at` to `Data.Looper.command`.** That would leave two
tables that agree by coincidence — which is exactly the arrangement
`Data.Looper.Verb` was created to end, and for the same reason:

> *Both wrote the words out as string literals, and neither knew what the other
> knew.*

## 4. The architecture: one vocabulary, three decoders

`Machine.act` already does the right thing and does it one layer too deep. It
resolves `(slot, switch, gesture)` to a `Duty` and then computes meaning from
the duty alone — *"keyed by what the switch is for, never by which switch it
is"*. The duty-keyed core is `onDuty`, and today it is unreachable except
through an MC6 bank and a switch number.

So:

```
  page click ──┐
  MC6 switch ──┼──► Duty (+ Subject) ──► perform ──► [Action] ──► socket
  Twister ─────┘
                    Data.Looper.Machine
```

- **`perform :: Rig -> Subject -> Duty -> Array Action`** is the entry point,
  and it is the *only* one. `Machine.act` becomes `Banks.decodeSwitch` plus a
  call to it.
- **`Subject = Focused | Loop Int`.** The MC6 always passes `Focused` — it has
  six switches and cannot name eight loops in a parameter gesture. The Twister
  passes `Loop i` for its per-loop encoders, because on a 4×4 every loop has its
  own knob and turning it must not steal focus. The page passes whichever its
  control means.

  Making the subject an explicit argument is what kills the bare-vs-`at` bug as
  a *class*: there is no longer a way to send a per-loop verb without having
  said which loop, because the compiler asks.

- **`Data.Looper.command` becomes CC → `Duty`**, not CC → `Verb`. It does not
  disappear: the MC6 assignment UI, board presets and the pedal face all index
  Itajara by CC, and `DESIGN-LOOPER.md` §2 is still right that a virtual pedal
  addressed by CC is what buys all of that for free. It stops being a *meaning*
  table and becomes an *addressing* one.

### 4.1 Value duties and step duties are the same duty

The Twister's turn has to become a `Duty` like everything else, and the
vocabulary already half-supports it: `Rate Number` and `Place Int` exist, put
there by the Speed and Pan banks. `StepChance`, `StepFade` and `StepDecay` do
not carry a value — they mean "move one rung from wherever the engine says you
are".

Rather than a second family, **a step duty is defined in terms of a value
duty**:

> `StepChance` reads the loop's current chance from `Rig`, finds the next rung of
> `chanceLadder`, and delegates to `Chance n`. The ladder is a *rendering* of the
> parameter for a surface that can only press; the value is the parameter.

That gives one place where chance becomes a command, and it means a footswitch
and a knob provably cannot disagree — the same argument `Data.Looper.Verb` makes
about spellings, one level up. It also means the ladders keep the property that
made them acceptable in the first place: the position is computed from what the
daemon reported, never counted on a device.

New value duties needed to complete the set: `Chance Number`, `Fade Number`,
`Decay Number`. `Rate` and `Place` already exist.

## 5. Eight loops

**Itajara grows from six loops to eight**, to match the Twister's 4×4.

Six was the MC6's number: `loopSwitches = 6` is documented as *"what the MC6 has
underfoot without an FS3X, and a loop you can only reach through an accessory is
a loop you will not use"*, and `N_LOOPS` was then set to match it. With the page
as the reference surface that reasoning inverts — the loop count should come
from the instrument, and the foot reaches what it can.

- **Loops 7 and 8 are page-and-Twister only.** Not a deficiency: they are the
  ones you *set up* rather than stomp — long beds, drones, the thing you started
  ten minutes ago and want to keep pulling in and out.
- **The wire is unaffected.** `dispatch` picks the loop from a single leading
  digit, so 0–7 still fits and `Verb.at` does not change.
- **The arena is `N_LOOPS * MAX_LAYERS * max_frames`, allocated once at
  startup.** The engine header says 46 MB at defaults; eight loops is about
  61 MB. Not a consideration.
- **`loopSwitches` must stop claiming to equal `N_LOOPS`.** Its comment says
  *"Must equal `N_LOOPS` in the daemon"*, and that invariant is now deliberately
  broken. Introduce `nLoops = 8` as the model's count; `loopSwitches` keeps the
  value 6 and becomes *"how many of the eight the MC6 can reach"*. Left as is,
  the next person restores the equality and the app grows two ghost loops.

Everything that iterates loops — `Component.Looper.Slots`, the board simulation,
`busy_elsewhere`, the anchor-loop grid — scales without argument. The one thing
to look at is whether eight rows still read at a glance on the Looper page.

## 6. What each surface is actually good at

The three are not ranked; they have different shapes, and the layout follows
from the shapes.

**The page** — unlimited controls, arbitrary labels, real display, no latency
worth measuring, and both hands. It is the superset by definition and the only
place a rarely-used duty should have to live.

**The MC6** — the only one operable with both hands on an instrument. Six
switches per bank, thirty banks, and three gestures per switch. Its costs are
specific:

- a duty behind a `Double` or `Hold` waits out the recognition window before it
  is sent at all, which is what `deferralOf` and `@late` exist to undo;
- a label change is an editor-session upload, the better part of a second;
- it is the only surface a player cannot look at while playing.

**The Twister** — sixteen encoders in four banks, each encoder both a **press
and a turn**, each with an RGB and a 0–127 ring. On a table, so it is the
controller for the sessions where the hands are free: looping the modular rather
than the guitar. Its properties, in the order that matters:

1. **Every duty can have its own control**, so nothing needs a gesture, so
   nothing waits out a window. **Presses are at press-down and `late` is 0.**
   For the modular case that is an accuracy win, not a convenience one.
2. **Press and turn are the same physical control.** A loop's encoder is both
   its selector and its mixer. The MC6 cannot express this at all.
3. **It can be relabelled instantly, in colour, having no text.** One CC changes
   an encoder's hue. So its layout can be *dynamic* where the MC6's must be
   static — eight loop encoders can carry phase as colour, live, at snapshot
   rate.
4. **It holds a value without owning it.** This is the property that matters and
   the one easiest to state wrongly (see §7). The app writes the ring position
   from the daemon's snapshot, so the knob can be *told it is wrong*.

## 7. The correction about ladders, and the one about toggles

An earlier version of this argument said four of the seven `BankSlot`s exist
because a footswitch cannot hold a number. That is not true, and the two ways it
is untrue are both worth having written down, because both are underused
capability rather than absent capability.

**The MC6 can count.** Its scroll-counter functions would do the chance, fade
and decay ladders on the device. `Data.Looper.Banks` already says why we do not
use them:

> *the device would keep its own position, and a device that keeps state is the
> one thing here that cannot be told it is wrong.*

So Quantise, Speed and Pan as whole banks are a **directness-vs-state-ownership**
trade, not a capability limit — and with 30 banks the space is not scarce. What
the Twister changes is narrower than "it can hold a value": it is that an
encoder holds a value *the app assigns it*.

**The MC6 can relabel a toggle switch for free, and we are not using it.** Two
findings:

- `MC6Preset` carries a **`toggleName`** as well as a `shortName`, both eight
  characters, so a toggle switch shows a different label in each position with
  no upload. A `Record` switch can genuinely read `Record` / `Stop`. But
  `SysEx.purs:156` writes `toggleNameTLV shortName` — the writer hands the same
  string to both fields, so today the second label is unreachable.
  `ControlBankSwitch` needs the field and the writer needs to stop duplicating.
- `MC6MsgType` includes **`MsgSetToggle`** (type 11). If that is reachable from
  outside the device, a toggle's position is *correctable* — at editor-session
  speed rather than press speed, but correctable, which is more than the scroll
  counter offers. Unverified; see §12.

The looper family currently uses `toToggle: false` on every switch. `RecordLoop`
and `Transport` are the obvious candidates; `Reverse`, `Pendulum`, `OneShot`,
`LevelArm` and `ClickToggle` are all flags being spent as momentaries. This is
MC6 work that this document has no opinion about beyond noting that it is
available and that **a toggle is device state**, and therefore subject to the
rule in §11.

## 8. The Twister's shape, as constraints

- **4 banks × 16 encoders.** The stock CC map is a clean grid: turns on channel
  1, presses on channel 2, and the bank shifts the CC by 16 — so bank *b*,
  encoder *i* is CC `16b + i`.
- **`parseTwisterMsg` is bank-blind.** It treats the CC as the encoder index, so
  banks 2–4 (CC 16–63) index past the end of a 16-slot array and vanish without
  a word. `bank = cc / 16`, `index = cc % 16`. Four lines, and until they exist
  three quarters of the controller is silently inert.
- **The app owns the page; the device is asked to follow.** This section said
  the opposite for most of a day, and the argument for it was good and wrong:
  the device switches banks by itself and every message carries the bank, so
  the app could *read* the page instead of remembering it, and a piece of state
  in two places became no state at all.

  What that reasoning left out is that reading a page is not the same as
  reaching one. It put the only way between pages on a side button this repo
  cannot program, whose behaviour nobody here has verified — and the
  consequence arrived within an hour of the surface being used: **stuck on page
  2 with no way back**.

  So there are two facts and they are kept apart. `twisterPage` is the page the
  app is showing and it decides what every encoder means. `twisterHeardBank` is
  where the *device* says it is, still read off the wire. They are normally the
  same number; they come apart when the device will not take a bank change, and
  then **the content comes from the app's page and the address from the
  device's block**, so the lights land where somebody is looking.

  The device still wins when it *moves*: a change in the heard bank is the
  device navigating and is adopted. A heard bank that merely stays put is not,
  which is what lets the app page a device parked on bank 1 for ever.
- **While the looper has the controller, the ch 5 side buttons are its pager.**
  They are the only physical buttons on the device that certainly reach the app
  — the encoders' presses are spoken for, and the device's own bank switches
  are neither ours to program nor verified. Walking to another pedal is what
  the pill row is for; walking between two pages of the loops is the thing you
  do with your hands full. `RefreshLEDs` keeps its button.
- **`bankSelectMessage` is still a candidate**, and now it is a courtesy rather
  than a dependency: `[0xB3, bank, 127]` goes out on every page turn, and if
  the device honours it the two stay in step and the LEDs land on its own
  block. If it does nothing, paging still works.
- **You cannot press an encoder without turning it.** Measured by using it:
  the knob rotates a little on the way down, so a press arrives with a nudge
  beside it and which the app sees first is not decidable. On page 1 that is not
  cosmetic — the press selects a loop and the turn sets a value on it, so taking
  a loop in hand would quietly move whatever that value is.

  Two consequences. **What sits under the press had to change**: it was
  `chance`, and a nudged chance is inaudible until the pass it eats, where a
  nudged *level* you hear at once and correct without thinking. That is what
  forced the level verb (§10). And **the app now withholds a turn** until it is
  clear it was not part of a press — 60 ms — while a press deafens its own
  encoder until the finger comes off it and for 300 ms after that, because the
  nudge can land on either
  side.

  The shape of that is the MC6 withholding a single press until it knows it was
  not a double, and it is worth seeing that it is the same thing: the app is
  doing for the Twister what the pedal does for itself. What differs is the
  price. The MC6's wait is spent on a *transport* gesture, so the lateness has
  to travel with the command and the daemon reaches back into the pre-roll for
  it. Nothing withheld here is sample-critical — a level is not a downbeat — so
  the latency is simply absorbed and no `@ms` travels with it.
- **Focus is what decides which surface the controller shows**, and opening the
  Looper page sets it to Itajara. The side buttons still walk away to a pedal,
  deliberately: the controller belongs to whatever you are looking at, and that
  has to include looking at something else.
- **LED writes** are `0xB0 index ring` and `0xB1 index hue` — already
  implemented as `sendRingPosition` and `sendRGBColor`. The Twister also has
  animation channels for pulsing; unverified which, and a recording loop that
  pulses is worth the experiment.
- **Configuration lives in the device, not in this repo.** The MC6 is compiled
  from `Data.Looper.Banks` and uploaded; the Twister is configured by the
  Midifighter Utility from a `.mfs` file we do not hold. See §11 for the rule
  that follows.

## 9. The layout

Two banks, where the MC6 family uses seven pages.

```
BANK 1 — Loops                        BANK 2 — This loop
 L1    L2    L3    L4                  level  pan    speed  decay
 L5    L6    L7    L8                  chance fade   Grid   Length
 Rec   Ovr   Stop  REVOX               Spread Shift  Dense  Save
 Layrs Clear Captr ▸page               Rev    1Shot  Listen ▸page
```

The card at the foot of the Looper page prints this from
`Data.Looper.Twister.pages`, so it is generated rather than transcribed:
`Component.Looper.TwisterMap` knows how to draw a 4×4 of things that describe
themselves and nothing about what any of them does. The test suite prints the
same layout, which is how the `Grid` cell was caught — it read **"1 Bar"**, both
misleading (the engine's grid is the anchor loop's cycle, not a bar) and broken
(`Grid n` always sets *on*, so the encoder could not be pressed twice). `Duty`
gained `OnGrid Boolean` and `GridToggle`; `Grid n` and `Free` are now the MC6's
renderings of them, and `Free`'s cell went to the third erasure, Forget Length.

**Bank 1 — Loops.** The top two rows are the eight loops, one encoder each:

- **press** = `SelectLoop i` — focus, and open its page on the MC6 if one is
  listening;
- **turn** = that loop's **level**, subject `OnLoop i`, *without* taking focus
  — the knob you reach for while something else is recording. It was chance
  until the device said otherwise; see §8;
- **ring** = the playhead. `phase` is already in every snapshot at 30 Hz and the
  ring is 0–127 around a circle. Eight turning rings is the display this project
  keeps saying it wants — resolved musical meaning rather than control
  positions;
- **colour** = `LoopPhase`. Recording, overdubbing, multiplying, playing, armed,
  idle — six states and six hues, which is the whole reason `LoopPhase` was
  closed as a type.

The bottom two rows are the verbs, subject `Focused`. Eight duties with no
gesture and no bank change, where the MC6 needs `LoopPage` plus a hold.

**Bank 2 — This loop.** Sixteen controls for the focused loop: turn sets the
value, press resets it to unity or flips the mode, colour shows on/off. The
three `Step*` duties do not appear — they are the MC6's rendering of these same
parameters (§4.1).

**No start or end trim, though the first sketch had them.** The daemon has no
verb for either (§10), and a knob that moved nothing would be exactly the
failure this surface exists to prevent — worse than a gap, because it looks
like it worked. The row went to the second multiply instead: Spread to make
room, Shift to decide where in it the bar falls, Dense as the way back. Those
are presses rather than a knob for a related reason — the snapshot reports no
per-loop spread, so a spread knob would be holding a position nothing could
correct, which is the one thing §11 rule 5 forbids.

Six knobs, and six is the number of parameters the daemon takes a number for:
`vol`, `sp`, `pan`, `xf`, `dec`, `ch`. `ClearAll` came off this page to make
room — an all-loops erase on a surface you are reaching across is not a button
that wants to be near your hand, and it is still on the MC6 and the page.

**Banks 3 and 4 are reserved and deliberately empty.** The obvious tenant is the
per-layer surface — CCs 40–68 in `Data.Looper.command`, every one of them
`NotYetImplemented` today. A bank with room in it is better than one that has to
be redesigned to admit the next thing.

## 9.1 What the surface settled into

Three cells changed hands after the first sessions, and each for a reason
worth keeping.

**Undo and Redo became one knob.** They are one axis, and this device reports
absolute positions, so the stack is a *place*: turn down to undo, up to redo,
ring shows how deep. It sends the difference between the knob and what the
daemon reports, so a layer removed by a footswitch moves the knob rather than
confusing it. Eight layers across 128 steps is sixteen a layer, so the
press-nudge is harmless by arithmetic. Press still undoes one.

**Arm and Pendulum came off.** `ArmLoop` is `lev1` then `r` — the mode plus the
gesture — and the mode is one press away on page 2 as Listen; a shortcut to
something already on the surface is the first cell to give up. Pendulum is a
once-a-session mode and keeps its MC6 config switch.

**The pager is an encoder, not a toggle**, and it is the same corner on every
page. Turn picks the page, press goes home to Loops. It read absolute position
at first, which meant sweeping half the knob to reach the next of two pages —
and a third page would have made each *band* narrower rather than the *gesture*
smaller, wrong both ways round. It steps now: the ring is pinned to a reference
position, so any deviation is a fresh turn and its sign is the direction.

**It is a position with ends, and the device owns it** (2026-08-27, after two
designs that both failed on the hardware). The pager reads where it stands, in
fixed 32-unit bands — a quarter turn a page — and the app writes to that
encoder *only* when the app moved the page by itself.

The two dead designs are worth keeping, because both looked right:

- *Position over the whole travel.* Two pages meant sweeping half the encoder,
  and a third page made each **band** narrower rather than the **gesture**
  smaller. Wrong both ways round.
- *A step from a parked position.* Parked at the page's own end there was no
  travel left to turn into, so **forward-wrap was unreachable on hardware** —
  the ring sat at 127 and the device clamps there. The test asserting it passed
  by feeding `pageTurn` a value of 130, which nothing can send. Parked in the
  middle instead, the ring had to be rewritten after every change, so one
  direction cost a full notch and the other cost a single unit.

What both had in common was the app deciding where "here" was, and the knob
arguing with it. It doesn't any more:

| | |
|---|---|
| band width | `pageStep = 32` — a right angle, fixed, not travel ÷ pages |
| which page | `pageFor v = clamp 0 (pages'-1) (v / pageStep)` |
| past the end | nothing. **Clamped, not wrapped** — a knob with a physical stop should stop, and it makes the gesture reversible: turn back exactly as far, get back exactly as many pages |
| the app moves the page | `showTwisterPage` — writes `pagerRing p = p * pageStep` to the encoder |
| the knob moves the page | `adoptTwisterPage` — writes nothing back |
| on taking focus | `dimAllLEDs` zeroes every ring, then `showTwisterPage 0`. Zero is page one, so the app, the lights and the knob agree without anyone being told |

A third page costs another 32 units of travel rather than making all three bands
narrower; three use 96 of 127 and there is room for a fourth.

The page is also the pager's **colour** — teal, violet, and yellow named for the
page not yet built. Two indicators for the one fact you must not be wrong about,
and they cannot disagree: both are computed from the page being drawn.

One property worth stating because the previous designs lacked it: this **does
not depend on writing to the encoder working at all**. If the device ignores a
ring write, paging by hand still reads correctly — only the card's "turn to this
page" button would stop carrying the knob with it.

**And then a third design failed, and the reason retired the whole premise**
(2026-08-27, same day). "The device owns the position" was one assumption short:
the device owns *four* positions. **A Twister keeps a separate value for every
encoder on every block** — the `16b + i` arithmetic `parseTwisterMsg` decodes on
the way in, read in the other direction — and the app was changing block on
every page turn, via `bankSelectMessage`. So paging swapped which of four stores
the corner encoder read from, and the new one held whatever it had last been
left at. Those leftovers survive a reload, which is why the same test gave a
different answer each time and why it felt like persistent state somewhere. It
was: in the device.

Symptom: a quarter turn reached page 2, the pager's ring dropped to zero, and a
click or two later it was back on page 1 and thereafter unrepeatable. It
presented as a threshold problem and survived every fix aimed at one, because
none of them touched *which store was being read*.

Mirroring the value into the other three blocks is the obvious repair and it was
tried first. It is wrong: it keeps four copies of one fact in step by hand, and
it has to keep doing so while a hand is turning one of them.

**So the device is pinned to one block and the app owns the paging outright**
(`Lights.deviceBank = 0`, `pinDevice`). A page turn now sends no block change at
all — only a repaint of the sixteen encoders the device is already showing,
which costs exactly what the old block change cost, since `twisterLit` was
cleared and everything repainted either way.

This is not a new position so much as the one the rest of the code had already
taken. `onPage` throws the device's block away before deciding what an encoder
means; `sendAllLEDs` has only ever written to block 0. The looper was the single
surface that disagreed — and that disagreement had a second bug in it nobody had
noticed: walking from the looper to a *pedal* while the device sat on block 2
lit block 0, which is to say lit nothing you could see.

What is given up is the device's own block buttons, which are now put back
rather than followed (`TwisterMidiReceived`). That is a gain. They changed the
meaning of all sixteen encoders with nothing on screen to say so.

Three notes for next time:

- **The pure tests could not have caught any of this and still cannot.**
  `pageFor`, `pagerRing` and `pageStep` were right throughout and every
  assertion about them passed, twice over. All three bugs lived one layer down,
  in which CC the write went out on — `Component.Twister.Lights`, effectful, on
  the wire.
- Each of the three dead designs was a true statement about the hardware with a
  missing qualifier: the device owns the position (*which* position?), the
  device owns the block (*so what does an encoder mean?*). The fix each time was
  to take ownership rather than to guess better.
- The remaining hardware unknown is now harmless but still unmeasured: whether a
  ring write to a non-active block lands at all. Nothing depends on the answer
  any more, because nothing writes to one.

**The card follows the knob** (2026-08-27). With the pager working, the panel
in the Looper page shows **one page at a time** — a tab strip, then that page's
grid — instead of printing both side by side. Turning the pager moves the
highlight, so the strip is a readout of the encoder as much as a control, which
is why the current tab is not a button.

Printing both was a workaround for a page turn you could not perform, and it
cost the panel its width: 1180px of card over the loops the card describes. It
is 640px now — four cells across and nothing else — and the reclaimed half is
loops you can still see while reading it. The phase key goes with page 1, where
it applies; carrying it onto page 2 was something the two-column layout did by
accident.

The card's warning line changed with it, and the reason is worth recording
because it was a bug the pin introduced. It compared the page being shown with
`twisterHeardBank` and warned when they differed — correct while a page *was* a
device block, and wrong the moment the device was pinned, since it would then
have fired on every visit to page 2. It compares the heard block against
`Data.Twister.deviceBank` now: not "which page", but "is the device still where
we put it". That is also why `deviceBank` lives in `Data.Twister` rather than
beside the MIDI writes — a view needs it to ask the second question.

That also handed the ch 5 side buttons back to prev/next *pedal*, which they
had to be: with the looper holding the controller there was otherwise no way to
reach a pedal at all.

## 9.2 Revox — the loop as a tape

Everywhere else here a pass is non-destructive: layers are kept whole and
`decay` is a resolution applied at playback, which is why turning decay off
brings a faded loop back. That is the right default and it is not what a tape
does. Two Revoxes with the second feeding back below unity erase as they
record, and there is no version of what was under the head that was not erased.

So it is a mode you opt into, and the price is stated rather than hidden.

- **Entering flattens the loop to one layer**, at the gains it is being heard
  at, so what you hear the instant before is what you hear the instant after. A
  tape has no layers, and writing over layer zero of several would erase one
  voice and leave the rest — a mode that only half applies. The fold is not
  reversible and the ack says so.
- **Undo goes away**, and the layer scrub refuses *by name* rather than going
  quiet: "loop 1 is a tape — undo went with the layers". The difference between
  a mode and a broken knob.
- **`fb` is its own verb, not `dec`.** Same musical idea by two mechanisms, one
  destroying and one not; a single number meaning "resolution here, erase head
  there" depending on a flag is the overload this codebase keeps refusing. `dec`
  still works in Revox and still does what it always did — and the two compound,
  which is deliberate.
- **`tone` rolls off the top on each pass.** Tape loses the high end before it
  loses the level, and losing only the level is what makes a feedback loop sound
  digital. One pole, on what is already on the tape as the head goes over it.
  **Revox only**, and that is design rather than shortcut: outside it a filter
  would have to be a different filter per layer per pass count, cascaded as deep
  as the loop is old.
- **A tape is threaded, not recorded.** `blank<secs>` gives a loop a length and
  nothing in it — the only route to a length that is not a recording. It is
  **one silent layer, not none**: playback sums `0..n_layers` and the recording
  layer sits *at* `n_layers`, so a loop with no layers is silent even while
  something is written into it. In Revox that matters, because the erasing write
  goes into layer zero, which is the layer that has to be playing for the tape
  to come round under your hands.
- **The Revox encoder carries both halves**: press for the mode, turn for the
  length. They are one idea — a tape is a loop of a chosen length that you play
  onto, and choosing the length is how you start.

`threaded` is the fact a layer count cannot carry: a threaded tape has one layer
and so looks exactly like a recorded loop, which made the length knob a one-shot
until the flag existed. Cleared the moment anything is recorded, which is the
moment resizing would stop being a choice of length and become a trim.

## 9.3 The recording is visible now

Everything the display knew about a loop was a *committed* layer, so you pressed
record, the slot went the colour of recording, and nothing else moved until you
closed the take. The one moment you most want a picture — is it hearing me, am I
loud enough, how far round am I — had nothing to say.

`recEnv` is atomics rather than the layer-envelope mutex, because it is written
from the audio callback and that one is not. A first take has no length to lay a
picture against, so it is drawn against the arena and rescales when the loop
closes; an overdub knows its cycle and fills in wherever the playhead is, on the
second pass as well as the first, which is why the buckets take a peak and not a
store. Empty whenever nothing is recording, so the display has one test rather
than a second copy of "what counts as recording".

**It is what makes Revox safe to use at all.** A destructive pass has no undo,
so watching it happen is the only feedback there is.

## 9.4 Four pages, cut by when you reach for them

Two pages became four on 2026-08-27, and the cut is the whole of it. The second
page was not a function, it was a drawer — *everything about the loop in hand* —
so a fader you ride, a crossfade you set between takes and a mode you choose
once a session all sat together because they were all about the same loop.
Sorting them by **when you reach for them** produced four pages, and it filled
the third page that nobody could think of a tenant for.

| | | |
|---|---|---|
| 1 | **Loops** | teal | what is sounding, and the whole write head |
| 2 | **The set** | violet | the eight against each other — where each sits, whether it runs |
| 3 | **Shape** | yellow | the loop in hand, while you play it |
| 4 | **Set up** | blue | the loop in hand, before and between takes |

**The set is the transpose of Shape** — one parameter across every loop, where
Shape is every parameter of one loop. Pan is the parameter because placing loops
is inherently comparative: you are listening to where the *others* are, which is
exactly what a page of one loop cannot help with. Its eight encoders are in the
same eight positions as the Loops page, so which knob is which loop is learned
once and only the verb underneath changes.

Four is also what the pager's travel holds at `pageStep = 32`, so the surface is
now spent rather than reserved. The two blocks that were being kept for the
per-layer surface are gone as a *reservation* — since the app owns paging
outright (`Data.Twister.deviceBank`) a page was never a block again anyway.

What the cut turned up, which is the useful part:

- **Arm was two page turns away.** It came off the Loops page on the argument
  that `ArmLoop` is the mode plus the gesture and the mode was already on the
  surface as Listen, "one press away". It was one press *and two page turns*,
  which made the most time-critical gesture in the rig the slowest thing on the
  controller. Arm is back in the write-head row, which now matches the MC6's own
  loop page switch for switch — and each of the four wears the colour of the
  phase it produces.
- **Multiply could not be asked for by any hand.** On the CC table, so reachable
  from a web button; on no MC6 bank and no encoder. A verb the vocabulary had
  and no surface could send. It has a cell on Shape.
- **The tape's parameters were sliders on a web page.** Revox is the one mode
  with no undo, and what a pass leaves of what was under it — the number that
  separates Frippertronics from a tape echo — was the one thing you had to look
  away to set. Revox, feedback and tone are the bottom row of Set up, together.
- **There was no way to stop everything by hand.** Stop all and Start all live
  on the MC6's global row and nowhere else, so with the looper holding this
  controller the panic button needed a foot. They are on The set.
- **Four violets in a row said nothing.** Spread, shift, dense and save take all
  wore one colour, and Clear shared violet with the Revox flag two cells up — a
  destructive verb and a mode, on a surface whose case for colour is that it is
  taken in rather than read.

## 9.4.1 The second cut, from reading the printout

The four pages were designed and then *printed*, and reading them side by side
on paper — before any of it was tested — found seven things the design could not
see from inside. That is worth recording as a method: a layout is a spatial
argument, and a spatial argument is much easier to check on a page than in a
table of case branches.

- **Clear and Clear All were in different cells.** Loops had Clear at 13 and The
  set had Clear All at 14, on two pages whose bottom rows a hand learns as one
  row. Both are at 12 now, the corner furthest from the pager — which is the
  knob the hand reaches for most and therefore the one it misses from.
- **Overdub was Record with a refusal bolted on.** `Machine.onOverdub` and
  `onRecord` send the same `r` in every case that reaches the wire; Overdub's
  only distinct behaviour is declining an *empty* loop with "record it first".
  On the MC6 that refusal earns its switch, because a foot cannot see what it is
  about to write to. On a surface where the loop's colour is under your hand, it
  is a second switch whose only difference is that it sometimes says no. The
  cell went to the grid.
- **The freed cell took the grid and the bar count together** — press for
  quantise, turn for how many bars. That was the point of freeing it: the first
  take needed Grid and bars from Set up and Record from Loops, so making a loop
  meant two page turns before the count-in. It is now one page. The pair belongs
  on one encoder for the same reason Revox and the tape do: a take that waits
  for the bar and a take that is a known number of bars are one idea.
- **`every` belongs beside `chance`.** They are the two ways of thinning a loop
  — one random, one periodic — and they were on different pages. Shape's middle
  row is now chance, every, slot, dense: how often it sounds, from the least
  deliberate to the most, and then the way back.
- **Pendulum was filed as a mode.** It sat on Set up because it is chosen
  deliberately; so is a speed, and nobody put that on Set up. What makes
  something a setting is that you would not touch it *mid-phrase*, and turning a
  loop round and back is the most mid-phrase thing in the rig. It is beside
  speed now.
- **A knob that absorbed another control has to say so.** The grid/bars cell
  was named `bars`, with the grid a line further down under `press` — where a
  scan does not reach. Andrew, on the printout: *"I was quite puzzled as to
  where grid had gone even though I had agreed to the combination."* The card
  now derives such a name from both halves, gated on whether the press is the
  knob's way *home*: that leaves exactly the two mode-carrying knobs,
  `bars/Grid` and `tape/Revox`, and a third would name itself.
- **`keeps` was a bad name and `on` was worse.** `keeps` is a low-pass corner
  and is called `lo-pass`. `on` is which slot of the `every` period a layer
  lands on, and is called `slot` — which is the daemon's own word in the ack
  ("layer 1 is on slot 3 of 4"), so the knob and the reply now agree.

What is left on Set up is seven controls: one-shot, listen, forget-length, and
the tape row. **A thin page is not a failed page** — it is the four-page cut
doing what it was for, which was to stop this one being a drawer.

## 9.4.2 The sheet

Reading the printout is what found §9.4.1, so printing got a button:
**Print sheet** opens a self-contained document in its own tab — the four boards
at a glance, a page of detail for each, then the recipes — and the browser's own
dialog makes paper or a PDF of it. `Data.Looper.Sheet` builds it as a pure
`String` from `pages` and `Recipes.recipes`; `Foreign.Sheet` writes it into a
blank window.

Three things about the shape of that, none of them incidental:

- **Its own tab, not a print stylesheet over the app.** A print view of the app
  is a print view you have to be *looking at*, and Chrome throttles a background
  tab until the looper stops handling Twister messages. The whole reason for
  paper is that the app keeps focus.
- **A pure `String`, so the suite reads it.** The tests check that every control
  name, every recipe and every step appears, that pages are numbered from one,
  and that each tone's ink is emitted from `swatch` — none of which would be
  reachable if the document were built in the DOM.
- **A blocked pop-up is reported.** `openSheet` returns whether a window
  actually opened, and a refusal lands in the log the player is already
  watching. Nothing throws when a pop-up is blocked, which is the exact shape of
  the silent failures this project keeps finding in its own ack path.

The colours are the one duplication: `swatch` holds the seven hexes for anything
that has to draw them itself, and `static/index.html` has the same seven for the
app's own swatches, because a document in another tab cannot reach that
stylesheet. The shared `Tone` guarantees a colour cannot go *missing* from
either — not that they agree on which one.

## 9.4.3 Two fields for one fact, again

The four-page cut was tested, and the second run of the first recipe failed
where the first run had worked. Andrew: *"clear must subtly reset something. It
doesn't stop recording at the bar limit on subsequent recordings."*

It did, and the engine was not the thing that was wrong. `Loop::cleared` zeroed
`loop_len` and left `cycles` alone, which was harmless for as long as a bar
count could only come from a recording — the two were made and destroyed in the
same moment. `len<n>` broke that by sizing an *empty* loop, so a cleared slot
said "no length" and "four bars" at once. Every call site inside the engine
checks the length first and bails, so nothing there misbehaved.

The damage was on the surface, and it needed the whole chain to be visible:

1. the Twister's bars ring is drawn from `cycles`, so a cleared loop read four;
2. the app **writes ring positions back to the device**, so the encoder
   physically sat at four bars on a loop that had none;
3. turning it "to four" therefore moved nothing, emitted no CC and sent no
   `len4`;
4. the next take recorded open-ended, and looked like a broken self-close.

That last step is why it presented as an engine fault. A probe against the live
daemon settled it in one run — two rounds of `0c`, `0g1`, `0len4`, `0r`, both
committing at 8.000 s exactly — which said the engine was fine and the command
had never arrived.

**The lesson is the same one `sized-but-empty` taught, read the other way
round.** There, `loop_len > 0` stopped meaning "has material". Here, `cycles > 0`
stopped meaning "has a length". Two fields describing one fact must be born and
die together, and the test that guards it asserts *agreement* rather than
behaviour: a cleared loop is indistinguishable from a fresh one on every field
that describes a length. `close_at` and `rec_len` were reset with them — both
describe a recording that is no longer going to happen.

The reason clear reaches all of this at all is worth stating: on this surface
the device holds no value of its own, so **anything the engine forgets, the
encoder is told to forget too.** That is the property that makes a nudge
harmless, and it is the same property that turned a stale field into a control
that could not be operated.

## 9.4.4 The click comes to you

*"Why wouldn't our looper change the Link speed? If I record a few bars as a
basis and I'm a bit off with my timing vs the click, and I trim the end, we can
just change the BPM."* — and the answer was that it should, and that most of the
plumbing was already there.

link-spike answers `/link/set-tempo` on UDP 57122 and propagates to every peer.
Itajara had no outbound OSC at all — it listens for anchors on 57125 and
otherwise only ever receives — so this is `link::set_tempo`, a UDP send, and one
exact-match arm in `dispatch`: **`<n>bpm`, take the session tempo from loop n.**

### Why this is not warping

**No audio moves.** `loop_len` is frames; loops play at frame rate and stay
phase-locked to each other whatever a bar is. What a bar length reaches is the
click, quantised launches and closes, `set_bars` arithmetic — and the rest of
the Link session. This is the principle the bar model already runs on, at rig
scale: *move the grid to the audio, never the audio to the grid.* It is the
opposite of the Ableton move, and it is what a floor looper has always done.

It also takes the **average** over the bars, not the timing within them. Play
four bars a little long and the click comes to you; play them unevenly and they
stay uneven, which is the point.

### The half that was missing

The first attempt needed three gestures — record free, declare the bar count,
take the tempo — and the middle one could not be made. `set_bars` has three
behaviours and *declaring* is reachable only with no clock; with one, it
**resizes**, which would have snapped the 8.129 s take to 8.000 and cut exactly
the overhang the whole exercise is about.

The real gap was upstream: **`commit` set a length and never a bar count.**
`cycles` is zero for every freely recorded loop, and zero reads as one
everywhere, so an eight-second take showed "1 bar" on the encoder. Committing
now rounds `len / bar` to the nearest, at least one, whenever there is a clock —
which is honest on its own terms, fixes the bars ring for free takes, and
reduces the whole recipe to **Arm, play, press Tempo**.

Only with a clock. Without one the first loop *is* the pulse and its whole
length is one cycle, which is what `loop_grid` depends on.

### What it cost to find the guard

The first live run put the whole rig on **29.56 bpm**. The take was four bars and
`cycles` was zero, so it was read as one bar — and 29.56 is inside Link's
20..999, so the range check passed and the number went out to Ableton and the
modular.

That is the failure this rig is built to avoid: not a wrong answer that looks
wrong, a wrong answer that looks ordinary. The range check cannot catch it, because
one quarter of a plausible tempo is also a plausible tempo. So `bpm` refuses
outright when `cycles` is zero — a loop nobody has counted may not define the
tempo — rather than falling back on the `.max(1)` that every other reader of
that field uses. It is the same lesson as `close_at`: **a default that is right
for a readout is not automatically right for a command.**

### What it costs when other loops exist

Nothing to them: they are frames, they do not move, and they stay in relation to
each other. What moves is the click and everything downstream of Link, so loops
recorded against the old click are now out with the click and still in with each
other. Sometimes that is exactly the intent. The ack counts them and says so
rather than deciding.

Cell: page 2, index 11, beside launch / click / monitor — the row that is
already the facts about the rig. Yellow, and the only yellow on that page:
every other press there acts on this rig and stops, and this one leaves it.

## 9.4.5 Stereo, and an input a loop can choose

*"I don't think I'd realised that we are summing the stereo inputs before
placing them in a stereo field."* — and it was worse than that. The engine was
mono end to end and `--in-ch` named **one** channel; the others were not summed,
they were **dropped**. Half of a stereo pedalboard, a ping-pong delay or a wide
reverb never reached the machine.

Two changes, done together because they touch the same three places — the input
callback, the ring indexing, and the source configuration — and doing them
apart would have meant rewriting the ring twice.

### Stereo

`CHANNELS = 2`, interleaved, everywhere: the arena, the pre-roll rings, every
layer, the saved WAVs. The arena doubles to 702 MiB at the defaults; `--max-secs`
is the dial. Interleaved rather than planar because the mix wants both channels
of a frame at once.

**`pan` became two controls wearing one knob**, and this is the part that is a
decision rather than a type change:

- A loop folded to **mono** is one signal being *placed*, so the equal-power pan
  is right — 3 dB down each side at centre, which is what buys a constant
  loudness across the travel.
- A loop that is **not** folded is two signals already in a field. Panning them
  would collapse a width that was recorded rather than invented, and at centre
  it would take 3 dB off both sides for nothing. What the knob means there is
  **balance**: unity at centre, one side falling to silence at the end of the
  travel, attenuating only — so a balanced loop can never be louder than the
  take, and there is no headroom to lose.

**Mono is a playback decision, not a capture one.** Andrew asked for it as a
capture option — sum a source with nothing in its sides so it can be placed
precisely. At playback it is strictly better: the audio is always kept in
stereo, so folding is free to try, free to undo, and nothing is thrown away by
a choice you had to get right before the take.

### An input per loop

`--source name=l[,r]`, repeatable, one-based on the command line. A mono jack is
a source whose two channels are the same input, which needs no special case
anywhere downstream. Without any, `--in-ch` becomes one source called `in`, so
an existing command line is unchanged.

**Per loop, and `ClaimPast` is the whole argument.** The pre-roll ring exists so
you need not decide in advance — you play something good and claim it
afterwards. A *global* input selector puts that decision straight back in front
of you, and the one time it would matter is the time you were on the wrong
input. So every source fills its own ring continuously and the loop says which
it wants when it takes it. The cost is 11.5 MB a source a channel against an
arena of hundreds.

Two things fell out rather than being designed:

- **Level-arm listens on the loop's own source.** Arm a drum loop and it waits
  for a drum; arm a guitar loop and it waits for a guitar. One shared peak would
  have had each starting on the other.
- **Monitoring follows the loop that is armed or recording**, because that is
  what you are playing into. It keeps its sides, where the click does not: the
  click is a reference and the monitor is the thing about to be recorded.

The daemon refuses `src` mid-take. Splicing two different rooms into one layer
is not something anyone means.

### What was measured

A tone on the left only, played into a BlackHole loopback, recorded and saved:
the WAV comes back `48000 Hz, stereo, pcm_f32le` with **−18.1 dB on one channel
and −91 dB on the other**. Capture, arena and export keep the sides apart.

## 9.5 Speed is bipolar, and Reverse was a spelling of its sign

`Data.Looper.Verb.Rate` has said all along that the daemon takes ±0.125 to ±4
and *the sign is the direction*. The knob only ever sent one sign, so `Reverse`
had a cell of its own — a second spelling of a number's sign — and, worse, the
ring could not show which way round a loop was: `toKnob` read `speed`, which is
a magnitude, while the direction lives in `reverse`. A loop running backwards at
half speed drew exactly like one running forwards at half speed, and had done
for as long as the knob existed. Nothing could see it while the knob could only
ask for one sign.

It is bipolar now — centre stopped, either way out faster, left backwards, which
is how the Chase Bliss pedals and the Count to 5 do it and reads immediately.
Unity is the press rather than the centre, which is the trade that bought the
direction; there are two of them, one each side. The band at the centre is
`detentWidth` wide and means exactly zero, because an exponential cannot reach
zero — a dead band is what makes stopped a place the hand can find rather than
one it approaches.

**Held is not stopped**, and this is the part that is a claim about the engine
rather than about a knob. A loop at speed zero is still playing: it has not
given up its place in the phase-locked set and it is not muted, where
`Transport` silences one and keeps its position. The two look alike from outside
and are not, so `phaseTone` gives held its own colour rather than borrowing the
muted one.

**This asks one thing of the daemon**: `sp0` is refused today. Until it is
accepted the centre band is a request the engine declines — which the ack path
says out loud, so the knob is honest in the meantime rather than pretending.

## 9.6 Still on the list

- **Retrospective recording.** `ClaimPast` has a cell on the Loops page, a
  double-press on the MC6's global row and CC 5, and `perform` sends a real
  `t` — but the daemon does not implement it. It is the one control on the
  surface that looks like it worked. Either the daemon grows it or the duty
  becomes `NotYet` on **all three** surfaces at once; a duty that means
  different things depending on what pressed it is the thing rule 1 forbids.
- **Start and end trim** still have no verb, so Shape's bottom half is empty on
  purpose. Empty is honest and a page has no obligation to be full.
- **Spread wants to be a knob** and cannot be one: the snapshot reports no
  per-loop spread, so the ring would hold a position nothing could correct. The
  smallest useful thing the daemon could add.

## 9.7 The bar, and length as a count of it

Built 2026-08-27, daemon and app together. The change everything else rests on
is one line of `Shared::grid`:

> **The bar comes from Link when there is a clock, and from the first loop when
> there is not.**

`grid()` was the anchor loop's cycle, and its comment refused Link on the
grounds that *tempo alone gives a bar's length but not where the bar falls*.
Both halves were in fact being stored by then — `link_micros`, `link_beat` and
`link_frame` — and nobody had done the arithmetic. `engine::bar_origin` does it
now, at the moment an anchor lands, which is the only place the beat position
and the frame counter are in scope together.

**What that conflation was costing.** While the pulse *was* the first loop's
length, **no loop could ever be shorter than the first one** — a one-bar kick
under a four-bar phrase is not a small feature request, it is a thing the model
had no room for. Separating them makes "a multiple of the first loop" and "a
division of the first loop" the same operation on one number.

| | |
|---|---|
| `Loop::cycles` | how many bars this loop is; `loop_len == cycles * bar` |
| `len<n>` | sets it, and does one of three things depending on the loop |
| `s<n>` / `ph<n>` | how often the newest layer sounds, and on which slot |
| `lq<n>` | what a launch waits for, in beats. Rig-wide |

**`len` does three things and says which.** On an **empty** loop it sizes it. On
the **first loop with no clock** it *declares* — `len4` on a four-bar phrase
makes the bar a quarter of it and touches no audio, which is how a clockless
session gets a loop shorter than its first take. On **anything else** it
resizes, and the layers keep their own lengths inside the new one. They are one
control because they are one question — *how many bars is this* — asked of a
loop in three states; splitting them would make the player decide which verb
they meant, which is a decision about the engine rather than about the music.

**The second press is gone.** A loop that knew its length before recording
began knows its close, so `spawn_closer` — one thread, polling every 5 ms —
closes it. It re-checks state before it acts, and that *is* the cancellation: a
foot that closes early leaves `PLAYING`, a clear leaves no length, a new take
moves `rec_from`, and any of those makes the close a no-op. There is no flag to
forget to clear. The closing press now survives in exactly one place: the first
take of a clockless session — which is the one you were going to trim anyway.

## 9.8 Length and sparsity came apart

`sparse` set the newest layer's period **and** multiplied the loop's length by
the same factor. So "how long is this loop" and "how often does the material
sound in it" were one gesture, and **a four-bar loop whose phrase sounds every
bar was not reachable at all**.

`s<n>` is absolute now and changes no length. Three knobs hold the three
numbers: **bars** on the Loops page, **every** and **slot** on Shape. They are
on two pages because they turned out to be about two different moments — a
length is what a take needs *before* it starts, the other two are what you do to
it afterwards. Which makes the thing this was asked for a matter of turning
three knobs — record a bar, make the loop four, put the bar on the third of
them — and the waveform already drew that picture,
because `l_period`/`l_phase` have modelled it since the multiply rewrite. What
was missing was never the mechanism; it was a press fixed at `s2`,
multiplicative, that also moved the length.

Absolute rather than multiplicative because a knob asks *what should this be*.
Multiplying is the right shape for a footswitch and the wrong one for a control
whose position has to be readable off the engine.

## 9.9 Tempo and metre are a readout

They arrive from link-spike, which has them from Ableton. A control here would
be a second place the rig's tempo is decided, so there isn't one: the Looper
page's legend shows bpm, metre, the bar the engine is counting in, and what a
launch is waiting for. The bar is worth showing beside Link's own, because
without a clock they differ and the difference is never obvious.

**Launch quantise is separate from the bar on purpose.** The bar is what a
*length* is counted in; `launch_q` is what a *start* waits for. A DAW keeps them
apart and so does this — "close on a whole bar" and "start on the next beat" are
both wanted at once, and collapsing them would take away free-length takes over
a quantised rig. It is in **beats**, so it means the same thing in 3/4 as in
4/4; `-1` is a bar and is the default, `0` is none. On The set, because that
page's subject is already all of them — a global on a per-loop page is how the
old page two became a drawer.

## 10. What only a knob can do — and which of it is engine work

The per-loop vocabulary the daemon implements is `sp pan xf dec ch s<n>` plus the
flags. Everything below is absent from `dispatch` and is therefore **daemon work
before it is app work**:

- **Start and end trim.** The one Andrew asked for, and the most interesting: it
  interacts with `period`/`phase` and with the crossfade continuation, since the
  frames after the loop closed are kept precisely so the wrap can be continuous.
  Trimming moves the join.
- **Origin nudge** — the Echoplex start-point move. Today `OneShot` is
  documented as *the one gesture that moves a loop's zero*, and it is a mode for
  that reason: losing your place in the phase-locked set should be something you
  switch on. A nudge knob needs a deliberate answer about the grid, not a
  parameter.
- ~~**Per-loop level.**~~ **Added 2026-08-25, and the hardware is what decided
  it.** This section said to build the surface on what exists and let a session
  say whether the missing level was felt. The session said so within minutes,
  and not in the way expected — see §8's new note. `vol` is now a verb: a
  linear gain per loop folded into the pan gains once per buffer, `0` to `-60`
  dB, refusing above unity the way `dec` refuses positive, with silence at the
  bottom of the travel because a fader that cannot reach zero is one you do not
  trust.
- **Scrub.** Falls out of start/end almost for free once positions are
  addressable, and is the reason to do them together.

Continuous **speed** and **pan** need no engine work at all — `sp` and `pan` take
numbers, and it is only the `Rate`/`Place` ladders that discretised them.

## 11. Rules

Written to be checkable.

1. **One vocabulary.** A duty that no surface can reach is dead code; a command
   that is not a duty is a bug. `perform` is the only way to the socket.
2. **Nothing hardware-only.** Every duty is reachable from the page. Tested by
   enumeration over `Duty`, the same way the meaning table is.
3. **Every per-loop command carries its subject.** No caller may rely on the
   daemon's selection. The daemon's own comment: *"selection that only some
   callers depend on is a mode."*
4. **A step duty delegates to a value duty.** No parameter has two paths to the
   wire.
5. **No state in a controller that the app cannot correct.** An encoder ring is
   written from the snapshot, so it complies. An MC6 toggle position does not,
   unless §12 says it does — until then a toggle is acceptable only where the
   app mirrors the daemon back within a frame, exactly as `ClickToggle` already
   argues for itself.
6. **The Twister is configured to the stock CC map, and the repo holds no device
   config.** *Device as stateless output; the store is the truth.* The one known
   deviation is the side buttons on channel 5 — either bring them back to stock
   or write down why not.
7. **LED writes are a courtesy and never block.** Same rule as `ShowBank`: audio
   never waits on a display.

## 12. To verify on the hardware

Claude cannot see the rig; these are Andrew's to check, and three of them change
the design if they come back the other way.

- **Does the Twister's stock map really put bank *b*, encoder *i* on CC
  `16b + i`, turns on ch 1 and presses on ch 2?** The whole of §8 assumes it.
- **What is currently loaded on the device?** Side buttons on ch 5 CC 8/9/10 is
  not stock, so something is configured. Rule 6 wants to know what.
- **Which channels drive the ring and RGB *animations*?** A pulsing encoder for
  a recording loop is worth having if it is one CC.
- **Are the hues right?** `Data.Looper.Twister` names seven wheel positions and
  maps the six `LoopPhase` values onto them, and not one of them has been seen
  on the hardware — they were chosen from the range the twelve pedals already
  use. Recording must be unmistakable at a glance; the rest can be argued
  about. This is the item most likely to need an afternoon rather than a
  minute.
- **Is `MsgSetToggle` reachable from outside the device** — can the app put a
  switch's toggle position where the daemon says it should be? If yes, §7's
  objection to device-side state weakens and the toggle work gets easier.

## 13. Order of work

The Twister is last, which is the point of §1: the reference surface is the one
on the wrong table.

1. ~~**Move the page onto the machine.**~~ **Done 2026-08-25.**
   `Data.Looper.command` is CC → `Duty`; page buttons resolve exactly as
   footswitches do. Disposed of the bare-address bug as a side effect, and
   turned up two duties the CC table had been calling unimplemented while the
   machine had been doing them for weeks (`Redo`, `SaveTake`) and one the
   machine had never heard of (`Multiply`).
2. ~~**Expose `perform :: Rig -> Subject -> Duty -> Array Action`.**~~ **Done.**
   `Banks.decodeSwitch` is one decoder of three. Value duties added and the
   `Step*` family delegates to them. `Rig` grew the two global flags, which let
   `Verb.ClickToggle` — the last flipping form the app sent — be deleted.
3. ~~**Eight loops.**~~ **Done**, including the daemon: `N_LOOPS` is 8 and the
   self-test returns +0 samples on all four phases.
4. ~~**Teach `parseTwisterMsg` about banks.**~~ **Done.** The side buttons were
   left alone — see §8; the device owns the bank.
5. ~~**The Twister decoder and the two bank tables**, plus LED feedback driven
   from the snapshot.~~ **Done.** `Data.Looper.Twister`; the lights are diffed
   against what was last sent so a still frame costs no messages.
6. **The MC6 toggle work** — `toggleName` through `ControlBankSwitch` and
   `SysEx`, then `Record`/`Stop` and the flag switches. Independent of all of
   the above, and **not** done.
7. **Start/end trim in the daemon**, once there is a knob worth putting it on.
   Not done, and §9 explains why the knobs were not built ahead of it.

## 14. Settled, 2026-08-25

- The page is the reference surface; MC6 and Twister are optional controllers.
- One `Duty` vocabulary, three decoders, one `perform`.
- Subject is explicit; nothing relies on the daemon's selection.
- Eight loops; 7 and 8 are page-and-Twister only.
- Two Twister banks now, two reserved for the layer surface.
- The Twister needs no gestures, and therefore no lateness compensation.

## 15. What the work turned up

Three things found while building it that were not in the design.

- **Two of `Verb`'s unit comments were wrong**, and had been for as long as they
  existed. `Rate` said "a percentage: 100 is unity" while the daemon takes 0.125
  to 4 and every caller had always sent `Rate 0.25`; `Chance` said "a
  percentage, 100 is always" while the daemon takes 0 to 1 and `chanceLadder`
  has always held 1.0, 0.75, 0.5. Nothing was broken — no caller ever believed
  either comment — which is exactly why they survived. They matter now because
  the knob scales are written *from* those ranges.
- **`Multiply` was reachable only from the page.** The MC6 loop family has never
  had a switch for it, so the reference surface could ask for something the
  machine had no word for. That is what having two vocabularies costs, and it
  went unnoticed because each table was complete on its own terms.
- **The arena is 351 MiB at eight loops, not 61 MB.** The engine's header figure
  was stale by a wide margin; see §5.

## 16. Open questions

1. ~~**Does a per-loop level get added to the daemon?**~~ Answered by the first
   session on the hardware, and by the hardware rather than by the music: §8.
   What is still open is the **taper** — the fader is linear in decibels, so
   half its travel sits below -30 dB where differences are hard to hear. A real
   fader spends most of itself between -12 and 0. Left linear because the ring
   has to be its inverse and a two-segment taper is two chances to get that
   wrong; revisit after a session of actually mixing with it.
2. **Are 60 ms and 300 ms the right windows?** Both are guesses from one report
   of the press-nudge, not measurements. If a press still moves a level, raise
   `turnHoldMs` first.

   The 300 was also being counted from the **press**, which meant a press held
   longer than that disarmed the guard while the finger was still down — so the
   nudge that comes with letting go landed as a real turn (2026-08-27). On the
   pager that read as pressing to go home and being paged somewhere else on the
   way back up, which happens exactly as often as you hold the press a beat.
   The guard now lasts as long as the finger does, plus the window.
3. **Should the tape-length knob step like the pager?** A sweep fires ten
   `blank`s in half a second — harmless, since each re-threads an empty tape,
   but imprecise if you want a particular length.
4. **What moves the MC6's page when the Twister changes focus?** `followBoard`
   only tracks presses it saw, so a Twister-driven `SelectLoop` leaves the board
   showing another loop's page. Push a `ShowBank`, or accept a stale board and
   let the page be where you look.
5. ~~**Ring as value or ring as playhead?**~~ Answered by the hardware: the ring
   *is* the encoder's value, so a playhead drawn on it was a playhead written
   into the number the next touch would send. A ring on an encoder that carries
   a value belongs to that value. The old text:  Bank 1 wants the playhead; a loop's
   chance then has no readout on its own knob. Colour saturation is a candidate
   and needs an experiment more than an argument.
6. ~~**Does the Looper page still read at eight rows?**~~ Answered by making it
   two rows of four, the same grid as everything else — §5.1.
7. **Do the two hardware surfaces ever disagree about focus?** Both write
   `looperFocus` and the machine is stateless, so nothing can corrupt — but two
   people, or one person and one forgotten press, can surprise each other.
