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
  clear it was not part of a press — 60 ms — while a press or release deafens
  its own encoder for 300 ms afterwards, because the nudge can land on either
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
 Rec   Ovr   Stop  Arm                 Spread Shift  Dense  Save
 Undo  Redo  Clear Capture             Rev    Pend   1Shot  Listen
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
3. **What moves the MC6's page when the Twister changes focus?** `followBoard`
   only tracks presses it saw, so a Twister-driven `SelectLoop` leaves the board
   showing another loop's page. Push a `ShowBank`, or accept a stale board and
   let the page be where you look.
4. **Ring as value or ring as playhead?** Bank 1 wants the playhead; a loop's
   chance then has no readout on its own knob. Colour saturation is a candidate
   and needs an experiment more than an argument.
5. ~~**Does the Looper page still read at eight rows?**~~ Answered by making it
   two rows of four, the same grid as everything else — §5.1.
6. **Do the two hardware surfaces ever disagree about focus?** Both write
   `looperFocus` and the machine is stateless, so nothing can corrupt — but two
   people, or one person and one forgotten press, can surprise each other.
