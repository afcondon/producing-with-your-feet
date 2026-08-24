# Year Zero — the rules for banks, switches and globals

*2026-08-24. Supersedes the bank map in the header of `Data/MC6/Reserved.purs`,
which was written bottom-up from a series of local decisions and should be
regenerated from this.*

## Why this document exists

Nobody ever decided how the MC6 was laid out. Each bank was placed for a local
reason at the moment it was needed, and the accumulated result was then written
down as if it had been designed. Three bugs in two days came directly out of
that, and all three were the same bug wearing different clothes:

- **The probe and a control page both sat on bank 20.** A collision that does
  not fail but *uploads* — the second write landing on top of the first.
- **Five pages copied off the device sat on top of the loop machine's own
  pages.** The write sent both, the device kept the last, the survey compared
  the first, and five banks reported "device disagrees" about writes that were
  perfect. A day went looking for a fault in the hardware.
- **A board assigned to switch A of bank 21 is destroyed by every sweep**,
  because assignments are a source of truth that the thing which writes the
  device has never heard of.

None of these were coding mistakes. They were the absence of a rule, discovered
one at a time by the device.

So this is the rule set. It is written to be *checkable* — most of what follows
should end up as a test or a refusal, not as a paragraph someone remembers.

## What this instrument is

Two facts about the *use* of the board, both of which change what counts as a
good design and neither of which was written down before.

**This is a home-recording pedalboard, not a live-performance rig.** The app is
on a screen in front of you the whole time. Nothing has to survive being
unreadable, and nothing has to be recoverable by foot alone — if the board ends
up somewhere unhelpful you can look, or fix it from the app. That does not make
navigation unimportant, but it does mean a stranded bank is an *annoyance*
rather than a disaster, and it means the earlier rationale in this file about
what "bites mid-take" was borrowed from a different instrument. Design pressure
should come from convenience, not from the dark.

**The app is where presets are authored; the board is where they are recalled.**
Andrew's design intent, stated 2026-08-24: build individual pedal presets in the
app, then compose most **full-board presets** from those. With pedals this
complex and this many of them, that is an ergonomic necessity rather than a
preference — nobody assembles a MOOD patch by foot.

That has a consequence for the layout, and it is the reason board presets get a
block of their own with headroom:

> **The MC6 is primarily a recall surface for board presets.** Everything else
> on it — pedal-preset pages, live control — is secondary to that, and the
> pedal-preset pages exist mostly as a *staging area* for material that will end
> up inside a board preset later.

## Vocabulary

Words that were being used for more than one thing, pinned to exactly one each.

| Term | Means |
|---|---|
| **bank** | One of the device's thirty pages of twelve switches. Always a *wire* number, 0–29. |
| **switch** | One of the twelve positions on a bank, `A`–`L`. |
| **page** | A thing *this app* authors, which compiles to a bank. Pages and banks are not the same kind of object: a page is source, a bank is output. |
| **producer** | Code that generates pages from other state — the loop machine, the diagnostics builder, the board compiler. |
| **global** | A switch that is on every page. Not a device feature; it compiles to one copy per bank. |
| **assignment** | A board preset bound to one switch of one bank. |
| **mark** | The `03A r7` a sweep writes onto a switch it leaves blank, and the run number on a bank name. |
| **sweep** | One whole-map write: every bank the app generates, plus a blank over every bank nothing claims. |

**Retired: "board mirror."** It was my coinage, it named bank 1, and it blurred
three unrelated facts together — that bank 1 was home, that boards were assigned
there, and that the sweep skipped it. Only the first was ever load-bearing, and
under these rules it moves to bank 0 anyway. The word does not survive Year Zero.

## The hardware, as constraints

Facts about the MC6 MKII that the layout has to be true about. These are not
negotiable and several of them have already cost a day each.

1. **Thirty banks, numbered 0–29 on the wire.** Morningstar's editor and the
   device's own screen display each one higher. The wire number is the only one
   that appears in code, in the UI, or in this document.
2. **Twelve switches per bank**: six on the unit, six more from two FS3X
   expanders. Physically the unit's six are `D E F` across the top and
   `A B C` across the bottom (`Survey.physicalOrder`); `G`–`L` are the
   expanders.
3. **The expanders exist only because of a settings section we cannot
   write.** `omniports` (`03 23`) is what makes them real, and the only settings
   section with a proven write is `03 21`. **A factory reset would leave six
   switches, not twelve, and we could not put them back.** No plan may require
   one.
4. **Sixteen messages per switch, silently truncated** past that
   (`Board.messageLimit`).
5. **Names are 8 characters for a switch, 24 for a bank**, padded or truncated
   without comment, and seven-bit ASCII only — an em dash truncates the frame.
6. **An upload frame's bank number is ignored.** The device writes to whatever
   bank it is currently standing on, so every write is preceded by a jump and a
   confirmation.
7. **There is no acknowledgement in MIDI**, so every write waits for the device
   to say something rather than assuming a delay was long enough.

## The organising idea: bank 0 is a gateway

The instrument has *usage models*, not a numeric order. In any given moment the
board is being used as a looper, or as a set of full-board recalls, or as a set
of one-pedal recalls, or as a live control surface. Those are different
instruments that happen to share a chassis.

So **bank 0 is a gateway**: twelve switches, each a jump into the entry page of
one usage model. Nothing else lives there.

This has a consequence worth stating on its own, because it makes most of the
remaining layout decisions free:

> **With a gateway, bank numbers stop being ergonomic and become addresses.**

Nobody scrolls through banks any more; you go home and jump. That means the
*position* of a block barely matters, and the two things that do matter are that
a block is **contiguous** (so a producer can compute `base + index`) and
**stable** (so a bank number in a jump keeps meaning the same page tomorrow).

It also kills the old "machinery at the low end, user space from 15 up" rule.
That rule existed to keep the useful pages within scrolling distance. With a
gateway there is no scrolling distance.

## The map

Blocks, each with a declared size. Sizes marked **?** are the open questions at
the end of this document; everything else follows from what exists today.

```
wire     block            size   what
────────────────────────────────────────────────────────────────────────
  0      gateway            1    one switch per usage model
  1- 4   control pages      4    one per functional group: its bypasses and its controls
  5-12   looper             8 ?  the loop machine's pages + transport
 13-15   board presets      3    36 slots for the 24 wanted, a page of headroom
 16-23   pedal presets      8    recall, step-through, and deep single-pedal pages
 24-27   spare              4    unclaimed, cleared by every sweep
 28-29   machinery          2    probe + diagnostics, resident
```

**The bases are not fixed yet**, and should not be written into code until the
looper's page count is settled — it is above almost everything and moves it all.
Per B3 a block's base and size live in one place, so this is one edit when it
comes, not thirty.

### Control pages are grouped by function, not by pedal

The first draft of this had a page of every bypass, on the assumption that
twelve pedals fit twelve switches. That assumption is wrong twice over.

**It is wrong arithmetically.** Four of the thirteen registry entries are
`DualEngage` — Flint, Lost+Found, MOOD and Onward each have two independently
bypassable channels. Flint and MOOD declare a `both` CC that takes the whole
pedal out in one message; Lost+Found and Onward do not, and so cannot be reduced
to one switch at all without an app verb (B17).

**And it is wrong as a way of thinking.** There is no such thing as a pedal
that is always on — Iridium and Riverside are simply the *least likely to be
changing*, and they still move for gain structure and tone. The useful division
is by what a pedal is **for**:

| group | pedals |
|---|---|
| tone / gain structure | Clean, Riverside, Iridium |
| ambient / evolving | MOOD, Onward, Lost+Found, Habit |
| delay / reverb | Mercury7, Flint, Brig |
| live FX | Lex, plus second-board pedals not under MIDI control |
| *unplaced* | Hedra |

A wall of twelve bypasses is a list of things; a group page is a **thing you are
currently doing**. So each control page carries its group's bypasses *and* the
handful of controls that matter when you are working on that group — which is
also what makes the leftover switches on each page worth having rather than a
sign the page is half empty.

Hedra is deliberately unplaced. It is a harmoniser rather than an effect — it
generates new material — and it is the pedal that most obviously wants a page of
its own rather than three switches on somebody else's.

**Worth chasing: the missing `both` CCs.** If Lost+Found and Onward have a
true-bypass CC we have not transcribed, each drops from two switches to one and
the ambient page gains room. The same two CCs relieve the sixteen-message
ceiling on board presets, where four dual pedals costing two bypasses apiece is
what puts an all-twelve board over the limit (`Data/Pedal/Engage.purs`,
DESIGN-v2 §5). One transcription job paying twice.

### Pedals are not equally deep, and pages should not pretend otherwise

The single most useful thing to know about this block: **the amount of control a
pedal wants varies enormously, and not with how good it is.**

- **Hedra** has had elaborate multi-bank treatments in the past, because it is
  genuinely that complex. It warrants a page, possibly more.
- **Brig** has never needed more than three switches in its entire life: tap
  tempo, infinite repeat on hold, and a preset scroller.

So "a page per pedal" is the wrong unit. The right one is **a page per pedal
that needs one, and a shared page for the pedals that do not** — Brig's three
switches sitting alongside two or three other shallow pedals' three, which is a
better page than either would make alone.

This is why the pedal block is described as recall, step-through *and* deep
single-pedal pages: they are three shapes drawn from one pool of banks, assigned
by what each pedal actually needs rather than by a uniform rule.

### Why the pedal-preset block holds two different kinds of page

253 pedal presets exist and 96 slots do not hold them, so this block is a
**curated, growing selection** rather than a mirror of the store — populated as
Andrew finds settings worth keeping, with the expectation that many graduate
into board presets and stop needing a slot of their own.

Two page shapes share the block, and they are not the same tool:

- **Recall pages** — one switch per preset, for material you know you want.
- **Step-through pages** — a switch walks forward through one pedal's presets,
  which is the shape that suits *seeking inspiration* rather than retrieving a
  known sound. This is explicitly wanted and is the reason the block is eight
  pages rather than the two or three that direct recall alone would justify.

## The rules

Numbered so they can be cited in a refusal message or a test name.

### Addressing

**B1. Wire numbers everywhere.** Code, UI, logs and documents use 0–29. The
editor's off-by-one is a fact about Morningstar's software and is converted, if
ever, at the very edge.

**B2. One page per bank.** Two pages claiming one bank is an error, never a
merge and never a last-one-wins. The sweep refuses rather than writing something
it could not afterwards check (`ControlBank.doubleClaims`).

**B3. Blocks are contiguous and declared.** A block has a base and a size, both
named in one place, and a producer addresses its pages as `base + index`. A page
placed by hand outside its block is a failing test, not a convention.

**B4. Every bank is accounted for.** Each of 0–29 is exactly one of: generated
by a producer, cleared by the sweep, or explicitly exempt with a stated reason.
There is no fourth state, and "we have not looked" is not an answer the map may
give.

### Navigation

**B5. Every page has a way home**, and home is bank 0. On a recording board this
is convenience rather than rescue — the app can always put you back — but a page
with no way out is still a page you have to stop and think about, and thinking
about the pedalboard is the thing this app exists to prevent. It is also why
globals are applied to cleared banks: a blank page with no exit is worse than a
blank page.

**B6. A bank jump must point at a bank something claims.** A jump into empty
space is a dead end; `Survey.stranded` and `Survey.deadEnds` exist to find both
and should be surfaced, not just computed. Softer than it sounds, since the
device's own bank up/down still works — the accusation is "no programmed way
out", not "trapped".

### Gestures

The MC6 recognises gestures itself, and what it does was **measured on the
device** (2026-08-21, `Diagnostics.gestureProbeBank`) rather than taken from
documentation:

```
single tap    Press and Release arrive 1 ms apart, at the DECISION
double tap    DoubleTapRelease alone — Press and Release both suppressed
long press    Press, then LongPress ~600 ms later, and NO Release
```

So `Release` / `DoubleTapRelease` / `LongPress` is a **clean, mutually exclusive
triple**. One switch carries three meanings with nothing to disambiguate by
hand — which is what makes the following convention possible at all.

**B14. Discrete actions fire on foot-up, and the release family is the
vocabulary.** For a two-channel pedal:

| gesture | action | means |
|---|---|---|
| tap | `Release` (2) | toggle the whole pedal |
| double tap | `DoubleTapRelease` (6) | toggle the first channel |
| long press | `LongPressRelease` (4) | toggle the second channel |

`LongPressRelease` rather than `LongPress` so that **all three fire at the same
moment relative to the foot** — when it comes up. Mixing them would mean one of
a switch's three meanings happening mid-press and the other two after, which is
the sort of inconsistency a foot notices without being able to say why.

The particular assignment of channel to gesture matters less than its being
identical on every page. A foot that has to remember which pedal reversed them
has learnt nothing.

*Not yet measured:* the probe covered `Release`, `DoubleTapRelease` and
`LongPress`. `LongPressRelease` is in the model (`MC6Action` 4) and in
Morningstar's action list, but this rig has not confirmed how it interacts with
`LongPress` — whether binding only the release suppresses the threshold event,
and whether the ~600 ms threshold still gates it. That is a probe run, and B14
rests on the answer. There is a fourth release-family action
(`LongDoubleTapRelease`, 8) which we are deliberately not spending.

**B15. A switch that binds no double-tap must still answer one.** The device
suppresses `Release` on a double *whether or not anything is bound to it*, so a
switch with only one meaning answers a fumbled double with **silence** — the
worst possible response, because nothing tells you it happened. Bind the tap's
own value to `DoubleTapRelease` and two taps too close together come out as one
tap. `Looper.Banks.bindings` already does this; it is a rule, not a local trick.

**B16. Nothing rhythmic carries a double-tap.** The device withholds the single
press until it knows the gesture, so any switch that *might* be double-tapped
answers a few hundred milliseconds late. That is tolerable for a bypass and
fatal for tap tempo, which is both immediate and repeated. The double-tap window
is currently a guess bounded at 414 ms and is the one number still worth
measuring.

*(For the looper this cost is already bought back: gestures are dated from the
press and the pre-roll ring un-does the delay, so a double-tap costs response
but never the recording. Nothing else has a ring, so nothing else gets that
refund.)*

**B18. A momentary control is a `Press`/`Release` pair and carries nothing
else.** Some controls are active only while the foot is down — MOOD's right-hand
micro-looper, Brig's Infinite, Lex's Brake. These cannot use the release family,
because the entire point is that something happens on the way *down*: the switch
sends on `Press` and undoes it on `Release`.

Such a switch must carry **no double-tap and no long press**, and the reason is
the same measurement as B16 seen from the other side. The device withholds
`Press` until it knows the gesture, so a momentary engage on a double-tappable
switch arrives a few hundred milliseconds after the foot did — and on an actual
double it never arrives at all, because `Press` is suppressed outright. A
momentary switch is a one-gesture switch, and that is a property of the pedal's
control rather than a choice about the page.

**B17. A switch speaks either to a pedal or to the app, and says which.** A
direct message is stateless and works whether or not anything is running. An
**app verb** — one CC on the recall channel, which the app expands — is how
anything *conditional* has to work, because the device cannot ask a question.

"Toggle the whole pedal" on Lost+Found or Onward is exactly such a case: with no
`both` CC, toggling both channels means sending two messages, and if the
channels are currently in different states there is no single right pair to
send. The app knows the state; the device does not. So that gesture is an app
verb.

The price is that an app verb does nothing with the app closed — acceptable
precisely because this is a recording board with the app on screen, and a good
example of that framing paying for itself.

### Globals

**B7. A global is on every page, or it is not a global.** No membership list, no
per-page exception. The escape hatch is `dissolve`, which turns it into an
ordinary local copy everywhere and forgets the link — the exception bought once,
deliberately, rather than thirty times.

**B7a. A global's *target* may be a function of the block, never of the page.**
Inside the looper's pages, "back" means back to the Loops grid you came from,
not the instrument's front door — going home from there is a second press you
would always make anyway. So the way home is re-aimed per block
(`Global.applyGlobalsTo`).

This is not the per-page override B7 closed, and the difference is worth being
precise about. The slot is unchanged on every page, so your foot still knows
where it is. There is still exactly one definition to edit, and editing it still
changes everywhere. What varies is a target **computed from the map**, and a
page cannot disagree with the map — so the three states that killed overrides
(local / global / global-but-not-here, with no way to tell which you were
standing in) do not come back. You cannot override this from a page; you can
only move a block. The block's own entry page keeps the global's original
target, or its way out would be a jump to where it already is.

**B8. A global costs one switch on all thirty pages.** With twelve switches that
is an expensive commitment, so the number of globals is a budget: currently one
(`< Back` on `G`). Adding a second should feel like a decision.

**B9. A global overwrites whatever a page had there.** Warn when that destroys
real work, and only then — a warning that fires on the ordinary case is one
people learn to click past (`Global.displacedByGlobals`).

### Compilation

**B10. The device is output; the store is the source.** Nothing reads the device
to decide what to write. A read is for *checking*, never for deciding.

**B11. Every source layer is compiled by the sweep.** This is the rule the
assignment bug broke: a source of truth that the whole-map write does not know
about is silently destroyed by it. If something can put content on a switch, the
sweep compiles it or the sweep is wrong.

The layers, in the order they are applied, last wins:

```
  page content        what a producer or a person authored
      ↓
  assignments         boards bound to a switch of a bank
      ↓
  globals             furniture, unconditional, every page
      ↓
  marks               only where nothing else claimed the switch
```

**B12. One list is written and checked.** The sweep and the survey call the same
function (`intendedMap`). A check that assembles its own idea of the expected
answer can agree with itself while disagreeing with what was sent — which it did,
four separate times, each one presenting as a hardware fault.

**B13. Blank switches carry marks.** An empty label is wasted evidence. A mark
says which bank it thinks it is on, which switch, and which sweep wrote it, which
is what separates "never written", "written last time", and "landed on the wrong
bank".

## What is transient

Standing, until the board is used in earnest (Andrew, 2026-08-19 and 2026-08-24):

- **Everything on the MC6 is transient.** It can be overwritten at any time
  without asking. It is compiled output and the store can regenerate it.
- **Everything in the app's store is transient too — except the pedal
  presets.** Those 253 files are the one irreplaceable thing here; they encode
  work at the pedals that cannot be re-derived from anything.
- **The one thing that must not happen is a factory reset**, per constraint 3
  above. Not because of the banks, which we can rewrite, but because of
  `omniports`, which we cannot.

## Settled, 2026-08-24

Answers to the first round of open questions, recorded so they are not
re-litigated:

- **Board presets: 24 is the target**, given three banks so there is a page of
  headroom. They are the primary surface (see *What this instrument is*).
- **Pedal-preset pages are a growing curated selection**, added as settings are
  discovered, with step-through pages alongside recall pages for inspiration.
  Eight banks.
- **Machinery stays resident.** The probe and diagnostics keep two banks; there
  is enough space and no reason to make them ephemeral.
- **Control pages are grouped by function**, four of them, each carrying its
  group's bypasses and the controls that matter while working on that group.
  "Always on" is retired as a concept — Iridium and Riverside are merely the
  least likely to change, and they still move for gain and tone.
- **Ableton is deleted for now.** The existing page at bank 19 goes; when it
  comes back it will most likely be session-record control rather than a second
  looper, and it can come out of spare. Nothing in this map reserves it.
- **Gesture conventions are settled** (B14–B17): tap / double-tap / long press
  as whole pedal / first channel / second channel, identically everywhere.

## Open questions

1. **Are the looper's eight pages settled?** Seven loop-machine pages plus the
   legacy hand-driven transport. It sits above almost everything, so this is the
   question blocking the bases being written into code.
2. **Where does Hedra go?** It is a harmoniser rather than an effect and wants a
   page of its own; whether that page lives in the control block or the pedal
   block decides how the groups are counted.
3. **Which pedals share a shallow page?** Brig needs three switches; presumably
   others do too. The pairing is a judgement about how you work, not something
   the registry can answer.
4. **Do Lost+Found and Onward have a true-bypass CC?** A transcription question
   against the pedals' MIDI implementation, not a design one — but it buys a
   switch each and relieves the board-preset message ceiling.
5. **How does `LongPressRelease` actually behave?** B14 uses it so that all
   three gestures fire on foot-up, but the probe never covered it. One run of
   the gesture bank answers it.
6. **What is the real double-tap window?** Bounded at 414 ms by measurement and
   still a guess inside that. It sets how late every double-tappable switch
   answers, so B14 and B18 both rest on it.
7. **What else belongs on the gateway?** Twelve switches; the list so far names
   five — looper, board presets, pedal presets, control pages, machinery.
