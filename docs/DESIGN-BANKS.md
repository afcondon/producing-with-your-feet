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
  1- 2   live control       2    whole-pedal bypasses, then per-channel + tap
  3-10   looper             8 ?  the loop machine's pages + transport
 11-13   board presets      3    36 slots for the 24 wanted, a page of headroom
 14-21   pedal presets      8    recall pages and step-through pages, curated
 22-27   spare              6    unclaimed, cleared by every sweep
 28-29   machinery          2    probe + diagnostics, resident
```

### Why live control is two pages, not one

The tidy "twelve pedals, twelve switches" reading is wrong, and the code already
knew why: **four of the thirteen registry entries are `DualEngage`** — Flint,
Lost+Found, MOOD and Onward each have two independently bypassable channels.
Two of them (Flint, MOOD) declare a `both` CC that takes the whole pedal out in
one message; two (Lost+Found, Onward) do not, and so cannot be reduced to a
single switch at all.

Counting what actually needs a switch, with Iridium and Riverside left out as
always-on and Itajara excluded as the looper:

| | switches |
|---|---|
| single-engage pedals — Brig, Clean, Habit, Hedra, Lex, Mercury7 | 6 |
| dual with `both` — Flint, MOOD | 2 |
| dual without `both` — Lost+Found, Onward | 4 |
| **whole-pedal control** | **12** |
| per-channel for Flint and MOOD as well | +2 |
| **per-channel control throughout** | **14** |

So page one is a full twelve with nothing spare, and *anything else at all* —
tap tempo, per-channel splits for Flint and MOOD, a bypass for a pedal currently
assumed always-on — needs page two. Hence two.

**Worth chasing: the missing `both` CCs.** If Lost+Found and Onward turn out to
have a true-bypass CC we have not transcribed, whole-pedal control drops from 12
switches to 10 and page one gains room. The same two CCs also relieve the
sixteen-message ceiling on board presets, where four dual pedals costing two
bypasses apiece is what puts an all-twelve board over the limit
(`Data/Pedal/Engage.purs`, DESIGN-v2 §5). It is one transcription job paying
twice.

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

### Globals

**B7. A global is on every page, or it is not a global.** No membership list, no
per-page exception. The escape hatch is `dissolve`, which turns it into an
ordinary local copy everywhere and forgets the link — the exception bought once,
deliberately, rather than thirty times.

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
- **Live control is two pages**, because whole-pedal bypasses fill the first
  exactly.

## Open questions

1. **Are the looper's eight pages settled?** Seven loop-machine pages plus the
   legacy hand-driven transport. If the transport is dead, that is a bank back
   and everything below it shifts.
2. **Does Ableton get a block?** There is a working page at bank 19 today and a
   stated want to drive Ableton from the looper UI later. It is currently
   nowhere in this map.
3. **What else belongs on the gateway?** Twelve switches; the list so far names
   five — looper, board presets, pedal presets, live control, machinery.
4. **Do Lost+Found and Onward have a true-bypass CC?** A transcription question,
   not a design one, but it buys two switches on the live-control page and
   relief on the board-preset message ceiling.
5. **Which pedals are genuinely always-on?** Iridium and Riverside are assumed
   so here, and that assumption is load-bearing for the twelve-switch count.
