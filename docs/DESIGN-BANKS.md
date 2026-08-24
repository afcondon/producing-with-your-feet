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
  1      live control       1    a pedal bypass per switch, plus tap
  2- 9   looper             8    the loop machine's pages + transport
 10-11   board presets      2    24 full-board recalls, 12 to a page
 12-23   pedal presets    12 ?   one page per pedal, 12 presets each
 24-26   spare              3    unclaimed, cleared by every sweep
 27-29   machinery          3 ?   probe + diagnostics
```

Two observations that fall out of the arithmetic and are worth knowing before
committing to it:

- **Twelve pedals fit exactly on twelve switches.** The live-control page is one
  bypass per switch with nothing left over — so tap tempo, if it is wanted, costs
  a second page or a long-press.
- **253 pedal presets exist and 144 slots do not hold them.** The pedal-preset
  block is necessarily a *curated selection*, not a mirror of the store. Which
  pedals get a page, and which of their presets, is a decision the app has to
  make explicitly rather than by truncation.

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

**B5. Every page has a way home**, and home is bank 0. A page you can walk into
and not walk out of is the failure that bites mid-take, and it is the reason
globals are applied to cleared banks too.

**B6. A bank jump must point at a bank something claims.** A jump into empty
space is a dead end; `Survey.stranded` and `Survey.deadEnds` exist to find both
and should be surfaced, not just computed.

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

## Open questions

The map above is committed except where marked. These are the decisions that
change it, and they are Andrew's:

1. **How many pedals get a preset page?** Twelve pages is 40% of the device for
   a feature that may only be wanted for the three or four pedals actually
   swapped mid-set. This is the single biggest lever on the layout.
2. **Are the looper's eight pages settled?** Seven loop-machine pages plus the
   legacy hand-driven transport. If the transport is dead, that is a bank back.
3. **Does Ableton get a block now?** There is a working page at bank 19 today
   and a stated want to drive Ableton from the looper UI later.
4. **Is live control one page or two?** Twelve pedals fill twelve switches
   exactly, leaving nowhere for tap tempo.
5. **Should the machinery pages be resident at all?** The probe and the two
   diagnostics pages are debugging instruments, not usage models. They could be
   written into a scratch bank on demand and cleared afterwards, returning three
   banks and removing them from the gateway.
6. **What else belongs on the gateway?** Twelve switches, and the list so far
   names four or five.
