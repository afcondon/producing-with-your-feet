# Producing With Your Feet — v2 design

**Status:** proposal, 2026-08-13. Written against the owner's brief of the same
date, for reaction before the work starts.

**Relationship to existing docs.** `mc6-sysex-programming-plan.md` (March 2026)
documents the SysEx transport — protocol frames, upload session, message types.
That layer stands and v2 builds on it. `loopypro-clip-settings.md` and
`loopink-analysis.md` describe an integration v2 removes; keep them as history,
not as specification.

---

## 1. The premise

**The pedalboard becomes a rack unit.** Nothing on it gets stomped. The MC6 is
the only thing on the floor, and everything else is reached by MIDI — from an
iPad while exploring, from the Mac while programming.

That reframes the app. It is not a remote control for a pedalboard you are
standing on; it is the only way those pedals are ever touched.

Two workstations, deliberately different:

| | Transport | For |
|---|---|---|
| **iPad** | Bluetooth (WIDI) to the pedalboard | Exploration, with a guitar in hand |
| **Mac** | USB-C to the MC6 | Programming the MC6, recording |

The iPad cannot do both, and this is structural rather than a misconfiguration:
as BLE central it holds both WIDI transceivers, leaving neither free for the
MC6→pedalboard link. While the iPad is exploring, the MC6 is not a foot
controller. That is an acceptable trade because the two activities do not
overlap in time.

---

## 2. The central problem: pedal state is a belief

Most of these pedals cannot report their state. Nothing comes back. So the
app's model of a pedal is **a projection from a known baseline plus the changes
it has since applied** — never an observation.

This is the fact that shapes everything else, and v2 should make it explicit
rather than leave it implied.

### What follows

- **A baseline must be asserted, not assumed.** An explicit action — "this
  pedal is now at its default/known state" — with a timestamp. Until that has
  happened, the app knows nothing.
- **Confidence decays, and only the user can restore it.** Any out-of-band
  event invalidates the belief: someone turns a physical knob, the pedal is
  power-cycled, a preset is recalled from the pedal's own footswitch. The app
  cannot detect any of these.
- **So confidence must be visible per pedal.** Trusted since baseline; suspect
  once anything unobservable may have happened. A pedal whose state is merely
  *assumed* should look different from one that is *known*.

### Why this matters more than it sounds

The diff-audition feature (§3) is only sound relative to a trusted belief. A
diff computed from a stale model sends the wrong parameters and produces a
sound that is neither the old preset nor the new one — and nothing in the
system can tell you that happened. Silent wrongness is the failure mode to
design against.

---

## 3. The preset lifecycle

A preset moves through four states, and the design should name them:

**Develop.** Direct control from a known baseline. The app tracks every CC it
sends, which is what makes the resulting values trustworthy.

**Save.** Named, written to the store (`pwyf-store`). Values, notes, the pedal
it belongs to. This is the durable artefact.

**Audition — send as a diff.** Send only the CCs that differ between believed
current state and the target preset. Non-destructive, consumes no slot on the
pedal, and fast enough to A/B. This is the everyday operation.

**Flash — write to a pedal slot.** Store the preset in the pedal's own memory
and record which slot in `PedalPreset.savedSlot`. Thereafter the preset can be
recalled with a single Program Change instead of a burst of CCs.

### Diff from what?

An open decision with real consequences:

- **Diff from default** is correct but expensive: auditioning ten presets means
  ten baseline resets, each a full parameter send.
- **Diff from believed current** is cheap and fast but compounds any error in
  the belief.

Recommendation: **diff from believed current for auditioning**, with
baseline-and-full-send available as an explicit "make certain" action. Fast by
default, correct on demand. But this is the owner's call and it changes the UI.

### `savedSlot` is also a belief

Worth stating because it is easy to miss: the store's record of *which pedal
slot holds which preset* is subject to exactly the same problem one level up.
Flash preset Y over slot 3 and the store must be updated; save a preset from
the pedal's own controls and the store is silently wrong. 39 of the current 252
presets already carry a `savedSlot`, and none of those claims has been verified
against hardware.

### The fifth state: a slot we never captured

*Implemented 2026-08-15.* Some of the best sounds on the board were saved to a
numbered slot years ago, and some pedals will not give them back — the two Meris
units name their sixteen presets, but only inside the Meris editor. Requiring a
captured value map before a slot can be used is friction with nothing on the
other end of it: what a board or an MC6 switch needs is the number.

So a **slot reference** is a preset with an empty value map and a `savedSlot`
(`Data.Preset.isSlotRef`). It sits in the same library, the same board entries
and the same Program Change path as a captured preset. Two operations have to
check first: recall sends the PC rather than streaming nothing, and
overwrite/assign-slot are withheld, because both would claim we hold values we
do not.

This is the *inverse* of Flash. Flash writes our values into the pedal; a
reference admits the pedal's values were never ours.

### The reset, and why it does not live in the pedal

A "known good" per pedal is what re-anchors belief after any period the app was
not watching. The tempting implementation is a locked preset in the pedal's own
memory — the last slot, recalled with one Program Change.

**That reintroduces the problem it is meant to cure.** Recalling slot N and
concluding "the pedal is now at baseline" is a *belief*, resting on the slot
still holding what we flashed. The reset's entire job is to replace belief with
knowledge, and a stored preset cannot do that. Sending every baseline CC can:
afterwards we do not think the pedal is at those values, we know, because we
transmitted each one.

So the authoritative reset is **the baseline in `config/pedals/*.json`, sent as
a full CC sweep**. This is already what the owner asked for in every respect
that matters — it is per-pedal, it is versioned in Git, and it is genuinely
un-overwritable in a way no pedal slot is. MOOD's baseline is 45 CCs and covers
even the DIP-switch section, since Chase Bliss exposes those as CC 61–63.

It also works on Meris, which has sixteen slots and no spare one — a
slot-based reset is impossible there, and a mechanism that covers 11 of 13
pedals is not an anchor.

**A pedal-slot copy is still worth having, as a convenience.** One footswitch to
known-good when you are at the board without the app. That is a cache of the
authoritative baseline, not the source of it, and the honest way to build it is:
sweep the CCs (which is the prerequisite anyway — it is how the pedal gets to
baseline before you save), then save, then lock the slot so the app never
targets it.

Per-brand locations, as proposed:

| Brand | Reset slot | Reachable by |
|---|---|---|
| Chase Bliss | 122 (last) | plain Program Change |
| Strymon | bank 2, preset 99 (#299) | **Bank Select + PC — see below** |
| Meris | none available | CC sweep only |

**#299 is not currently expressible.** `makeProgramNumber` caps at 127 and
`savedSlot` is a bare program number, because a MIDI Program Change addresses
128 values. Reaching Strymon's third bank needs a bank message first; the MC6
has `MsgStrymonBankUp`/`MsgStrymonBankDown` for exactly this, so the hardware
side is solved, but `PedalPreset.savedSlot` would have to become bank+program,
with the codec and the MC6 message generation following.

Worth noting what that would cost if it spread: two messages per pedal instead
of one turns a twelve-pedal board recall from 12 messages into 24, over the
16-message ceiling of §5. It stays affordable only because the reset is a
deliberate standalone action and never part of a board recall — which is another
reason to keep the managed range (50–75) inside bank 0.

### Reserved slots, and the one thing Git cannot restore

The slot browser sweeps everything a Program Change can reach, because the
factory presets are most of the reason to browse. That makes it newly possible
to point the save ceremony at a slot that shipped with the pedal.

`rig.json` already separates the two questions — `range` is what the browser
sweeps, `managed` is where this app saves — which keeps the default safe on the
Strymons. It does not cover Chase Bliss, whose first two slots are factory
content inside a range with no managed sub-range, and it does not cover a slot
the owner simply does not want to lose.

**Planned: a lock, per slot, that the save ceremony refuses to target.**

Version control and the pedal cover different halves of the problem, and the
halves happen to be complementary. Git protects the *library* — names, values,
which slot we believe holds what — and restores any of it, but it has never held
the pedal's own memory and cannot put Strymon factory preset 3 back. A factory
reset does exactly the opposite: it restores the pedal's content and destroys
ours. So nothing here is truly unrecoverable; what a lock prevents is the
*recovery*, which means factory-resetting to retrieve one preset and then
re-flashing everything of ours from the store.

That puts this well below "data loss" in priority. The stronger argument is
simpler: these pedals hold hundreds of slots and Program Change reaches 128 of
them, so there is never a reason to aim a save at a low one. The convention is
avoiding a pointless collision, not rationing a scarce resource.

Which is why `managed` should stay modest rather than expand to fill the space.
**A flashed preset is a starting point, not a finished sound** — enough variety
to get into the ballpark on a given pedal, then tweaked by hand from there. Most
pedals have nowhere near seventy-eight distinguishable useful settings, so a
library that large would be a library nobody could choose from. Both MIDI brands
now reserve from 50 up (`Strymon` 50–75, `Chase Bliss` 50–122); the ceiling is
generous, the expectation is that it stays mostly empty.

(Strymon factory *values* are recoverable in principle without a reset: sweep a
control across its range and the Faves LED blinks green as it passes the stored
value. This is a per-parameter, eyes-on-the-pedal procedure with no MIDI
readback, so it cannot be automated and is only worth doing for a sound that
matters. Noted so nobody re-derives it.)

**Meris is a different case, and it is half solved.** `HedraEdit.app` and
`Mercury7Edit.app` are Kivy apps, and each ships its factory library as plain
JSON inside the bundle — names, descriptions, and every parameter by name.
Every one of those parameters maps onto a CC we already declare, so
`import-meris-library.js` turns them into thirty-six ordinary captured presets
(seventeen Hedra, nineteen Mercury7). Meris sounds now arrive with real names
and real values rather than as bare numbers.

What it does *not* give us is the pedals' own sixteen slots. The library is
what Meris shipped; the slots were filled by hand, years ago, from among those
sounds and others. The descriptions name a factory slot number and the import
deliberately does not turn that into a `savedSlot` — asserting slot 9 holds
`SubTerra` would be precisely the unfounded belief §2 exists to prevent. So
slot references remain the right mechanism for these pedals; they simply now
sit alongside a library worth auditioning.

Two details worth keeping. Meris stores every parameter twice, as a
ToeUp/ToeDown pair bracketing an expression sweep; we take the heel and drop
the toe, since this app has one value per CC. And the editors keep *no* user
library on disk — nothing under Application Support, Documents or Preferences —
so a patch you saved in the editor lives in the pedal and in the editor's own
window, and nowhere we can read.

Two shapes share the mechanism and should not be conflated:

- **Factory reservations** — static, brand-level, declarative. Belongs beside
  `managed` in `rig.json`, so it is versioned with the repo and identical for
  everyone with that pedal. Covers "the Strymon factory presets" and "the first
  two Chase Bliss".
- **Owner locks** — dynamic, per slot, set from the UI on a sound the owner has
  decided not to risk. Belongs in the store beside the presets.

Note that a lock here is *advisory*, and should be honest about that. This app
never writes a pedal's preset memory — no MIDI message it sends causes a save.
What it does is run the ceremony that tells the owner to hold the footswitch.
So the lock intercepts a human action, not a transmission, and its whole job is
to be read before the foot goes down.

### Keeping the belief synced: observe, don't mediate

*Confirmed empirically 2026-08-16.* The MC6 mirrors its pedal-bound messages to
USB as well as DIN — a footswitch programmed to send a MOOD CC shows up in the
app console. This was the open question behind two competing designs, and it
settles it in favour of the cheaper one.

**Rejected: mediation.** Program each switch to emit on an app-owned channel;
the app translates and forwards to the pedal. It syncs perfectly, but it makes
the app load-bearing — browser closed and the pedalboard is dead.

**Adopted: observation.** The MC6 keeps talking to the pedals directly. The app
listens to every channel and updates its belief from what it hears. Same sync
while the app is running, no added latency, no MC6 reprogramming, and the board
still works when the app does not. It also picks up switches programmed by hand
in the Morningstar editor, which mediation never would.

*Implemented 2026-08-16.* `MC6MidiReceived` now decodes every channel: a CC on
a pedal's channel updates that pedal's values, and a Program Change adopts the
values of the captured preset flashed to that slot — twelve knobs from one
message, the largest belief update available anywhere in the app. A slot
reference records only the number, since that is all we ever knew.

Two things it deliberately does not do. It never transmits: the pedal already
had the message directly from the MC6, so re-sending would be redundant at best
and a loop at worst — which is why it cannot go through `SetValue`. And it skips
writes whose value is already held, because every write re-renders and a board
recall arrives as a burst of a dozen messages.

The original text follows. `MC6MidiReceived` interpreted channel 1 (board
recall) and channel 13 (Itajara) and dropped the rest on the floor. The work was
to decode the rest
against the registry — every pedal declares its channel and its CCs, so the
mapping already exists.

Mediation now has **no default use**. It was expected to be needed for board
recall — one proxy CC expanding to twelve Program Changes — on the grounds that
a scene could not fit in sixteen messages and that a direct recall would break
the app's picture of the board. Neither holds: scenes generally fit (see the
corrected budget in §5), and the app observes a direct recall going past like
any other traffic. It stays available as a deliberate escape hatch for a scene
that wants to exceed the ceiling; channels 9 and 16 remain free for that.

Neither mechanism sees a knob turned by hand, and neither knows what happened
while the app was closed. That is what §3's reset is for: observation *keeps*
belief true, only a reset can *make* it true.

---

## 4. The MC6: two kinds of switch

The brief makes a distinction the current code partly has and should make
central.

**Live controls — the majority.** Instant on/off for things designed to be
operated by feet: MOOD's freeze, Habit's hold, and similar. These are the
reason the MC6 exists. `Data.MC6.ControlBank` already models this
(`ccToggleMessages`, `ccMomentaryMessages`).

**Scene recall — occasional.** A board preset: *these pedals, at these presets*.
`BoardPreset` plus `mc6Assignments` already models this, and it is the
thinnest-used part of the app — 5 board presets against 252 pedal presets.

v2 should treat these as peers rather than treating scene recall as the primary
thing. In practice most MC6 switches will be live controls.

---

## 5. The message budget, and why flashing exists

An MC6 MKII preset carries **at most 16 messages**. With twelve pedals that is
the binding constraint on what a board preset can express.

*Confirmed 2026-08-16* against the device's own backup: all 360 presets (30
banks x 12) carry exactly 16 message slots, so this is firmware rather than an
artefact of our reverse-engineering. The same file shows a **separate 16-slot
bank-level message array** — messages that fire on *entering* a bank, which the
pages design in `DESIGN-CONTROLS.md` has not yet spent. And for scale: the most
messages any hand-built preset on the device actually uses is eleven.

**The rule v2 adopts:** a board changes *at most one thing per pedal* — either
a bypass **or** a program change.

**Corrected 2026-08-16.** That rule was costed as "twelve pedals, twelve
messages, four spare". Wrong: a *dual-engage* pedal takes two CCs to bypass, and
four of the thirteen are dual-engage (Flint, Lost + Found, MOOD, Onward). One
thing per pedal across all twelve therefore costs up to **sixteen** messages,
and the bank jump makes seventeen. The discipline is right; the headroom does
not exist.

Compiled from the real library: `test board 2` costs 16 + jump and is **already
over**, `Testing board presets` sits exactly on 16, and the other three cost six
or fewer. So the ceiling is not theoretical, and `SysEx.purs` drops the overflow
silently via `Array.take 16` — a board that reads correctly on screen is missing
messages on the hardware.

**The headroom is recoverable.** Most or all of the two-channel pedals accept a
single message that bypasses the whole pedal, rather than one CC per channel.
`Flint` declares `CC 33 "Both"` beside its two channel toggles, and `MOOD`
declares `CC 55 "True Bypass"`; `Onward` and `Lost + Found` declare nothing, but
the Chase Bliss pedals share a MIDI implementation, so that is more likely a gap
in our transcription than a gap in the pedals. With a one-message bypass for all
four, `test board 2` drops from 17 to 14 and `Testing board presets` from 16 to
13 — comfortably inside the budget.

Two things to get right before treating it as free:

- **`config/pedals/*.json` is a partial transcription.** Habit's `Other` section
  has two entries; these files were written for the controls that mattered at
  the time, not as complete MIDI specs. Verify against the manuals.
- **True bypass is not "both channels off".** Dropping the relay takes the pedal
  out of the path and cuts trails; turning both channels off leaves it buffered
  and may let them ring. For a board that *establishes* a starting point the
  former is usually right, but they are different sounds and the config should
  not conflate them.

The mechanism: `DualEngage { a, b }` gains an optional `both :: Maybe CC`, and
the compiler emits the single message where it is declared and two where it is
not — so an unverified pedal stays correct-but-expensive rather than wrong.
*Done 2026-08-16*, declared for Flint and MOOD. Onward and Lost + Found appear
to have no such message, so they remain at two.

**The escape hatch, if a board still will not fit.** Treat the *tone* pedals —
Iridium, Riverside, Clean — as a separate concern from the rest, and let a board
address one group or the other. A board that leaves the tone stack alone is
three to five messages cheaper, and the split is musically natural: the tone
pedals are what the guitar sounds like, the others are what happens to it.

This is what makes flashing (§3) necessary rather than merely an optimisation.
Sending a preset as parameters costs one message per changed CC, which blows
the budget on a single pedal. Recalling a flashed preset costs one Program
Change. The 16-message ceiling is the reason the pedal's own memory has to be
used at all.

### Boards compile, and compilation can fail

The useful framing: **a board preset is a source program; the MC6 preset is
compiled output**, with a hard budget of 16.

That means compilation has a result, and the result can be *too big*. The UI
should say so at programming time — "this board needs 19 messages" is a far
better error than a switch that half-works on stage. Nothing in the current
code checks this.

---

## 6. What comes out

LoopyPro integration is removed. Looping and recording move to Ableton or a
hand-rolled Mac looper against the Audio4c.

| Removed | Lines |
|---|---|
| `Data/Loopy.purs` | 438 |
| `Component/Loopy/{Panel,ClipSettings}.purs` | 473 |
| `Foreign/LoopyProject.{purs,js}` | 521 |
| Scattered, mostly `App.purs` | ~150 |

**But `Data.Loopy` is not integration code.** It is a working model of looper
semantics — `LoopPhase` with a transition function, count-in and count-out
modes, record-end actions, beat quantisation, clip settings. It was written to
*predict* Loopy Pro's behaviour, which required understanding it well enough to
reimplement. Keep it, renamed, as the specification for our own looper. The CC
numbers are Loopy's; the state machine is ours.

Also removed: the MC6 **input** path and the footswitch relay, per the brief's
"no connection back from the MC6". That is where most of the 108 `App.purs`
LoopyPro references live.

`Foreign/ClipDiagram.js` is a clip visualisation currently in Loopy's service —
probably worth keeping for the looper.

---

## 7. Open questions

1. **Diff from default or from believed current?** §3. Changes the UI.
2. **How is a baseline asserted?** Per pedal, or a whole-board "set everything
   to default" sweep? The latter is one action but a long burst of CCs.
3. **What happens to the 39 existing `savedSlot` claims?** They are unverified
   beliefs about hardware. Trust them, or invalidate and re-flash?
4. **Does the Twister stay?** Not mentioned in the brief either way, and it is
   a live-control surface that overlaps with the MC6's job.
5. **Is a board preset allowed to be partial** — addressing only some pedals,
   leaving the rest as they are? The budget permits it and it is probably more
   useful than an all-twelve scene.
6. ~~**Where do factory reservations end?**~~ Resolved: don't declare them. The
   app cannot know which slots shipped with content, the loss is recoverable by
   factory reset, and with hundreds of slots there is no reason to save low
   anyway. Ship the lock mechanism empty and let the owner lock what they
   actually care about. §3.
