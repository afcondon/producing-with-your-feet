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

**The rule v2 adopts:** a board changes *at most one thing per pedal* — either
a bypass **or** a program change. Twelve pedals, twelve messages, four spare.

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
