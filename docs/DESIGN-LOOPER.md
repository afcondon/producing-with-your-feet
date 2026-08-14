# The looper — design notes

**Status:** brainstorm written up, 2026-08-13. For reaction before anything is
built. Companion to `DESIGN-v2.md`, which removes the LoopyPro integration this
replaces.

---

## 1. Scope

The target is **not** a Loopy Pro clone. It is *a little better than an EHX 720*
— which is a much smaller and much more achievable thing, and the reduction is
the most useful decision made so far.

The 720 is: stereo, one loop, unlimited overdubs, undo/redo, twelve minutes,
one LED. Beating it means:

- a master cycle, so parallel loops can be locked to each other
- multipliers — record two bars, then a few taps later be recording eight with
  the two playing underneath
- overdubs as layers, with real undo
- half-speed and reverse
- start-time nudging
- and, mostly, **being able to see what is going on**, which is where the real
  win is

The precedent is Hedra and the Chase Bliss pedals in this app: the improvement
did not come from adding features, it came from showing resolved musical
meaning instead of raw control positions. A looper is a stronger candidate for
that treatment than a pedal is, because a looper's state is *entirely* hidden.

---

## 2. Prior art worth reading

**The Echoplex Digital Pro (Gibson/Oberheim, 1994–2000s).** Fifteen years of
thought about exactly the multiplier gesture described above. `Multiply` is
that gesture: press it during playback, play over four cycles, press again, and
it rounds to the cycle boundary and hands back a four-times-longer loop with the
original inside. `Insert`, `Substitute`, `Replace`, `Undo` and the SUS
(hold-for-momentary) variants complete a function set that has not really been
improved on. Most Max for Live looping devices, and Mobius, are
re-implementations of it. **Read the EDP manual before designing a function
set** — it is free and it is the single best piece of prior art.

**The Electrix Repeater (2001).** Four tracks, rackmount, loops stored to
CompactFlash. Its headline was **independent time and pitch per track** — record
at one tempo, play back at another, with no chipmunk — and it had a `slip`
control for nudging tracks against each other, which is Loopy's start-time nudge
twenty years early. Electrix folded around 2003 and nothing quite replaced it.

What to take from the Repeater is the **panel, not the DSP**: one column per
track, every track's state visible simultaneously, dedicated controls, no menu
diving. Its reputation was earned by not being modal. Decoupled time and pitch
is also worth having — it turns half-speed from a side effect into a musical
choice, and on layered buffers it is cheap.

---

## 3. The clock: the first loop defines the cycle

The multiplier requirement implies bars, and bars imply a tempo — but a count-in
before playing is exactly the friction the 720 does not have. The resolution:

> **The first loop recorded defines the cycle. Everything after is an integer
> multiple of it.**

Free-form capture for loop one; strict scheduling for everything after. This is
the EDP architecture and it dissolves the awkward choice between a
Link-quantised clip machine and a free-form set-the-length-by-playing machine:
the hard part of free-form applies only to the first loop, and the hard part of
scheduling applies to everything else against a grid that is already known.

**A tempo falls out for free**, and `link-spike` is already in the rig. So the
looper can *publish* the derived tempo — Ableton, purerl-tidal and the modular
all learn the tempo from two bars of guitar. **The looper becomes the rig's
clock master.** Nothing commercially available does that; it is available here
only because the surrounding infrastructure already exists.

---

## 4. Layers, never mixdown

Every overdub is a separate buffer, mixed at playback. Cheap loopers mix down
because they have four megabytes of RAM; a Mac has no such excuse.

What this buys:

- **unlimited undo**, free
- muting or soloing one layer
- half-speeding or reversing *one* layer while the rest plays forward
- re-rendering a single layer (§6)

It is also the principle already adopted in `Triggerfish.Clips`: **store
everything, flatten late**. Consistency here is worth something — the same rule
should hold across the rig.

Cost: eight bars of stereo float32 at 48k and 120bpm is about 6 MB per layer.
Irrelevant.

---

## 5. Always be recording

Keep a rolling pre-roll ring buffer. Sixty seconds of stereo float32 is ~23 MB —
nothing on a Mac, impossible on a pedal. Two things fall out, and the second is
the best single idea in this document.

**Backward quantisation.** A late tap can be snapped to the beat *behind* it, so
the loop starts on the beat with the attack intact rather than clipped. This
makes Loopy's start-nudge unnecessary rather than reimplementing it.

**Retroactive capture.** You played something good and did not hit record. Hit
it afterwards and take the last eight bars. No pedal can do this. It is the
feature most likely to change how the thing gets used, and it is nearly free
once the ring buffer exists.

---

## 6. What a layer is: provenance

The intended workflow is guitar → MIDI Guitar on the iPad → softsynths on the
iPad → audio back to the Mac. So **the primary artefact is audio**, with MIDI
kept alongside for editing and re-doing.

That means MIDI is not a peer track type. It is **provenance attached to a
layer**:

| Field | Presence |
|---|---|
| audio buffer | always |
| source `MidiClip` | when the layer came from MIDI-triggered synths |
| believed board state | when the layer came through the pedals |

One self-describing unit. Every layer knows how it was made.

This de-risks the MIDI work substantially: it is **off the critical path**. If
the tracking is ragged or the capture drops entirely, the loop is unaffected. So
the audio looper can be built first and MIDI capture added later without
re-architecture — which is the right order regardless.

### Re-render

Once a layer knows its own notes, the operation that matters is **re-render**:
play the stored MIDI back out to the iPad, record the audio return, replace the
layer.

- change the patch → same performance, different sound
- quantise the notes → the loop tightens
- transpose → the loop moves

This is DAW-grade editing coming out of a looper, and it is the thing the
Repeater never had.

The constraint: the synth is on the iPad and runs in real time, so **a re-render
costs exactly one loop length**. Eight bars is eight bars. That is acceptable —
it just makes re-render a deliberate action with a progress state ("re-rendering,
6.2s") rather than an instant undo, and it means several layers should be
queueable as a batch.

### The board state is provenance too

The pedals are in the record path for guitar layers, so **a loop is a recording
of a belief** (`DESIGN-v2` §2). Storing the believed board state with the layer
makes "what was I doing when I recorded that?" answerable, and "put the board
back the way it was for this loop" a single action.

No standalone looper can do this, because no standalone looper knows about the
pedals. It also gives the belief model a second job, which makes it easier to
justify.

### Control tracks, later

The app knows every CC it sends. So it can record its own outgoing control
stream as a timed track and replay it against the cycle: loop the guitar, and
loop the knob moves made over it. Not for v1, but the architecture should not
preclude it.

---

## 7. Where it runs

**Not in the browser.** Chrome's `getUserMedia` gives one device with ambiguous
channel mapping and AGC to fight, multi-channel input is unreliable, and a GC
pause is a click in the loop.

The pattern already exists in this ecosystem twice over. **A small Rust daemon on
`cpal`, opening the Audio4c directly, exactly like `es9-daemon`, with the
PureScript app as its UI over WebSocket.**

| Owner | Responsibility |
|---|---|
| daemon | buffers, sample clock, latency compensation, the ring buffer, playback |
| app | UX, MC6 mapping, provenance, the store |

Consequences worth having: the loop survives the browser being closed, the MC6
can drive the daemon with the app absent, and the daemon can speak to
`link-spike` directly for the clock-master role in §3.

---

## 8. Signal flow

Split the guitar at the **very front** — pitch detection collapses on anything
reverbed or pitch-shifted, so the tracking feed must be pre-pedals.

```
             ┌─▶ Audio4c in 1 ──USB──▶ iPad ──▶ MIDI Guitar ──▶ softsynths
  guitar ────┤                            │                          │
             │                            └──── CoreMIDI ──┐    USB audio
             └─▶ pedalboard ──stereo──▶ Audio4c in 3/4      │         │
                                                            ▼         ▼
                                           Mac: looper daemon ◀───────┘
```

The Audio4c has two USB-C host ports and inter-host routing is its entire reason
to exist, so the iPad leg should be configuration (iConfig/Auracle) rather than
cables. Four inputs covers this with one spare.

**Note that the synth path does not go through the pedals at all.** The looper is
therefore capturing two quite different sources, which forces a decision the UI
must expose: does a layer record *a source* or *the mix*? (§12.)

---

## 9. Don't buy MIDI Guitar for the Mac

Not for this rig. If the softsynths live on the iPad, tracking belongs on the
iPad too — shortest path, and the licence is already owned. Moving tracking to
the Mac would mean sending MIDI *to* the iPad and getting audio back: the same
round trip plus a hop, and a second latency to calibrate. The Mac version only
pays off if the sound source moves to the Mac.

Which is worth flagging only because **the Mac-side version already exists in
this ecosystem**: `continuo` hosts a single AU or VST3 headless and plays
incoming MIDI through it to CoreAudio. A sound living in continuo would put
tracking, synth and looper in one clock domain, and re-render would stop being a
real-time device round trip.

Not a recommendation — the iOS AUv3 catalogue is presumably why the synths are
there. But it is the version of this that gets dramatically cleaner, and it
would only be needed for whichever sounds get re-rendered most.

---

## 10. Latency, and what has to be measured

Three separate numbers, none of which should be guessed:

**Audio round trip** (in → out through the interface). A loop recorded through
the interface lands late by the full round trip, and **the error compounds on
every overdub** if uncompensated. This is the one that ruins a looper.

**Pitch-detection latency.** Roughly 10–20 ms up high, worse on the low E.
Notes have timestamps, so unlike audio this is fully fixable after the fact:
shift the capture backwards by the measured offset and it lands where it was
actually played. Possibly worth measuring per string range.

**Synth-return offset.** The MIDI copy reaches the Mac *ahead* of the audio it
produced, by the synth's rendering latency plus the return trip — and USB MIDI
is not sample-accurate anyway. Since the MIDI is a score rather than a
synchronised layer, small errors are cosmetic; but **re-render needs the number**
to line new audio up with old.

All three belong in the calibration tables `DeepStar` already writes and the
realisers already read, alongside the V/oct table. That is exactly what that
machinery is for.

---

## 11. The MC6 surface, and the UX thesis

The defect in every looper, the 720 included, is that **one switch means four
things depending on invisible state**, so you press and hope.

The thesis: **the app always shows what each switch will do right now, and when
it will take effect.**

> `SW2  Multiply → ends at cycle 4, in 2.1 s`

That is the Hedra move again — stop showing controls, show the resolved
consequence. It is also why the looper wants the app even though the daemon can
run without it.

Provisional switch set for one MC6 bank (six switches plus three combinations,
sixteen messages — see `DESIGN-v2` §5):

| Switch | Function |
|---|---|
| 1 | Record / Overdub — context-dependent, and *labelled* with its current meaning |
| 2 | Multiply |
| 3 | Undo (layer) |
| 4 | Loop select |
| 5 | Stop / Clear (long-press) |
| 6 | Reverse / half-speed |

Comfortably inside the message budget.

---

## 12. Display

Two views, both wanted, neither sufficient alone:

**Concentric rings** sharing one phase pointer. The eight-bar ring visibly
containing four sectors of the two-bar ring makes the multiplier relationship
*structural* rather than a number in a box. This is the view that answers "where
are we and when does the next thing happen".

**Per-track columns**, Repeater-style — every layer's state visible
simultaneously, level, source, length multiple, reverse/speed, mute. This is the
view that answers "what have I got".

`Foreign/ClipDiagram.js` is a clip visualisation currently in LoopyPro's service
and is probably reusable for one or both.

---

## 13. Minimum viable

Ordered so that each stage is usable on its own:

1. **Daemon + one loop.** Record, play, overdub as layers, undo. Beats nothing.
2. **Ring buffer.** Backward quantisation and retroactive capture. Beats the 720.
3. **Master cycle + Multiply.** Parallel loops locked to the first. Beats most
   pedals.
4. **Reverse, half-speed, decoupled time/pitch.**
5. **Link publish** — the looper as clock master.
6. **MIDI provenance + re-render.**
7. **Board-state provenance.**

Stages 1–3 are the project. Everything after is upside.

---

## 14. Open questions

1. **Parallel loops: slaved or independent?** Slaved (loop B is 1×, 2× or 4× of
   loop A, phase-locked) is simpler, matches the multiplier gesture, and
   preserves the derived clock. Independent (Loopy Pro) is more expressive,
   much harder, and quietly discards §3. **Recommendation: slaved.**
2. **Does a layer record a source, or the mix?** §8 — the synth path and the
   pedal path are different signals and both are wanted.
3. **What is the Undo granularity?** Whole layer, or the EDP's "long undo" that
   can retract part of a pass?
4. **Does the daemon need to run without the app at all**, or is app-present the
   assumed case? Affects how much state lives where.
5. **Loop storage.** Layers to disk as WAVs plus a manifest, into `pwyf-store`?
   That would also be the cleanest route into Ableton, and would make BlackHole
   unnecessary.

---

## 15. To verify before designing further

- **Does MIDI Guitar on iOS publish notes to CoreMIDI** as well as to its
  internal synth? The whole provenance idea in §6 rests on it.
- **Can the Audio4c routing matrix** carry the dry guitar to the iPad host and
  the synth audio back to the Mac host simultaneously? Two-host routing is the
  unit's purpose, so this should be configuration — but prove it early rather
  than discover a limitation late.
- **Measure the audio round trip** before writing any overdub code. §10.
