# The looper — design notes

**Status:** revised 2026-08-15. First written 2026-08-13 as a brainstorm; stages
1–3 of the original plan are built and working. This revision changes the
framing in three ways that invalidate parts of the first draft:

- the target is **recording**, not live performance
- Itajara becomes **a pedal in the registry**, not a separate application
- the AUDIO4c is **part of the instrument**, not just the way audio gets in

Companion to `DESIGN-v2.md`, which removes the LoopyPro integration this
replaces.

---

## 1. Scope

The original target was *a little better than an EHX 720*, and that reduction
did its job — it got a working looper built in two days. The target now moves,
for a reason that was not stated in the first draft: **this is for recording,
not for playing out.**

That changes what matters. Nothing is timing-critical in the way a stage
demands: a missed Multiply costs a retake, not a song. What matters instead is
everything the first draft treated as upside — layers that stay separable,
provenance, re-render, export, and the ability to reach in afterwards and change
what you did.

So the reference moves from the 720 to the **1440**, and past it. What the 1440
adds over the 720 is *multiple stored loops rather than one*, and that — plus
per-layer control — is the shape of the thing.

The 720 is: stereo, one loop, unlimited overdubs, undo/redo, twelve minutes, one
LED. Beating it still means everything the first draft listed:

- a master cycle, so parallel loops can be locked to each other
- multipliers — record two bars, then a few taps later be recording eight with
  the two playing underneath
- overdubs as layers, with real undo
- half-speed and reverse
- start-time nudging
- and, mostly, **being able to see what is going on**, which is where the real
  win is

The precedent is Hedra and the Chase Bliss pedals in this app: the improvement
did not come from adding features, it came from showing resolved musical meaning
instead of raw control positions. A looper is a stronger candidate for that
treatment than a pedal is, because a looper's state is *entirely* hidden.

**What the first draft got wrong:** it argued against a Loopy Pro clone. The
reduction was right at the time, but the destination is closer to Loopy than
that framing allowed — per-layer on/off, sequences of loops, a page you can
really go to town on. The distinction that survives is *where the depth lives*:
a simple pedal face on the board, and a dedicated page for the deep work.

---

## 2. The load-bearing decision: Itajara is a pedal

Everything else in this document is downstream of one choice.

> **Itajara is an entry in the pedal registry**, with a MIDI channel, a CC map
> and a layout — exactly like Habit or MOOD. The only thing that distinguishes
> it is that its CCs are routed to a WebSocket rather than a MIDI port.

That is a one-branch change in `SetValue`, and it buys the following for free,
because the app already has the machinery:

| Capability | Why it comes free |
|---|---|
| MC6 assignment to any switch, any bank, any combination | the assignment UI does not care what a CC talks to |
| Robustness to physical reconfiguration | MC6 on the floor, FS3Xs on the floor, MC6 on the desk — all of it is just which switch sends which CC |
| Board presets capture loop state | a board becomes "these pedals, this bypass state, **and** layer 3 muted" |
| Twister control of loop levels | the Twister maps to CCs |
| Any control assignable to any MC6 switch from the Controls page | that page indexes the whole registry, so Itajara joined it for free |

**But not in the Overview grid.** Being a pedal in the *model* does not mean
being a cell in the board view. The other twelve are settings you arrange and
leave; the looper is a live surface you operate. It keeps its pill — clicking it
opens the Looper page rather than selecting a cell — and its face renders there,
beside the transport.

That distinction is admittedly soft: MOOD, Habit and Onward are also live
surfaces (§6). Which is an argument for **two board views eventually** — the
loop-y things and the mod-y things — rather than for putting the looper back in
the grid.

**The routing detail that makes this work.** Every other pedal receives its CCs
from the MC6 *directly over MIDI* — the app is not in that path. Itajara has no
MIDI hardware, so its CCs must reach the app. Giving it a dedicated channel and
relaying **any CC on that channel** to the socket is general, requires no table
of gestures in `App.purs`, and means anything that can send MIDI — MC6, Twister,
a keyboard — drives the looper without new code.

Channels 2–8, 10–12, 14 and 15 are taken by the pedals; channel 1 is taken by
the app's own MC6 board-recall relay. **Channels 9, 13 and 16 are free.** This
document assumes **13**.

This supersedes the first implementation, which special-cased four gesture CCs
on channel 1 (`looperBaseCC`, `gestureFromCC`). That was a scaffold and should
be removed.

---

## 3. Prior art worth reading

**The Echoplex Digital Pro (Gibson/Oberheim, 1994–2000s).** Fifteen years of
thought about exactly the multiplier gesture described above. `Multiply` is that
gesture: press it during playback, play over four cycles, press again, and it
rounds to the cycle boundary and hands back a four-times-longer loop with the
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

## 4. The clock: the first loop defines the cycle

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

## 5. Layers, never mixdown — and stereo-capable, not stereo-mandatory

Every overdub is a separate buffer, mixed at playback. Cheap loopers mix down
because they have four megabytes of RAM; a Mac has no such excuse.

What this buys:

- **unlimited undo**, free
- muting or soloing one layer
- half-speeding or reversing *one* layer while the rest plays forward
- re-rendering a single layer (§8)
- a **source per layer** (§7)

It is also the principle already adopted in `Triggerfish.Clips`: **store
everything, flatten late**. Consistency here is worth something — the same rule
should hold across the rig.

### Width is a projection, so it belongs at playback

Layers are mostly stereo — ping-pong delays and the Lex's spaciousness are the
point of the board, and flattening them would be vandalism. But **store each
layer at its source's native width** and treat pan and width as *playback*
properties rather than baking them in.

That is not storage thrift. It is the thing a modern stereo pedalboard makes
hard and this makes easy:

> Record the dry mono guitar as a layer, place it hard left. Send that layer out
> to the board, record the reverb return as a **separate** layer, place it hard
> right. You have built the 1970s dry-one-side / wet-the-other production trick
> out of a stereo pedalboard — because the placement happens in the looper
> rather than in the signal chain.

Which promotes re-amping (§7) from an occasional workaround to *the mechanism*
for a sound that is otherwise unavailable on this rig.

It also stays honest to "flatten late": width is a projection, and projections
belong at the end.

Cost: eight bars of stereo float32 at 48k and 120bpm is about 6 MB per layer.
See §15 for what happens when loops get long.

---

## 6. Always be recording

Keep a rolling pre-roll ring buffer. Two things fall out, and the second was the
best single idea in the first draft.

**Backward quantisation.** A late tap can be snapped to the beat *behind* it, so
the loop starts on the beat with the attack intact rather than clipped. This
makes Loopy's start-nudge unnecessary rather than reimplementing it.

**Retroactive capture.** You played something good and did not hit record. Hit
it afterwards and take the last eight bars. No pedal can do this. It is the
feature most likely to change how the thing gets used, and it is nearly free
once the ring buffer exists.

### The ring should capture every input, not one

Now that the source matrix exists (§7), the ring buffer should hold **all eight
input channels**, not the selected one. Then:

> **Take can choose its source retroactively.** You play a pass with the board
> and the iPad synth both live; afterwards you decide you want the synth take —
> or both, as two layers.

That is the same "decide later" move as Habit and MOOD, extended across *sources*
instead of only across time. Sixty seconds of eight channels at float32 is about
92 MB. Nothing.

### The family this belongs to

Itajara is not a new kind of device on this board. It is the fourth member of a
family that is already there, and the resemblance is structural rather than
poetic — all four are **always recording, and let you decide afterwards**:

| | timescale | the gesture |
|---|---|---|
| Onward | fragments | grab and mangle what just passed |
| MOOD | a few seconds | the micro-looper is always capturing |
| Habit | ~30 seconds | scan back into the collector |
| **Itajara** | minutes, layered | Take, from the pre-roll ring |

Itajara's pre-roll *is* MOOD's left side, at a longer timescale. Which suggests
an organising principle for the page: **a common time axis**, with all four laid
out by how far back each can reach. That is a real fact about this board that no
manufacturer's UI could show, and it is the Hedra move applied to time instead
of pitch.

---

## 7. The source matrix

The AUDIO4c is not just how audio arrives; it is part of the instrument. The
daemon can see **eight input channels**:

| Host channels | Source |
|---|---|
| 0–3 | the four physical jacks |
| 4–7 | the second-host path — **the iPad's output, with no cabling** |

Measured; see §11. The consequence worth stating plainly: **MIDI Guitar on the
iPad driving softsynths arrives as a normal input.** No re-patching to switch
between "record the board" and "record the synth".

### Source is a per-layer property

Chosen at record time, stored with the layer:

- layer 1: the pedalboard, stereo
- layer 2: the iPad synth, stereo
- layer 3: the dry DI, mono, placed left

Stacked in one loop, separable forever. That is what "never mixdown" was already
promising; the interface gave it teeth. **"Both" becomes a third answer** to
"which thing is looping" — two layers laid down in one pass from different
sources.

### The send, and what it makes possible

A **pre-fader, pre-mute** send: the tap is a layer's raw samples, before level,
mute and pan. Pre is not a detail — it is what makes the workflow work:

1. record dry guitar → layer 3
2. **mute layer 3** in the mix; you do not want to hear the dry
3. send layer 3 out to the board (or the iPad), still at full level
4. record the return as layer 4, place it where you like

Post-fader would have made step 2 kill step 3.

Three uses, and the second and third were not in the first draft at all:

**Re-amping.** Above. The mechanism for §5's stereo trick.

**The pedalboard as outboard FX for the iPad**, with no guitar involved. Send an
iPad synth layer out through the board and record what comes back. The board
stops being an input chain and becomes a processor the looper can address.

**The pedalboard as outboard FX for the modular**, via the Instruō Larchd and
Cuir for level interfacing. Same mechanism, different patient.

Generalising: once there is a send and a source matrix, **the looper is a
patchbay with memory**. That is a materially bigger idea than "a looper", and it
is the one that justifies the dedicated page.

### Splitting the guitar

Split at the **very front** — pitch detection collapses on anything reverbed or
pitch-shifted, so the tracking feed must be pre-pedals.

```
             ┌─▶ Audio4c in 1 ──USB──▶ iPad ──▶ MIDI Guitar ──▶ softsynths
  guitar ────┤                            │                          │
             │                            └──── CoreMIDI ──┐    USB audio
             └─▶ pedalboard ──stereo──▶ Audio4c in 3/4      │         │
                                                            ▼         ▼
                                           Mac: looper daemon ◀───────┘
```

The iPad leg is configuration rather than cables, since inter-host routing is
the unit's entire reason to exist. Note that **capture needed no Auracle
configuration at all**; the send direction does. See §16.

---

## 8. What a layer is: provenance

The intended workflow is guitar → MIDI Guitar on the iPad → softsynths on the
iPad → audio back to the Mac. So **the primary artefact is audio**, with MIDI
kept alongside for editing and re-doing.

That means MIDI is not a peer track type. It is **provenance attached to a
layer**:

| Field | Presence |
|---|---|
| audio buffer | always |
| native width | always |
| source (which input, §7) | always |
| source `MidiClip` | when the layer came from MIDI-triggered synths |
| believed board state | when the layer came through the pedals |
| send lineage | when the layer is a re-amped return of another layer |

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
it just makes re-render a deliberate action with a progress state
("re-rendering, 6.2s") rather than an instant undo, and it means several layers
should be queueable as a batch.

Note that **re-render and re-amp are the same machinery** pointed at different
things: send something out, record what comes back, replace or add a layer. They
should share an implementation.

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

## 9. Where it runs

**Not in the browser.** Chrome's `getUserMedia` gives one device with ambiguous
channel mapping and AGC to fight, multi-channel input is unreliable, and a GC
pause is a click in the loop.

The pattern already exists in this ecosystem twice over. **A small Rust daemon on
`cpal`, opening the Audio4c directly, exactly like `es9-daemon`, with the
PureScript app as its UI over WebSocket.**

| Owner | Responsibility |
|---|---|
| daemon | buffers, sample clock, latency compensation, the ring buffer, playback, the send |
| app | UX, MC6 mapping, provenance, the store |

Consequences worth having: the loop survives the browser being closed, the MC6
can drive the daemon with the app absent, and the daemon can speak to
`link-spike` directly for the clock-master role in §4.

**The daemon opens no MIDI port, by design.** The app is the MIDI hub, so
exactly one process talks to the MC6 and exactly one place decides what a press
means. §2's channel-13 relay is how looper CCs get from there to here.

---

## 10. Don't buy MIDI Guitar for the Mac

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

## 11. Latency, and what has to be measured

Three separate numbers, none of which should be guessed:

**Audio round trip** (in → out through the interface). A loop recorded through
the interface lands late by the full round trip, and **the error compounds on
every overdub** if uncompensated. This is the one that ruins a looper.

> **Measured, 2026-08-14.** Audio4c, output 1 patched to input 1, 48 kHz,
> `itajara sweep`. Two results, and they are of different quality.
>
> **The form is exact and reproducible.** A single reading confuses two things,
> so the sweep varies the buffer to separate them:
>
> ```
> measured = residual − 2.00 × buffer_frames
> ```
>
> exact at 64, 128, 256, 512 and 1024 frames, with zero variance at each. The
> slope is two buffers — one on each side — because cpal's timestamps
> over-account for the output pipeline and the input pipeline alike. That part
> is bookkeeping, it is always exactly 2, and it cancels once the buffer size is
> known, which the engine always knows because it chooses it.
>
> **The residual depends on what else has the device open.** Confirmed by A/B on
> the day:
>
> | other clients | residual |
> |---|---|
> | Ableton running | 301 samples |
> | Ableton quit | **252 samples** |
>
> The whole curve shifts by 49 samples (~1.02 ms) while the slope stays at
> exactly 2.00 and the residual stays perfectly buffer-independent. Within a
> given configuration it is stable to the sample across dozens of fresh stream
> sessions; it steps rather than drifts. SuperCollider was running throughout
> both readings and is not a factor — only Ableton moved it. The mechanism is
> CoreAudio renegotiating the device's hardware buffer and safety offset when a
> client opens it.
>
> **So the number is session state, not a stored constant.** The engine must
> measure it for the configuration it is actually running in, and treat a stored
> value as a hint to be verified rather than a fact. A calibration that has gone
> stale is worse than none, because it is silently wrong in the one direction
> nothing in the sound reveals.
>
> Two operational consequences follow, and neither is optional:
>
> - **Calibrate with the rig as it will actually be**, not in a clean room.
>   Ableton open is the normal condition for recording, so a figure measured
>   without it is the wrong figure — the clean-room number is the misleading one
>   here.
> - **Watch for the change rather than assume it.** CoreAudio publishes device
>   property-change notifications; a daemon that holds the device can hear
>   another client arrive and mark its calibration suspect instead of carrying
>   on with a number that quietly stopped being true.
>
> Two other consequences. Raw timestamp arithmetic needs
> `true_offset = measured + 2 × buffer_frames` before it means anything. And a
> negative raw reading is normal rather than a fault — any path shorter than two
> buffers reads below zero.
>
> **The pedalboard path matters much less than first thought.** Two reasons,
> and the second is the general one.
>
> The pedals on this board have analogue dry-through, so the transient a click
> triggers on is not converted at all. Only wet content is delayed, and wet
> content being delayed is the effect rather than an error.
>
> More importantly, **constant pedal latency cancels**. Suppose a pedal adds
> 3 ms. Layer one: you play, it arrives 3 ms late, it is recorded 3 ms late.
> Layer two: you hear layer one, you play along, and your note is *also* 3 ms
> late. Both layers carry the same offset, so they agree with each other. It
> neither compounds nor misaligns; it shifts everything uniformly, which is
> inaudible.
>
> The residue is small and specific: a pedal with no analogue dry path (an amp
> modeller, say) engaged for one layer and bypassed for another puts those two
> layers a few milliseconds apart. Worth knowing as a number; not worth
> compensating, and arguably part of the sound.
>
> **This cancellation argument does not survive the send (§7).** See §20.

**The self-test this affords.** Because the correction is exact, the engine has
a check it can run on itself: record a loopback click against a playing loop and
it must land on the sample it was emitted on. Any error is a bug in the
alignment, not an unknown of the hardware. It is the only part of a looper that
can be verified rather than judged by ear — and building it first immediately
earned its keep, because it found the following.

### The two clocks — `itajara align`, 2026-08-14

**The interface's sample clock is not the host clock.** On this rig they differ
by about **15.6 ppm — 0.75 samples every second**. Measured as a dead-straight
line:

| elapsed | host clock vs device frames |
|---|---|
| 4 s | −3 samples |
| 12 s | −9 samples |
| 24 s | −18 samples |
| 48 s | −36 samples |

The first implementation converted each input buffer's capture timestamp into a
loop position using the host clock, and its alignment error tracked that column
exactly. A three-minute loop would have ended up 135 samples out; an hour's
session, 2700. Nothing about it announces itself — it is precisely the failure
mode described at the top of §11, arriving by a route that had not occurred to
me.

**The rule that follows: no host-clock arithmetic survives past startup.** Both
streams are driven by the same device clock, so their frame counters advance in
lockstep forever and differ only by a constant:

```
out_frame = in_frame + K

K = (C0 − P0) × rate − in_frames_so_far − offset_samples
offset_samples = residual − 2 × buffer
```

`K` is computed once, at the first input callback, and is the only place the
host clock is consulted at all. After that it is integer addition, and it cannot
drift.

**Verified:** alignment error `+0` samples at 4 s, 24 s and 48 s, while the host
drift over the same runs grew to −37. The engine now measures that drift and
ignores it, which is the correct relationship to have with it.

This generalises past the looper. Anything correlating captured audio with
played audio — the MIDI provenance of §8, the re-render alignment, any future
conformance test — must pair frame counters rather than timestamps.

**Pitch-detection latency.** Roughly 10–20 ms up high, worse on the low E. Notes
have timestamps, so unlike audio this is fully fixable after the fact: shift the
capture backwards by the measured offset and it lands where it was actually
played. Possibly worth measuring per string range.

**Synth-return offset.** The MIDI copy reaches the Mac *ahead* of the audio it
produced, by the synth's rendering latency plus the return trip — and USB MIDI
is not sample-accurate anyway. Since the MIDI is a score rather than a
synchronised layer, small errors are cosmetic; but **re-render needs the number**
to line new audio up with old.

All three belong in the calibration tables `DeepStar` already writes and the
realisers already read, alongside the V/oct table. That is exactly what that
machinery is for.

---

## 12. The control surface

Channel 13. Momentary means 127 acts and 0 is ignored, so a footswitch's release
message is harmless.

### Transport

| CC | Control | Type |
|---|---|---|
| 1 | Record / Overdub | momentary |
| 2 | Multiply | momentary |
| 3 | Undo | momentary |
| 4 | Redo | momentary |
| 5 | Take | momentary |
| 6 | Clear | momentary |
| 7 | Play / Stop | momentary |
| 8 | Reverse — all | toggle |
| 9 | Half speed — all | toggle |

### Source and routing

| CC | Control | Type |
|---|---|---|
| 20 | Record source | segmented — board L/R, jacks 1–4 mono, iPad L/R, multi |
| 21 | Record width | toggle mono / stereo |
| 22 | Monitor source | segmented |
| 23 | Monitor level | continuous |
| 24 | Send destination | segmented — output pair |
| 25 | Send source | segmented — layer / loop / live |

### Layers

Direct banks for the performative controls; everything less frequent hangs off
the selected-layer pointer, which is also the "step through the stack" gesture.

| CC | Control | Type |
|---|---|---|
| 40–47 | Layer 1–8 mute | toggle |
| 48–55 | Layer 1–8 level | continuous |
| 60 | Selected layer | continuous 0–7 |
| 61 / 62 | Layer next / previous | momentary |
| 63 | Solo selected | toggle |
| 64 | Selected layer — source | segmented |
| 65 | Selected layer — pan | continuous |
| 66 | Selected layer — width | continuous |
| 67 | Selected layer — reverse | toggle |
| 68 | Selected layer — half speed | toggle |

Per-layer reverse and speed are nearly free given layers never mix down, and far
more musical than the global versions — reverse the pad, keep the rhythm
forward. 8 and 9 stay as masters; 67 and 68 are the ones that will get used.

### Loops — reserved, see §17

| CC | Control | Type |
|---|---|---|
| 70 | Loop select | continuous |
| 71 / 72 | Loop next / previous | momentary |
| 73 / 74 | Loop save / load | momentary |

### Global

| CC | Control | Type |
|---|---|---|
| 80 | Loop level | continuous |
| 81 | Click | toggle |
| 82 | Click level | continuous |
| 83 | Input monitor | toggle |

The segmented controls map directly onto the existing `SegmentedKnob` in the
layout DSL, so the board-view face comes almost free.

---

## 13. The foot

The first draft designed a switch layout around performance constraints —
bank-independence, no long-press on timing-critical switches. **Those were
answers to a problem that does not exist here.** For recording, a bank change
before Multiply costs a retake.

What replaces it is simply: **assign from §12 as you please, and change your
mind often.** That is the whole point of §2.

The physical facts, for reference:

| Bank preset index | Switch |
|---|---|
| 0–5 | MC6 onboard A–F |
| 6–8 | first FS3X, G/H/I |
| 9–11 | second FS3X, J/K/L — not yet fitted |

So an MC6 MKII bank's twelve presets are **exactly** the maximum physical switch
count: six onboard plus two three-button aux pedals. Nine available today.

An earlier note in this repo described indices 6–11 as dual-press combinations.
That is wrong and should not be relied on.

---

## 14. Display

Two views, both wanted, neither sufficient alone:

**Concentric rings** sharing one phase pointer. The eight-bar ring visibly
containing four sectors of the two-bar ring makes the multiplier relationship
*structural* rather than a number in a box. This is the view that answers "where
are we and when does the next thing happen".

**Per-layer columns**, Repeater-style — every layer's state visible
simultaneously: level, source, pan, length multiple, reverse/speed, mute, and
its provenance. This is the view that answers "what have I got", and with §7 it
answers "and where did it come from".

Also wanted, and learned the hard way on 2026-08-15: **a live map of the switches
as they sit under your foot**, showing what each does right now and lighting up
on receipt. An afternoon went into being unable to distinguish "the stomp did
not arrive" from "the stomp arrived and the daemon ignored it". The map makes
the MIDI path self-evident, and it is the same idea as the pedal donuts — the
screen shows what your feet are about to do.

`Foreign/ClipDiagram.js` is a clip visualisation currently in LoopyPro's service
and is probably reusable for one or both of the first two.

---

## 15. Memory and limits

**There has to be *an* upper limit**, because the audio callback must not
allocate — that is why the arena is pre-allocated at startup. But the limit can
be enormous; RAM is the only real constraint.

Stereo float32 at 48 kHz is 384 KB per second per layer:

| loop length | 8 stereo layers |
|---|---|
| 30 s (current) | 92 MB |
| 2 min | 368 MB |
| 5 min | 920 MB |
| 10 min | 1.8 GB |

The current 30 s is a loop-pedal figure inherited from the proof of concept. For
recording, minutes is the right order — **5 minutes is a good default** and it is
a startup flag, so it costs nothing to choose deliberately.

Genuinely unbounded means one of two things, both deferrable:

- **growing the arena off the audio thread**, with the callback only ever reading
  a pointer — doable, but a glitch risk at the boundary for little gain
- **streaming layers to disk**, keeping a memory window

Disk streaming is the honest answer for **loop slots** (§17), where total memory
is `loops × layers × length` and does explode. Layers-to-disk-as-WAVs is wanted
anyway as the export path, so the two should be designed together.

**2026-09-04:** the limit is memory and only memory now. `--loops` and
`--layers` are uncapped, the arena is allocated zeroed so the kernel commits
pages only as loops fill (an 11 GB ceiling measured at 45 MB resident), the
daemon says the ceiling at startup, asks on a terminal past a quarter of
physical memory, and refuses past all of it with no override. Disk streaming
is a *someday*: the prefetch design it needs is a project, not a flag.

---

## 16. Controlling the interface itself

If the AUDIO4c's routing matrix can be driven programmatically, then **the
interface becomes a pedal too** — and its routing joins a board preset. Recall a
board and the I/O reconfigures with it. That is a substantial idea, and entirely
consistent with everything above.

There is strong precedent in this ecosystem: `es9-config` and `fh2-config` are
exactly this — typed models of a device's SysEx configuration protocol, with
round-trip parse/print and live read/write. The pattern is proven twice.

Order of attack, cheapest first:

1. **Check for published documentation.** iConnectivity has historically been
   more developer-friendly than most; worth ten minutes before reaching for a
   sniffer.
2. **Look inside AuracleX.** If it is Electron, the protocol is readable
   JavaScript and this is an afternoon rather than a project. `ES-config-electron`
   in `archived/` was exactly this situation.
3. **Snoop the wire.** USB MIDI SysEx between the app and the device, diffed
   against deliberate single-parameter changes.

What would make it worth doing: the send in §7 needs inter-host routing
configured, and that is the one thing on this interface that Auracle currently
has to do by hand. Automating the thing that is otherwise a manual step before
every session is the practical payoff; board-recall routing is the interesting
one.

---

## 17. Staging

Stages 1–3 are **done** (2026-08-14). Ordered so each is usable on its own:

1. ~~**Daemon + one loop.** Record, play, overdub as layers, undo.~~ Done.
2. ~~**Ring buffer.** Backward quantisation and retroactive capture.~~ Done.
3. ~~**Master cycle + Multiply.**~~ Done.
4. ~~**Itajara as a registry entry** — the CC map of §12, the channel-13 relay,
   a pedal face.~~ Done 2026-08-15. 48 controls on channel 13, of which the
   daemon implements 8; the rest report themselves as missing by name. The face
   lives on the Looper page rather than in the Overview grid, and the Controls
   page can already assign any of it to any switch.
5. **Per-layer mute and level** in the output sum. The dedicated page's reason
   to exist.
   **Half done 2026-09-04:** per-layer *enable* — a switch, not a gain; the
   layer stays whole and comes back with one verb. Level per layer is still
   open. See `DESIGN-HARVEST.md` §7.
6. **All eight input channels captured; source per layer.** §7.
7. **Per-layer pan and width**, stored native. §5.
8. **The send** — and with it re-amping, the board as outboard FX, and the
   self-calibration of §20.
9. **Export** — layers to disk as WAVs plus a manifest. The route into Ableton,
   and the precondition for loop slots. **Promoted 2026-08-16:** driving Ableton
   from the floor was dropped along with LoopyPro (`DESIGN-CONTROLS.md` §5), so
   getting loops out of the daemon is no longer one route into the DAW — it is
   the only one. Everything below it in this list assumes loops leave the daemon
   eventually; this is where that becomes true.

   **Files are the cheap answer, not necessarily the right one.** Living inside
   Ableton — as a plugin, or a Max for Live device talking to the daemon — would
   beat a manifest of WAVs, because the loop would still be a loop rather than a
   rendered artefact, and the overdub-after-the-fact ideas in §13 would survive
   the trip. Three things make it less far-fetched than it sounds: `continuo`
   already hosts AudioUnit and VST3 plugins headlessly in this rig, so plugin
   machinery is not foreign; Link is already running, so tempo agreement is
   solved; and the daemon's engine is a library the wrapper would call rather
   than a program it would have to reimplement.

   Unresolved, and worth an afternoon's investigation before committing to
   either: whether the wrapper hosts the engine in-process or talks to the
   running daemon, and what that does to the frame-counter clock pairing that
   §3 depends on. Export-to-files is the fallback that certainly works.
10. **Reverse, half-speed, decoupled time/pitch**, global and per-layer.
11. **Loop slots and sequences.** The 1440's move beyond the 720. Needs §15's
    storage model.
12. **Link publish** — the looper as clock master.
13. **MIDI provenance + re-render** — shares machinery with the send.
14. **Board-state provenance.**

---

## 18. Open questions

Several from the first draft are now answered and recorded here rather than
deleted, since the reasoning matters.

1. ~~**Does a layer record a source, or the mix?**~~ **A source**, stored per
   layer, chosen at record time — and retroactively choosable from the ring
   (§6, §7).
2. ~~**Stereo or mono layers?**~~ **Stereo-capable, not stereo-mandatory**:
   native width stored, pan and width applied at playback (§5).
3. ~~**Send pre or post?**~~ **Pre-fader and pre-mute**, so a layer can be
   silent locally and still feed the send (§7).
4. **Parallel loops: slaved or independent?** Slaved (loop B is 1×, 2× or 4× of
   loop A, phase-locked) is simpler, matches the multiplier gesture, and
   preserves the derived clock. Independent (Loopy Pro) is more expressive, much
   harder, and quietly discards §4. **Recommendation: slaved.**
5. **What is the Undo granularity?** Whole layer, or the EDP's "long undo" that
   can retract part of a pass? Note that `undo` currently *zeroes* the layer, so
   **Redo (CC 4) requires changing that first** — the layer must be unlinked and
   kept, not wiped.
6. **Does the daemon need to run without the app at all**, or is app-present the
   assumed case? §2 pushes toward app-present, since the CC surface is relayed
   by the app.
7. **Loop storage format.** WAVs plus a manifest, into `pwyf-store`? That is
   also the cleanest route into Ableton, and would make BlackHole unnecessary.

---

## 19. To verify

- **Does MIDI Guitar on iOS publish notes to CoreMIDI** as well as to its
  internal synth? The whole provenance idea in §8 rests on it.
- **Can the Audio4c routing matrix** carry the dry guitar to the iPad host and
  the synth audio back to the Mac host simultaneously? Two-host routing is the
  unit's purpose, so this should be configuration — but prove it early rather
  than discover a limitation late. §16 may make it programmable.
- ~~**Measure the audio round trip** before writing any overdub code.~~ Done
  2026-08-14 — and it taught us the number is session state rather than a
  constant, which is the more useful result. §11.
- **Calibrate at startup, or verify what is stored.** Follows from the above and
  is not optional. The self-test in §11 is the same mechanism, so this costs
  little beyond what is already wanted. The daemon currently takes
  `--residual 252` **on trust**, which is exactly the stale-calibration hazard
  §11 warns about.
- **The Audio4c channel map**, measured 2026-08-14 with `itajara map`:

  | input jack | host channel | click level | transit |
  |---|---|---|---|
  | 1 | 0 | −25.2 dBFS | 252 samples |
  | 2 | 1 | −25.2 dBFS | 252 samples |
  | 3 | 2 | −27.1 dBFS | 252 samples |
  | 4 | 3 | −27.1 dBFS | 252 samples |

  Identity mapping, no surprises. The useful part is the last column: **all four
  converters are in lockstep to the sample**, so one calibration constant covers
  every input and no per-channel compensation is needed. Exactly one pair
  answered on each run, which re-confirms there is no internal routing.

  The ~1.9 dB level difference splits exactly along the pairs — 1/2 together,
  3/4 together — which points at Auracle's stereo gain sliders rather than
  anything per-jack. Gain is set by hand and is not the engine's business; it is
  noted only so a later reading of these levels is not mistaken for a fault.

  Channels 4–7 read as hard digital zero with no iPad attached: the second-host
  path, and where the synth audio of §7 will arrive. Output jack 1 is host
  channel 0; outputs 2–4 are untested but presumably identity by symmetry —
  **and the send needs them, so test them.**

  No Auracle configuration was needed for capture; only the inter-host routing
  will need it.
- **No internal monitoring path.** With nothing patched, a click on output 1 is
  heard on no input at all. That matters because an interface that routes output
  back internally would yield a confident, precise, entirely fictional latency.

---

## 20. The pedalboard path — un-deferred by the send

**Parked 2026-08-14**, on the argument in §11 that constant pedal latency
cancels between layers. That argument was correct for the case it addressed and
**does not survive the send**.

> When a layer goes out through the board and its return is recorded as a new
> layer, the board's round trip lands *inside* the recorded audio. There is no
> second layer carrying the same offset to cancel against — the re-amped layer
> is late against the original by exactly the board's transit. So the number
> stops being a curiosity and becomes a correction the engine has to apply.

**The good news is that it stops being a manual measurement session.** Because
the looper owns both ends of that path, it can calibrate it itself, with
machinery that already exists in `measure.rs`: send an impulse out of the chosen
output pair, record the return, correlate, store the constant.

So this becomes a button — **Calibrate send loop** — run per output pair,
re-run whenever the board changes. Which is a much better resting place than a
procedure in a document, and it is the same design principle as §11: measure the
configuration you are actually in rather than trusting a stored number.

The manual procedure, kept for the first run and for diagnosing a surprise:

1. Patch AUDIO4c **output 1** → pedalboard in; pedalboard out **L → input jack
   1**, **R → input jack 2**. Everything on the board **bypassed**.
2. **Check levels first.** The Audio4c's output is line level, roughly 20 dB
   hotter than the instrument level the pedals expect, so start quiet:
   `itajara levels --device AUDIO4c --seconds 20`.
3. `itajara sweep --device AUDIO4c --amp 0.05`, raising `--amp` only if nothing
   answers.

**Reading it.** A residual at ≈252 means the board adds nothing to the dry path.
Meaningfully larger means something in the chain converts even when bypassed — a
buffered bypass with an A/D in it would not be visible any other way, and that is
the one genuine discovery available here.

**Second run, now worth doing:** repeat with each wet pedal engaged. Under the
send, "what does this pedal cost in samples" is no longer academic — it is the
per-destination constant for re-amping through that pedal.

## 21. Should the looper be its own app? — no, and the reason is the Twister

Asked 2026-08-27, when `Component.App` had reached 3,991 lines and 320 of
them mentioned the looper. The obvious move is to split PWYF in two: a
pedalboard app and a looper app. It was investigated and rejected, and the
investigation is worth keeping because the objection is not taste.

### The MC6 is not the problem

The relay is already partitioned by channel, and the partitions are disjoint
by construction:

| what arrives | who wants it |
|---|---|
| ch 13 | Itajara's CCs |
| `switchChannel` | the loop machine's own switch namespace |
| ch 1, value 127 | a board-recall footswitch |
| other `0xB0`–`0xBF` | passive observation of pedal traffic |
| SysEx | the device protocol — dumps, acks, sessions |

CoreMIDI sources are multi-client, so two processes can both hold the MC6's
input and each filter to its own rows of that table. **No lock is needed.**
Programming the device *is* single-owner, but that is a human-initiated
write, and a lock around it is trivial.

### The Twister is the problem, and a lock does not fix it

One physical surface, one page, sixteen rings, **no read-back**. Arbitration
today is a single variable — `focusPedalId == itajaraId`. Split the app and
that becomes state in two processes, able to disagree, with no way to ask the
device which it believes; `Data.Twister`'s own header rejects exactly this
shape for exactly this reason.

Two writers to a write-only device is last-writer-wins, and **the loser is
never told it lost**. That is not a semaphore problem. It is a single-owner
problem, and the only cure is one owner.

### The measured objection: a background tab is a dead looper

Chrome throttles timers in unfocused tabs. The looper polls the daemon on a
timer; the MC6 uploader is made of timers; and `LooperSocket`'s own liveness
watchdog *deliberately exempts hidden tabs*, because a background tab's
silence is not evidence. So two tabs means **whichever one you are not looking
at stops being live** — and the looper is precisely the thing that must keep
running while you look at something else. This has already cost an hour once
(the MC6 upload that "stalled" was an unfocused tab, not a device refusing a
bank jump).

There is also a rule on the books that a second app violates head-on: *the
daemon opens no MIDI port; the app is the MIDI hub, so exactly one process
talks to the MC6 and exactly one place decides what a press means.*

### What was done instead: split by layer, not by app

The looper is not spread through PWYF because it is entangled with the
pedals — it is spread through `Component.App` because *everything* was. So
the cut is horizontal:

```
  Data.Looper.*              meaning. Duty -> Machine.perform -> Action.
        │                    Halogen-free, and always was.
  Component.Looper.Page      the page. A row of the fields it reads, a
        │                    record of the actions it can cause, and no
        │                    word for the socket.
  Component.Looper.Control   the adapter. Every Machine.Action except
        │                    ShowBank, which is injected.
  Component.Twister.Lights   device output. Polymorphic in the action and
        │                    the slot row, so it *cannot* decide anything.
  Component.App              the hub: both MIDI ports, MC6 sessions,
                             channel routing, and which surface owns the
                             controller.
```

`Component.App` went 3,991 → 3,466 lines with no behaviour change.

The two lower modules are polymorphic in the action type on purpose. It is
not generality; it is proof. A function whose type cannot name `Action`
cannot dispatch one, so "the lights are painted from the daemon's snapshot,
never from what the app just asked for" is now enforced by the compiler
rather than asserted in a comment.

### If two surfaces at once ever becomes real

The coherent version of "two apps" is not two browser tabs. It is **moving
the hub down**: a daemon holds the MIDI ports and does the channel-partitioned
routing, and browsers become views. Then *who owns the Twister* is a field in
a snapshot, exactly as `click` and `monitor` already are, and the existing
rule — for the things the engine owns, the snapshot is authoritative — covers
it for free. It also ends the throttling problem, because the timers that
matter stop being browser timers.

That is a deliberate reversal of the hub rule rather than an accidental one,
and it is the only version worth the cost. A Twister daemon *alone*, with the
MC6 still in the browser, is strictly worse than either end of the choice.

### The package split, and why it is two packages rather than one

The domain closure of `Data.Looper.*` is **16 modules, ~5,100 lines, and
contains no `Component.*`, no `Config.*` and no Halogen at all**. So a
published package is real and bounded — but eleven of those sixteen are the
shared controller layer (`Data.MC6.{ControlBank,Message,Types}`, `Data.Midi`,
`Data.Pedal.*`, `Data.Twister`, `Foreign.LooperSocket`), which the looper
needs because it knows how to program a pedalboard for itself.

So it is `controllers` underneath `looper`, not a single lift-out. Worth
doing; not the afternoon's work it first appeared to be.
