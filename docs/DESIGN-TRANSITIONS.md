# Switches as transitions — a direction, not yet a plan

*2026-08-17. Companion to `DESIGN-CONTROLS.md` (the MC6 as an instrument),
`DESIGN-v2.md` (pedal state as belief) and `DESIGN-LOOPER.md` (Itajara).*

This records a direction that came out of a session about loops and globals. It
is deliberately not a roadmap: the one experiment that would tell us how much
there is to gain cannot be run yet (§7). Written down so the reasoning survives
until it can be.

---

## 1. The criticism that started it

Ask whether this app is more than a nicer Morningstar editor and the honest
answer is: partly, and the tell is precise. **Our nouns are theirs.** Bank,
preset, message, toggle position — the data model was adopted wholesale, and
almost everything added since is an adjective on one of those nouns. Provenance
is an adjective on a bank. Verb classification is an adjective on a preset.
Globals are an adjective on a slot. They are good adjectives; the sentence
structure is Morningstar's.

Four things genuinely are not theirs:

- **The pedal registry.** We know CC 27 on channel 15 is a named Habit parameter
  with a range; their editor knows a number. This is the only place we operate on
  the *rig* rather than on the controller, and it is why board presets can exist.
- **A model of belief about a device.** Their editor presents its own document as
  truth. We separate observed from authored from declared from unknown, and
  refuse to launder one as another.
- **The survey as graph analysis** — reachability, stranded banks, dead ends.
- **Refusing rather than truncating** at the sixteen-message budget.

But all four are about *knowing* the pedalboard better. None changes what a
footswitch fundamentally is: a thing that sends a fixed list of messages when
pressed. That is Morningstar's model, and it is where tap-dancing comes from.

---

## 2. The proposal

**A switch is bound to a transition, not to a message list.** The state is the
rig's configuration; a transition names a target; the messages are *derived* as
the difference between where we are and where we are going.

Three consequences, each of which addresses something already hit:

**Messages become derived rather than authored.** A transition's cost is its
diff. This is the bank-entry insight generalised: hoisting "Pedal A off" to bank
entry is exactly "the transition *into* this state carries that action, so the
transitions *within* it need not". It also largely dissolves the sixteen-message
budget, because you stop paying to send what is already true.

**The states and the graph already exist and are simply not connected.** A board
preset *is* a state — a named whole-rig configuration. The navigation graph the
survey computes *is* a transition graph. Board presets are states with no
transitions; banks are transitions with no states. Nothing needs to be drawn from
scratch; the machine is latent in data we already hold.

**"Where am I" becomes answerable**, which is the actual cure for tap-dancing.
Tap-dancing is not caused by having too few switches. It is caused by the rig's
state being invisible, so it has to be reproduced by hand. It also legitimises a
context-dependent switch: one switch meaning different things in different states
is modal confusion when undeclared and an interface when the machine declares it.

---

## 3. The information theory, stated properly

A fixed layout is an **order-0 code**: every switch means one thing regardless of
context, so it must be sized for the whole vocabulary of a session. A state
machine is a **context-dependent code** — the same press decodes differently
given the state — which is why a context-modelling compressor beats a static
Huffman table on the same data. The gain comes from conditioning the code on
something both ends already know.

That sharpens what to optimise. The floor on presses-per-action is the entropy of
the *action stream*, and that stream is heavily peaked: a session uses a few dozen
distinct actions in strongly habitual order. So most actions should cost one
press, where today many cost two or three — jump a bank, then press. **The waste
is not switch count. It is a static code against a peaked, state-dependent
distribution.**

Corollary worth keeping in view: the available compression is bounded by how
skewed the real distribution is. If the next action is genuinely unpredictable,
no encoding helps and the honest answer is that this idea buys nothing.

---

## 4. Put the modality in the app, not the MC6

For anything the app mediates — pedal presets, Itajara, AUM — **the MC6 switch
need not change at all.** It sends one stable CC and the app decides what that
means given the state. No SysEx, instant, visible on screen.

This is forced by a hard number: writing a bank is twelve presets at ~100 ms,
so **~1.2 s per bank**. Per-state MC6 layouts are ruled out during performance by
write speed, not by the idea. Only what the MC6 must do *itself* — its own bank
jumps — requires the hardware to change.

Which flips what a bank is for. If meaning lives in the app, a bank is not a
dispatch table: **it is a display.** You jump banks to relabel the six visible
switches, not to change what they do. Given that only six of twelve switches are
labelled, this may be the more valuable half of the idea — the scarce resource was
never the switches, it was the labels.

---

## 5. The cheapest first version

**Return to the hub automatically after an action completes.** "Playing a loop
puts you back to bank 0, where you can navigate to the next sound" — the
originating example, and the safest possible transition: deterministic, and
caused by a press you just made, so it cannot surprise you.

Note what it makes obsolete. `returnSwitchIndex` and the "< Back" switch exist
*because* the return is manual. Make it implicit and a switch is freed on every
page — one sixth of the labelled surface, measurable, with no new concepts and
nothing to author.

The generalisation is the **cyclic transition**: one switch that advances a song's
sections. That is a bank of tap-dancing collapsed into one footswitch, and it is
the same shape as the loop-arranging work in `DESIGN-LOOPER.md` — spread, shift,
and a phase that advances.

---

## 6. Two limits to build around from the start

**Observability is not uniform.** The MC6 can now be read in full; Itajara
reports its own state thirty times a second; AUM and most pedals tell us nothing.
A transition computed as a diff is trustworthy against the first two and
open-loop against the rest, and the design must know which — a rig confidently
wrong about the guitar synth channel is worse than one that re-sends everything.

The compound example — engage the guitar synth channel *and* route its output to
the looper, which reaches into AUM — is the most valuable case precisely because
it is the thing nobody can remember, and simultaneously the least verifiable. It
probably wants "send it all, idempotently" rather than a diff. Reliability is a
property of the destination, not of the mechanism.

**The code must stay predictable to the player, not merely short.** An adaptive
encoding that surprises you is worse than a longer one you trust, because you
will abandon it mid-take — exactly when it was supposed to help. Practically:

- transitions fire from things you did, never from inference;
- the screen always shows the current state and what each switch means *now*;
- no switch silently changes meaning during a take.

That makes the display part of the encoding rather than a readout of it, which is
a fair argument that this belongs in an app rather than in controller firmware.

---

## 7. The measurement, held in reserve

The way to find out whether the compression is 2× or 10% is to measure
presses-per-action on real sessions. The app is the MIDI hub — every press passes
through it, by the rule that keeps exactly one process talking to the MC6 — so it
can log the transition sequence and estimate the distribution the code should be
fitted to. No other controller can do this, because no other controller sees the
whole stream.

**Deferred deliberately.** Today's MC6 contents and today's habits are artefacts
of what could be programmed by hand in Morningstar's editor, so a log taken now
would measure the constraint rather than the workflow. The experiment only becomes
meaningful once the board is being used in earnest. Kept in reserve, not dropped.

---

## 8. The failure mode to avoid

**Do not build a general state-machine editor.** That is where interface projects
die: infinitely expressive, unauthorable, and you end up hand-drawing what you
used to hand-stomp. The discipline is that the authoring surface stays "this
switch goes to *that* state", or "next in this list", and the graph is something
you *see*, derived, rather than something you draw.

---

## 9. What holding a session costs (settled 2026-08-18)

The device **will not change bank for us without an editor session** — tested,
not assumed. The same request addressed to device `0x00` (the number connect and
disconnect use, and `F1=0` is otherwise the family of things done to a running
board: bank up/down `0,16`/`0,17`, toggle page `0,33`) does nothing at all: no
reply, and the MC6 was still on its previous bank when a session opened straight
afterwards. Morningstar's editor changes banks without the device visibly
entering edit mode because it holds one session open for as long as it is loaded.

So every idea in this document — the hub return, the auto-configured jumps —
requires a **held-open session**, not a connect per jump.

Holding one is only safe with the controller setting **"Load Preset Data into
Editor using Switch Press"** turned off. On (the factory default), the device
cannot distinguish a press meaning "load this into the editor" from one meaning
"engage this preset", so while an editor is connected it blocks the ambiguous
functions: **its own bank jump, and MIDI clock.** Off, presses stop feeding the
editor and everything is unblocked — which costs us nothing, because we select
presets in the app rather than by stomping.

Implemented as `Hold a session` in the survey's action row: the setting goes off
as the session opens and back on as it closes, so the unblocking is scoped to the
session rather than left behind on the instrument.

**Two things still open:**

- **MIDI clock is unverified on this rig.** The blocked-list is Morningstar's
  documentation, not something observed here, and the only clock consumers are
  tap tempo on a few pedals. Check it when that area is next worked on — it is
  the claim this whole direction rests on, and it would fail quietly.
- **The restore writes the default, not what was found.** The `3/33` reply
  carries the controller settings and certainly contains this bit, but which byte
  is unknown, so releasing a session turns the setting back *on* whether or not
  it was on before. Fixable by capturing `3/33` twice with the setting toggled
  and diffing.
