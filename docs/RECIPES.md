# Recipes — what to press, and what should happen

**This is the test script and the manual, and it is one file on purpose.** A
list of gestures that nobody runs goes stale in a week; a list that is walked
through on the rig every time the surface changes cannot. Every expected ack
below is quoted from a real one — if a run disagrees with this file, one of the
two is a bug and the run is usually right.

It is also a modal in the app, and it had to be: following a written sequence
means looking away from the browser, and **a background tab is a dead looper**
— Chrome throttles it and Twister messages stop being handled, which reads
exactly like a control that has stopped working. Both come from the same
module, so they cannot disagree.

And there is a third rendering, for paper: **Print sheet** on the Looper page
opens a self-contained document in a new tab — the four boards at a glance, a
page of detail for each, then these recipes — laid out for A4, which the
browser's own dialog turns into paper or a PDF. It is generated from
`Data.Looper.Twister.pages` and `Data.Looper.Recipes` like everything else here,
so reprint it whenever a control moves rather than annotating the old one.

## The board

<!-- GENERATED from `Data.Looper.Twister.pages`, like the recipes below: paste
     it from `spago test`, which prints it. Every word of it comes from
     `controlAt`, so a control that moves takes its own description with it. -->

```
Page 1 — Loops
  Loop 1            Loop 2            Loop 3            Loop 4
  Loop 5            Loop 6            Loop 7            Loop 8
  Record            bars/Grid         Stop/Go           Arm
  Clear             layers/Undo       Capture           page

Page 2 — The set
  Loop 1            Loop 2            Loop 3            Loop 4
  Loop 5            Loop 6            Loop 7            Loop 8
  launch            Click             Monitor           Tempo
  Clear All         Stop All          Start All         page

Page 3 — Shape
  speed             Pendulum          decay             ·
  chance            every             slot              Dense
  Multiply          Shift             Save              Export
  ·                 ·                 ·                 page

Page 4 — Set up
  One Shot          Listen            Length            ·
  input             Mono              ·                 ·
  tape/Revox        leaves            lo-pass           fade
  ·                 ·                 ·                 page

loop colours: armed violet, recordingFirst red, overdubbing orange, multiplying yellow, playing green, idle blue
```

The pager is the bottom-right encoder and reads an **absolute position**: 0–31
is page 1, 32–63 page 2, 64–95 page 3, 96–127 page 4. A quarter turn a page.
Press it to go home to Loops.

Two cells carry a mode on the press and a value on the turn, and are named after
both halves: **bars/Grid** on page 1 and **tape/Revox** on page 4. Each is lit
when its mode is on.

**Save and Export sit side by side, and they are not two spellings of one
thing.** Save writes *this loop's layers*, raw — itajara's own format, the take
you reload tomorrow to keep overdubbing. Export writes *every loop that holds
something*, each flattened and rendered to one WAV: layer gains, decay, speed,
direction and where a sparse layer lands, all applied, which is what Ableton,
Loopy Pro and `msm` mean by a loop.

What Export deliberately leaves out of the audio is chance, one-shot and mute —
the three things that decide whether you hear a loop *this time round* rather
than what it sounds like. Baking a roll of the dice into a file would make it
one performance instead of the loop, and every receiver these files go to can
roll its own. They are written into `export.json` as numbers instead: **what we
do not render, we record.**

Each file also carries an `acid` chunk — its tempo and beat count — so a take
drops into Ableton already warped rather than needing to be told its length
eight times. A loop at any speed but one, or on a pendulum, has no whole number
of beats to declare and gets no chunk, because a confident wrong answer there
warps to the wrong grid.

The files are numbered as the board labels them: loop 1 is `loop-1.wav`, even
though the daemon's own ack for it still says "loop 0". A filename is read in
Finder and in Ableton's browser with no ack beside it to explain the offset, so
the seam is crossed here, and the ack says so out loud when it does.

## The two facts everything else rests on

**The bar's length comes from Link; its downbeat comes from the music.** With a
clock, a bar is Link's bar. Where the downbeat falls is the first loop's origin
once anything has been recorded, and Link's only until then — because Link knows
a bar's length far better than a looper can, and knows where the downbeat is
only as well as a UDP hop allows.

**A loop's length is a count of bars.** The `bars` knob on the Loops page —
turn it before the take and the recording closes itself, or record free and it
counts its own bars against the clock. And the arrow runs both ways: **Tempo**
on page 2 takes the session tempo *from* a loop, so the click can come to what
you played instead of the other way round. That is what makes a
loop shorter than the first one possible at all, which is the whole of the
kick-after-the-song idea.

---

<!-- The recipes below are GENERATED from `src/Data/Looper/Recipes.purs`, which
     is what the app's Recipes modal renders. Do not edit them here: change the
     module and paste this section from `spago test`, which prints it. A written
     sequence that names controls is wrong the moment a control moves, and wrong
     in the one place nobody thinks to check, because it is only documentation. -->

> The daemon counts loops from zero, so the ack for Loop 1 reads "loop 0". The lines quoted below are its own words, untranslated — the app's own log line above each of them counts from one.

## A four-bar first loop, in time

The ordinary way in when Link is running and you want to sit on Ableton's grid. Check the legend reads a tempo before you start.

- **Page 1** press Loop 1
- **Page 2** press Click
- it ticks four to the bar, downbeat louder
  - *a click before anything is recorded — that is the point of it*
- **Page 1** press bars/Grid so it lights
- **Page 1** turn bars/Grid to 4
  - *loop 0 is set to 4 bars (8.000 s); record and it closes itself.*
- **Page 1** press Record, count yourself in
  - *loop 0 starts on the grid in 0.88 s*
- play four bars and touch nothing
  - *loop 0 committed: 8.000 s, 1 layer playing.*

> **Arm is a trap here with Grid on.** It waits for a sound and then for the next bar line, so playing just after a line costs almost a whole bar and the attack with it. Record and count in — that is what the click is for. **Running it a second time?** Clear forgets the grid flag AND the bar count, so both of those steps are needed every time round, not only the first.

## A four-bar first loop, where your note is the downbeat

The same length, started by playing rather than by counting. Use it when Link is giving you a tempo rather than a performance.

- **Page 1** press Loop 1
- **Page 1** leave bars/Grid unlit
- **Page 1** turn bars/Grid to 4
  - *loop 0 is set to 4 bars (8.000 s); record and it closes itself.*
- **Page 1** press Arm
- play — the take starts on your note
  - *loop 0 committed: 8.000 s, 1 layer playing.*

> That note becomes **bar one for the whole rig**: the bar's length still comes from Link, its downbeat now comes from you. Nothing is aligned to Ableton after this and everything is aligned to what you played.

## The click comes to you, not the other way round

For a take that is right except that it ran a little long or a little short against the click. Nothing is stretched — the session tempo is re-derived from what you played, which is the floor-looper move rather than the DAW one.

- **Page 1** press Loop 1, leave bars/Grid unlit
- **Page 1** press Arm, then play four bars
  - *loop 0 committed: 8.129 s, 1 layer playing.*
- look at bars/Grid — it already reads 4
  - *a free take counts its own bars against the clock, so there is nothing to declare and nothing to trim*
- **Page 2** press Tempo
  - *tempo taken from loop 0: 8.129 s over 4 bars is 118.09 bpm.*

> It takes the **average** over the bars, not your timing within them: play four bars slightly slow and the click comes to you, play them unevenly and they stay uneven. That is the point. It also moves Ableton and anything else on the Link session — if other loops are already down they keep their audio and stop agreeing with the click, and the ack counts them so you find out now rather than later.

## A one-bar second loop against it

The kick after the song. A loop SHORTER than the first one, which is the thing the old model could not express at all — the pulse was loop one's length, so one cycle meant four bars.

- **Page 1** press Loop 2
- **Page 1** press bars/Grid — the grid is per loop, so loop 2 needs its own
- **Page 1** turn bars/Grid to 1
  - *loop 1 is set to 1 bar (2.000 s); record and it closes itself.*
- **Page 1** press Record
  - *loop 1 committed: 2.000 s, 1 layer playing.*

## A bar, spread over four, landing on the third

One phrase placed in a longer loop rather than repeated through it. The layer keeps its own length throughout — only where it lands moves.

- record a one-bar loop as above
- **Page 1** turn bars/Grid to 4
  - *loop 1 is 4 bars (8.000 s); its layers keep their own lengths.*
- **Page 3** turn every to 4
  - *layer 1 sounds once every 4, on slot 1.*
- **Page 3** turn slot to 3
  - *layer 1 is on slot 3 of 4.*

> Watch the waveform rather than the words: the bar moves to the third of four empty ones. That picture is why this is three knobs and not a sentence about how often something happens.

## Multiply — playing the length instead of naming it

For when you do not know how many bars yet. You count with bars and you play with this; both are worth having.

- record a loop as in any recipe above — Multiply extends something, it does not start one
- **Page 3** press Multiply
  - *loop 0 multiplying from the start of this cycle (0.25 s recovered from the pre-roll) — play across as many cycles as you want, then x again.*
- play across as many cycles as you like
- **Page 3** press Multiply again
  - *loop 0 x1: now 4.535 s (1 cycles of 4.535 s) — 2 layers playing.*

> It feels like an overdub because it **is** one — an overdub that also lengthens the loop. Refused unless the loop is playing forwards at unity.

## Declaring a length, with no clock

Only reachable with Link off. With a clock, bars resizes instead — and the ack always says which of the two happened.

- **Page 1** press Record, play, press Record again
  - *the one place a closing press still survives*
- **Page 1** turn bars/Grid to 4
  - *loop 0 is 4 bars — the bar is now 2.000 s. Nothing was moved.*

> No audio changed. The pulse is a quarter of what you played, so a one-bar loop 2 is now possible.
PASS - the sheet names every control and every recipe
PASS - the sheet counts its pages from one
PASS - every tone has ink on the sheet
PASS - the sheet escapes what could close a tag
PASS - the sheet prints the eight loops once, as a range
PASS - every recipe has steps, and most of them say what to expect

## When something looks broken

1. **Is the browser tab focused?** Chrome throttles background tabs and the
   looper stops responding to the Twister. This is the first thing to check and
   it has cost hours.
2. **Read the ack**, in "What happened" on the Looper page. Every refusal in
   this engine says why. A control that appears to do nothing has almost always
   said something.
3. **Does the loop have what the verb needs?** `every` and `on` refuse on an
   empty loop; `bars` refuses with no clock and no recorded loop; Multiply
   refuses on a loop that is not playing plain.
