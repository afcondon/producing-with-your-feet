# Recipes — what to press, and what should happen

**This is the test script and the manual, and it is one file on purpose.** A
list of gestures that nobody runs goes stale in a week; a list that is walked
through on the rig every time the surface changes cannot. Every expected ack
below is quoted from a real one — if a run disagrees with this file, one of the
two is a bug and the run is usually right.

Next round this becomes a modal in the app. It has to: following a written
sequence means looking away from the browser, and **a background tab is a dead
looper** — Chrome throttles it and Twister messages stop being handled, which
reads exactly like a control that has stopped working.

## The board

```
Page 1 · Loops              Page 2 · The set
 Loop1 Loop2 Loop3 Loop4     Loop1..4   turn = pan, press = stop/go
 Loop5 Loop6 Loop7 Loop8     Loop5..8
 Rec   Ovr   Stop  Arm       launch Click Monitor ·
 layers Clear Capt  PAGE     StopAll StartAll ClearAll PAGE

Page 3 · Shape              Page 4 · Set up
 speed decay chance ·        Grid  OneShot Listen Pendulum
 Mult  Shift Dense  Save     bars  every   on     Length
 ·     ·     ·      ·        tape  leaves  keeps  fade
 ·     ·     ·      PAGE     ·     ·       ·      PAGE
```

The pager is the bottom-right encoder and reads an **absolute position**: 0–31
is page 1, 32–63 page 2, 64–95 page 3, 96–127 page 4. A quarter turn a page.
Press it to go home to Loops.

## The two facts everything else rests on

**The bar's length comes from Link; its downbeat comes from the music.** With a
clock, a bar is Link's bar. Where the downbeat falls is the first loop's origin
once anything has been recorded, and Link's only until then — because Link knows
a bar's length far better than a looper can, and knows where the downbeat is
only as well as a UDP hop allows.

**A loop's length is a count of bars.** `bars` on Set up. That is what makes a
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
- **Page 4** press Grid so it lights
- **Page 4** turn bars to 4
  - *loop 0 is set to 4 bars (8.000 s); record and it closes itself.*
- **Page 1** press Record, count yourself in
  - *loop 0 starts on the grid in 0.88 s*
- play four bars and touch nothing
  - *loop 0 committed: 8.000 s, 1 layer playing.*

> **Arm is a trap here with Grid on.** It waits for a sound and then for the next bar line, so playing just after a line costs almost a whole bar and the attack with it. Record and count in — that is what the click is for.

## A four-bar first loop, where your note is the downbeat

The same length, started by playing rather than by counting. Use it when Link is giving you a tempo rather than a performance.

- **Page 1** press Loop 1
- **Page 4** leave Grid OFF
- **Page 4** turn bars to 4
  - *loop 0 is set to 4 bars (8.000 s); record and it closes itself.*
- **Page 1** press Arm
- play — the take starts on your note
  - *loop 0 committed: 8.000 s, 1 layer playing.*

> That note becomes **bar one for the whole rig**: the bar's length still comes from Link, its downbeat now comes from you. Nothing is aligned to Ableton after this and everything is aligned to what you played.

## A one-bar second loop against it

The kick after the song. A loop SHORTER than the first one, which is the thing the old model could not express at all — the pulse was loop one's length, so one cycle meant four bars.

- **Page 1** press Loop 2
- **Page 4** press Grid — it is per loop, so loop 2 needs its own
- **Page 4** turn bars to 1
  - *loop 1 is set to 1 bar (2.000 s); record and it closes itself.*
- **Page 1** press Record
  - *loop 1 committed: 2.000 s, 1 layer playing.*

## A bar, spread over four, landing on the third

One phrase placed in a longer loop rather than repeated through it. The layer keeps its own length throughout — only where it lands moves.

- record a one-bar loop as above
- **Page 4** turn bars to 4
  - *loop 1 is 4 bars (8.000 s); its layers keep their own lengths.*
- **Page 4** turn every to 4
  - *layer 1 sounds once every 4, on slot 1.*
- **Page 4** turn on to 3
  - *layer 1 is on slot 3 of 4.*

> Watch the waveform rather than the words: the bar moves to the third of four empty ones. That picture is why this is three knobs and not a sentence about how often something happens.

## Multiply — playing the length instead of naming it

For when you do not know how many bars yet. You count with bars and you play with this; both are worth having.

- **Page 3** press Multiply
- the write head opens
  - *loop 0 multiplying from the start of this cycle — play across as many cycles as you want, then x again.*
- play across as many cycles as you like
- **Page 3** press Multiply again
  - *rounds to whole cycles, waiting for the boundary if it rounded up*

> It feels like an overdub because it **is** one — an overdub that also lengthens the loop. Refused unless the loop is playing forwards at unity.

## Declaring a length, with no clock

Only reachable with Link off. With a clock, bars resizes instead — and the ack always says which of the two happened.

- **Page 1** press Record, play, press Record again
  - *the one place a closing press still survives*
- **Page 4** turn bars to 4
  - *loop 0 is 4 bars — the bar is now 2.000 s. Nothing was moved.*

> No audio changed. The pulse is a quarter of what you played, so a one-bar loop 2 is now possible.
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
