# Harvest — editing, export targets and configurations

**Status:** first draft, 2026-09-04, from a conversation. Nothing here is
built. Companion to `DESIGN-LOOPER.md`: this picks up staging items 9 and 11
of §17 and revisits §21 for one specific case.

---

## 1. What this is

Four requests arrived together, and they are one path: **loops leaving the
daemon**.

- editing — an in and an out point per loop, each with a trimmer
- export options — beyond "every loop as a WAV"
- dedicated configurations — a build of daemon and surface shaped for one
  destination, the first being Instruo's Arbhar
- a more configurable daemon — loops, layers, lengths, and whether layers
  exist at all — and a looper surface that is separable from the pedalboard

The order they are treated in below is the order they depend on each other,
which is not the order they were asked in.

---

## 2. What already exists, so the plan stands on it

Facts read from the code on 2026-09-04, not from memory.

**The daemon's shape is two compile-time constants and one flag.** `N_LOOPS`
is 8 and `MAX_LAYERS` is 4 in `engine.rs`; `--max-secs` (default 300) sizes
the arena at startup as `loops × layers × secs × 2 ch × 4 B`, which at the
defaults is 3.7 GB. The arena is already sized at runtime, so the two
constants are the only thing standing between "compile-time" and "a flag".

**The snapshot already advertises `nLoops`, `maxLayers`, `sampleRate` and
`sources`.** The app does not read the first two: `Data.Looper.Banks.nLoops`
is the constant 8, with the invariant *"must equal `N_LOOPS` in the daemon"*
written beside it.

**Editing verbs the engine has today:** `len` (grow with material, refuse to
shrink below the longest layer — *"a length control that silently trims is a
length control you cannot use in a hurry"*), `o` rotate, `xf` crossfade the
wrap, `ph` and `s` place a layer and set its period, `z` free length, `t`
claim the past, `u` undo. There is no in point and no out point. The word
"trim" appears once, in the comment on `threaded`: *"cleared the moment
anything is recorded, which is the moment resizing would become a trim."*

**Two export verbs, deliberately not one.** `w<name>` (`save_take`) writes
one loop's *layers*, raw, with `take.json` — the session format, the thing
you reload. `ex<name>` (`export_set`) renders every loop flat through the
same `loop_at` the output callback uses, writes an `acid` chunk when the loop
is plain, and records chance, one-shot and mute in `export.json` *instead* of
rendering them: **what we do not render, we record.** Both land in
`~/.itajara/takes/<name>/`. And the stance on targets is already written
down, in the doc comment on `export_set`:

> No reel, no splice markers, no module-shaped anything. `msm` already
> knows what a Morphagene wants and what an Arbhar wants, and it should stay
> the one place that does. What only this daemon can supply is honest audio
> with its bar count attached, so that is all it supplies.

This document keeps that rule. Everything module-shaped below is msm's job.

**Display.** Each layer carries a 48-bucket envelope in the snapshot;
`Component.Looper.Slots` draws *structure* (which blocks of the cycle a layer
sounds in) and says of itself: *"the envelope goes inside those blocks later,
when the daemon grows a peaks message."* A trimmer needs that message.

**The socket serves several clients already** — one thread per connection,
*"there will be one or two clients"* — and every client goes through the same
`dispatch`.

**msm** (`SamplesProject/msm`) has a `ModuleProfile` per module: Arbhar
48 kHz/24-bit/stereo/13 s; Morphagene 48 kHz/32-bit float/stereo/174 s, 32
reels named `mg1`–`mg9`,`mga`–`mgw`, splices as a `cue ` chunk, 300 per reel;
Rample 44.1 kHz/16-bit, kit folders `A0`–`Z99`, voice files `1`–`4` + stem, up
to 12 samples per voice; QD (vpme.de, not Noise Engineering) 44.1 or 48 kHz,
16-bit, mono by default, 128 per voice. Three gaps that matter here, found in
the survey:

- `convert` and `batch` never call `prepare_for_export`, so Arbhar's
  truncation and Morphagene's cue trimming happen only through `sync`
- `folder_structure()` is advisory text; nothing creates kit folders or
  sample-set folders — `sync` flat-joins the stick path and a filename
- **Arbhar banks and layers are not modelled at all**, and the msm-web Arbhar
  panel is a stub marked "Research Needed"

**The Arbhar stick format is not a research question.** Instruo's own sample
libraries are on disk at `~/Library/Mobile Documents/com~apple~CloudDocs/Music
gear manuals/Instruo/Instruo samples/Arbhar 2.0/` and they *are* the spec:

```
_arbhar_library/            one library: 6 banks × 6 layers = 36 folders
  1_1_sample/1_BellDeath.wav   <bank>_<layer>_sample/<k>_<name>.wav
  1_2_sample/…
  …
  6_6_sample/…
_arbhar_library_2/ … _6/    five more libraries, same shape
_arbhar_scenes/             <bank>_<layer>_scene/, the module's own settings
_updater/
```

Every file measured is 48 kHz, 24-bit, stereo. Durations cluster at 12.5, 13
and 14 s with one 19 s outlier.

The firmware 2.0 manual (`/Volumes/Crucial4TB/Books/Manuals/Music/Instruo/
Arbhar-Manual-Firmware-2.0-web.pdf`, pp. 15 and 46–57) settles the rest:

- **Six layers of 10 s each**, and the module *"records for an additional 3
  seconds at the end of each layer"* so long grains at the scan extremes do
  not drop out. On load it *"will load the first 13 seconds of the first
  audio file found in the selected folder"*; several shorter files are
  concatenated until the total passes 10 s. So 13 s is not a ceiling to
  truncate at, it is **10 s of layer plus 3 s of tail**, and the tail is
  meant to be what follows the layer.
- **One `_arbhar_library` per stick**, 36 folders `<bank>_<layer>_sample`.
  The `_arbhar_library_2`…`_6` on disk are Instruo's extra downloads to
  rename into place, not something the module selects between. Library
  slots load **one file into one layer at a time**, from the Clone/Load/Save
  menu.
- **`_arbhar_scenes`**, 36 folders `<bank>_<scene>_scene`, each holding **up
  to six audio files and a `preset.txt`**. Loading a scene *"will load the
  first 6 audio files present in the folder into its 6 Layers"* in one
  action, and the `LoadConfiguration` line in `preset.txt` chooses whether
  the parameters, the layers, or both are loaded. 42 scenes are reachable
  with a stick in: 6 local and 36 on disk.
- File order inside a folder is alphabetical (the module renames on save so
  that *"the saved files will appear alphabetically above"*), so layer order
  is a naming discipline: `1_…` to `6_…`.
- The module runs at 48 kHz, 32-bit internally; the shipped libraries are
  24-bit and the manual states no file-format constraint beyond that.

---

## 3. Editing: a window, not a crop

**An in point and an out point per loop, non-destructive, and the loop plays
the window.** Playback and export share one renderer, so auditioning the
edit and exporting it cannot disagree — the same property that makes
`export_set` honest today.

Mechanically it is one line of position arithmetic. `loop_at` finds the
position in the loop as `(f − origin) mod len`; windowed, it is
`in + ((f − origin) mod (out − in))`. Layers keep their own lengths, periods
and phases, and `layer_pos` keeps computing from the loop position exactly as
now, so structure survives the window and the Slots drawing stays correct: it
just shows less of the cycle.

Three consequences to decide up front rather than discover:

- **Record is refused while a loop is windowed** — *"clear the window
  first."* A new layer's length is the window, its placement is relative to
  the cycle, and reconciling the two is a crop, which this deliberately is
  not. If cropping is ever wanted it is a separate verb that rewrites layers
  off the audio thread, for which Revox's flatten-on-entry is the precedent.
- **A windowed loop is off the grid unless the window is whole bars.**
  `plain()` grows a clause, and the `acid` chunk is written only when the
  window is bar-aligned. Handles snap to beats from `barFrames` and the Link
  quantum by default, with a modifier for free placement. For Arbhar nothing
  needs the grid; for Ableton everything does.
- **`xf` should apply at the window seam.** It crossfades the wrap today;
  windowed, the wrap is `out → in`, which is a cut in the middle of audio and
  needs the crossfade more, not less. Check where `xf` is applied before
  assuming it moves for free.

**Rotation, added on the day.** A window chooses which stretch of the
loop plays; `rot<frames>` chooses where inside it a pass *starts*. Position
zero of a pass is arena position `in + rot`, the render begins there and
wraps inside the window, and no sample moves. It is the "shift the start
point" that editing a loop always wants and that a window alone cannot give
without a crop.

**Wire.** Numeric verbs, per the grammar in `Data.Looper.Verb`: `<n>in<frames>`,
`<n>out<frames>`, `<n>win` to clear (the daemon counts loops from zero; the
surface from one — the usual seam). PureScript: `WindowIn Int | WindowOut Int
| ClearWindow`; `tools/check-verbs.py` must pass. The snapshot gains `in` and
`out` per loop so the page draws what the engine believes, never what it
asked for.

**Peaks.** A trimmer over a 48-bucket envelope is a trimmer you cannot use.
`<n>pk<buckets>` asks the daemon to render the loop through `render_loop`
(which exists, and is the same call as export) and reply with min/max pairs
for the *rendered* loop — not per layer — as a one-off message carrying a
`peaks` key so the decoder can tell it from a snapshot. Requested when the
Edit panel opens and again when the loop's layer set changes (the `born`
stamps say when). Not in the snapshot: thirty a second of something that
changes once a minute is the wrong shape.

**Surface.** An **Edit panel** on the Looper page for the selected loop — the
rendered waveform with two handles, bar and beat ticks under it, the window
length in bars and seconds beside it — driven by mouse and keyboard. **Not a
Twister page.** The looper is at seven pages and the CC space ends at 123
(`grab-bank-and-the-seven-page-wall`); an eighth page displaces one, and
editing is what you do after the take, sitting down. The two fine-nudge
encoders can come later if the mouse turns out to be the wrong tool; the
page is the reference surface, and this is recording, not performing.

---

## 4. Export: three renders, one manifest, and msm does the shaping

Two renders exist. One is missing, and it is the one the Arbhar idea needs.

| verb | what | one file per | grid | exists |
|---|---|---|---|---|
| `w<name>` | one loop's layers, raw | layer | no | yes |
| `ex<name>` | every loop, rendered flat | loop | `acid` when plain | yes |
| `exl<name>` | **every loop's layers, raw, one folder per loop** | layer | no | **no** |

`exl` is `save_take` in a loop over the loops, writing
`<name>/loop-1/layer-01.wav …` and one manifest. All three honour the window
(§3), and `render_loop` is where that happens, so they honour it for free.

**Manifest v2** carries what the shaping side will ask for: per loop its
window, bars, source and tempo; per layer its `gain`, `period`, `phase` and
`born`. This is where the provenance of `DESIGN-LOOPER` §8 eventually lands,
and it costs nothing to leave room for it now.

**The shaping is msm's.** One new subcommand:

```
msm harvest <take-dir> --module <m> --stick <path> [--library N] [--kit A0] [--reel 3]
```

reads `export.json` / `take.json`, applies the module's `ModuleProfile`
(rate, depth, channels, truncation — through `prepare_for_export`, which
`convert` currently skips), and writes the *module's* folder layout, which
`sync` currently does not. The mapping from looper units to module units,
which is the whole point:

| module | Itajara unit → module unit | one session yields |
|---|---|---|
| **Arbhar** | **loop → scene**, layer → layer (≤ 6), 10 s window + 3 s of the loop's own wrap as tail; library slots for single layers | 36 scenes + 36 library samples per stick |
| **Morphagene** | *set as reel:* each loop flat → one splice of one reel; or *loop as reel:* each layer solo → one splice | reels of 8 splices, or a reel per loop |
| **Rample** | loop → voice, layer → that voice's sample stack (≤ 12); four loops → one kit `A0` | kits |
| **QD** | loop → sample-set folder, layer → sample, mono, short windows | sets |

Morphagene's two mappings are both real and both cheap once splices are
positions in a manifest; the first is a set you can Organize through, the
second turns a loop's layers into things the module can play one at a time.
Rample's is the most exact fit: the module already thinks in "a voice with a
stack of samples", which is a loop with a stack of layers.

**Who runs msm.** The browser cannot spawn a process; the daemon must not
(audio thread, and the rule above). `pwyf-store` is Node, is already the
app's server side, and is the right seam: `POST /harvest` with the take name,
module and stick path, shelling to `msm harvest` and returning its stdout as
the ack. msm-web's Axum server is the alternative, but it binds `:3000` — the
same port as Minard's API — and coupling a harvest to a second running
service buys nothing the CLI does not.

---

## 5. Configurations

A configuration is **three profiles that must agree**, and the agreement is
checked at the seams rather than assumed.

**1. Daemon profile — flags, and a Bosun-registered command per profile.**

```
itajara loop --loops 6 --layers 6 --fixed-secs 13 --max-secs 13 …
```

- `--loops` and `--layers` replace `N_LOOPS` and `MAX_LAYERS`. Done, and
  uncapped: the arena is committed lazily, the daemon reports its ceiling
  and asks or refuses by physical memory, and the wire takes any run of
  digits as the loop. The snapshot carries the shape — `nLoops`,
  `maxLayers`, `sampleRate`, `maxSecs`, `fixedSecs`, `ringSecs` — so a
  surface lays itself out from it.
- `--fixed-secs L` threads every loop to `L` at startup and again after
  `c` — the `threaded` empty-tape state exists, and `len` on an empty loop
  already means *"record and it closes itself."* Fixed length is nearly free.
- `--layers 1` with Revox on by default is the **layerless** configuration:
  a tape, overdubs summing into the one layer. "Layers present or not" is a
  count, not a mode.
- Link off and free lengths (`z`) for grain destinations; on and bar-locked
  for Ableton.

Each profile is a **registered Bosun command** (`itajara-arbhar`,
`itajara-ableton`), because Bosun respawns the registered command and a
hand-started daemon loses the race (`bosun-respawns-the-registered-command`).
The flags live in the fixture, once.

**2. Surface profile — the app reads the daemon's shape instead of knowing
it.** `nLoops` and `maxLayers` come from the first snapshot. The constant in
`Banks.purs` becomes a *check* — *"this surface is laid out for 8 loops; the
daemon has 6"* — shown once, plainly, rather than two ghost loops or a
silently wrong bank sweep. The profile then gates what the page shows (which
panels, how many slots, which MC6 bank roles are uploaded, which Twister
pages exist — fewer, never more) and which export target the button offers.
Delivered as `?profile=arbhar` on the same bundle, not as a second bundle;
the split into bundles is §6's question.

**3. Target profile — the msm module and the stick**, held by the harvest
endpoint, not by the daemon.

**Arbhar, concretely.** Six layers because the module has six; a **10 s
window** because a layer holds ten; Link off; free lengths. And the unit a
loop maps to is a **scene, not a library bank**: a scene loads six files into
the six layers in one action, a library slot loads one file into one layer by
hand. Each loop's layers were played against each other, so a scene is *six
takes that belong together*, which is exactly what the omega layer-scan is
for. The loop count is not fixed by the module: 36 scene slots on a stick,
so eight loops is fine and six is one sitting per bank — pick the count by
the session, not the destination.

The 3 s tail is free and correct: `render_loop` renders one cycle, so the
harvest writes the 10 s window followed by its own first 3 s again — the
audio that *does* follow the layer, which is what the module wants there.
The msm profile's 13 s truncation should become "10 s + wrap", not "cut at
13".

Naming carries the layer order (`1_…` to `6_…`, alphabetical). A generated
`preset.txt` with `LoadConfiguration: Load Layers` makes a scene load its
audio without touching the panel's behaviour; a full scene with parameters
is a later option, and `arbhar-midi-mappings.md` is where the parameter
vocabulary would come from. Single layers that deserve a slot of their own
go to `_arbhar_library/<bank>_<layer>_sample/`.

---

## 6. Separability: three strata, not two

`DESIGN-LOOPER` §21 rejected a separate looper *app* because of the Twister
and the hub, and that stands for anything with feet on the pedalboard. The
question that arrived 2026-09-04 is different: **a "\<module\>'s Friend"** —
a small open-source looper for people with an Arbhar, a sound source and a
Mac, backed by the daemon, with every Twister control drawn on the page and
the Twister itself optional. For Andrew, the integrated app stays. So: can
the looper be decoupled from PWYF and the Twister, or is the Friend a
from-scratch thing?

**Not from scratch, and not "decouple the looper" either.** The import
graph, read on 2026-09-04, has three strata where §21 counted two:

| stratum | modules | imports | lines |
|---|---|---|---|
| **A. the Itajara client** | `Foreign.LooperSocket`, `Data.Looper.Verb`, `Data.Looper.Recipes` | nothing of ours | ~1,100 |
| **B. meaning** | `Data.Looper.Machine` (`Duty → perform → Action`) | A, **and `Banks` for `Duty`, `Subject`, `SwitchGesture`, `BankSlot`, `nLoops`** | ~700 |
| **C. surfaces** | `Banks` (MC6), `Data.Looper.Twister`, `Slots`, `Page`, `Engine` | B, and each other | ~5,300 |

Stratum A is already free of everything and is what every Friend needs
first: the socket with its liveness watchdog, the snapshot decoder, the verb
vocabulary that `tools/check-verbs.py` pins against `engine.rs`, and the
recipes. **This is the load-bearing reason not to start over.** Those
eleven hundred lines are a summer of found bugs — set-not-toggle, the ack
hole, sized-but-empty loops, zero-based loops on the wire, a socket that
can lie about being connected — and a fresh client rediscovers each one.

Stratum B has one leak, and it is the whole decoupling problem in one
place: **`Duty` is defined in `Banks.purs`**, the MC6 module. The meaning
layer's own vocabulary lives in the file that lays out footswitches. The
fix is a type move, not a rewrite: `Data.Looper.Duty` holds `Duty` and
`Subject` and knows no switch; `Banks` maps switches to duties, `Twister`
maps encoders to duties, and a web button maps a click to a duty. `Machine`
imports `Duty`, not `Banks`. `ShowBank` is already the injected exception
§21 made it; `SwitchGesture` (press, double, hold) goes with the
switches. The 3,946 lines of `test/Main.purs` pin the spellings and will say
what moved.

Stratum C has one more leak worth naming: `Component.Looper.Slots` draws
the loops in **the MC6's physical order** (`LB.loopRows`, `switchLetter`),
so the on-screen arrangement of a loop display is a fact about a footswitch
box a Friend does not have. It becomes a parameter. `Page` itself is a PWYF
page — MC6 bank numbers, Twister scene, the registry — and stays one; the
Friend gets its own page, which is mostly the Edit panel §3 wants anyway
plus the recording controls as buttons.

**What a Friend is, then:** A + B + a page + the harvest target of §4–5,
built as one bundle with a `?module=` profile, because the differences
between Arbhar's Friend, Morphagene's Friend and Rample's Friend are a table
— loops, layers, seconds, mapping — not code. "Switchable skins on a single
webapp" is the right instinct and it is the same profile mechanism as §5.
The Twister for people who have one is `Data.Looper.Twister` plus a WebMIDI
port: with no MC6 there is a single owner, so the hub problem of §21 does
not arise. What a Friend does *not* carry: `Config.*`, the pedal registry,
board presets, MC6 anything.

**And PWYF becomes the first consumer of the same packages.** That is what
makes the seam real rather than claimed — the same discipline as `reef`,
one source and two runtimes. The package order is A, then B; the split into
`looper` and `controllers` that §21 priced is C's problem and can wait.

**Costs, honestly.** A is an afternoon: a spago package around modules that
already have no dependencies. B is a day, mechanical, touching the imports
of `Banks` and `Twister` and whatever the tests pin. The Slots parameter is
hours. The Friend's page is the real new work, and most of it is the Edit
panel already on the list. **Open-sourcing has one structural
precondition:** the daemon lives in `itajara/` inside this repo, and a
Friend needs it without PWYF, so `itajara` becomes its own repository —
daemon, client package, Friend app — and PWYF depends on it. Also to check
before promising strangers anything: the daemon's calibration path
(`align`, `--residual`) and how much of it assumes this rig.

## 7. Order of work

Each step is usable on its own, and each unblocks the next.

1. ~~**Daemon flags** `--loops`, `--layers`, `--fixed-secs`; app reads
   `nLoops`/`maxLayers` from the snapshot and turns the constant into a
   check.~~ Done 2026-09-04: the two constants are `Shared::n_loops` and
   `Shared::max_layers`, `MAX_LOOPS` is ten because the wire names a loop
   by one digit, `--fixed-secs` threads every loop as an empty tape at
   startup and after `c` and stands in for `--max-secs`; the app says once,
   in the log, when the daemon's loop count is not the eight it is laid
   out for. Smoke-tested on BlackHole at 6 × 6 × 13 s.
2. ~~**Window verbs, peaks message, Edit panel.**~~ Done 2026-09-04, plus
   a **rotation** (`rot<frames>`): where a pass starts inside the window,
   without moving a sample, so a render begins where you chose rather than
   where the take closed. The Edit panel is three sliders over the
   waveform — in, out, start — stepping by a beat on the grid and a frame
   off it, with the window shaded and the playhead drawn. Two
   simplifications against §3: a windowed loop is simply not `plain()`, so
   it gets no `acid` chunk even when bar-aligned; and the seam crossfade
   reuses `xf` at loop level in `loop_at` rather than moving the per-layer
   one. Peaks are `mix_at` over every arena position — the flattened,
   levelled loop at unity — so the picture is in the positions the sliders
   set.
3. **`exl` and manifest v2.** The per-loop-per-layer render.
4. **msm `harvest`**: the importer, the Arbhar scene and library model with
   a generated `preset.txt`, the 13 s truncation replaced by "10 s + wrap",
   and the `prepare_for_export` omission fixed on the way.
5. **Store endpoint + "Export for Arbhar" button.** The first configuration
   is now real end to end.
6. **Profiles**: Bosun-registered `itajara-arbhar`, `?profile=` gating on the
   surface.
7. **Morphagene, Rample, QD** — each one mapping row in §4 and one msm
   profile, in the order they are wanted.
8. ~~**Stratum A as a package** (§6)~~ Done 2026-09-04: `itajara-client/`,
   seven registry dependencies, module names unchanged (`74afc74`).
9. ~~**`Duty` out of `Banks`** (§6)~~ Done 2026-09-04: `Data.Looper.Duty`
   holds the vocabulary, `Data.Looper.Switchboard` holds the MC6's way in,
   the grab facts ride in `Rig`, and `Data.Looper.Twister` no longer
   imports Banks at all (`677b313`). It took an hour, not a day.
10. **Arbhar's Friend**: A + B + one page + the `?module=` profile of §5,
    the Twister optional. **First light 2026-09-04**: `itajara/friend/`, one
    app with a face per module (`?face=arbhar`), designed in that repo's
    `docs/DESIGN-FRIEND.md`. On the way, stratum B moved into the client
    package (`Data.Looper.Duty`, `Data.Looper.Machine`; the machine reads
    the loop count from the snapshot and this app keeps its eight in
    `Data.Looper.Surface`), and a third package `itajara/surface` holds the
    Edit panel and the wave drawing, which this app now imports rather than
    owns. The harvest itself (steps 3–5) is still to do; the Friend's Save
    writes `w<take>-<n>` per loop and says so. ~~`itajara` becomes its own repository first, so
    the Friend has a daemon without PWYF.~~ Done 2026-09-04:
    `music/itajara` — `daemon/`, `client/`, `tools/` — with its history
    (`git filter-repo`), this app consuming the client by path (`4aa37a2`),
    and Bosun's registered command pointed at the new home.
11. **The `looper` / `controllers` split** for PWYF's own bundle, when C
    wants it.

Not on the list: Arbhar's MIDI interface (§8), dropped the same day it was
raised.

Also done 2026-09-04, off this list because it was asked for separately:
**per-layer enable** (`ly<n>1`/`ly<n>0`, `on` per shape in the snapshot, a
checkbox per layer row; `2878e43`). Necessary once layers are the grouping
a harvest uses — six takes in one loop and a way to hear five of them.

---

## 8. The MIDI half — considered and dropped, 2026-09-04

**Decision: no UI for Arbhar's MIDI interface.** The tool is for the
recording process — making samples with guitar, modular or iPad, getting
them onto the stick, and loading them — because that is where the
module's button combinations cost time. Once the layers are loaded, the one
thing MIDI adds that the panel cannot is polyphony, and a Keystep 37
plugged into the USB port already does that with no software at all.
Everything below is what the join *would* have bought, kept for the record
because the routing facts and the omni hazard stay true if this is ever
reopened. It is not on the order of work.


Instruo's MIDI implementation for Arbhar is not in the firmware 2.0 manual
(the word does not occur in it) but was shared by Instruo on Discord and is
recorded in `arbhar-midi-mappings.md`: eighteen CCs, of which five are ± pairs
(reverb, delay, hold, panning — each a positive and a negative CC, summed
like CV), one is **layer select** (CC 23), and the rest are the panel knobs.
No channel is stated; no capture, strike or load verb exists. Treat the
table as data we hold, not a library we publish.

**The cheap half is a registry entry.** Arbhar is a pedal in exactly the
sense `DESIGN-LOOPER` §2 made Itajara one: a channel, a CC map, a layout.
The registry buys the same free table — MC6 assignment, a Twister page,
board presets that capture the module's parameter state, the Controls page
— for one PureScript file. One new control kind is needed: a **paired CC**,
one bipolar encoder that sends the `+` CC above centre and the `−` CC below,
which the Twister's `center: Just (mv 64)` already anticipates. And one
hazard must be settled before the first note: **if the module listens omni,
it hears every pedal's CCs** — and CC 0–4 are reverb, delay, spray, scan and
length. Arbhar gets its own MIDI output port on the interface, or a filter,
before it joins the hub.

**How MIDI reaches it, without giving up the Twister.** Arbhar's USB port
is a *host* port: it reads a flash drive, and a Twister plugged straight in
works for the same reason. Two hosts cannot be cabled together, so there is
no lead from that port to the Mac's hubs. But the port wants a *device*, and
the cheapest USB MIDI device there is is a class-compliant USB-to-DIN cable
(or a Roland UM-ONE). Its USB end goes into Arbhar; its DIN-in end goes to
the **AUDIO4c's MIDI out**, which the Mac already owns as its own CoreMIDI
port and which carries only what the app sends there. That is the dedicated
port the omni hazard asked for. The MC6's MIDI out is the wrong one: it
merges everything arriving over USB and feeds the WIDI to the pedals, so
Arbhar on it would hear every pedal's CCs. The Twister stays on the Mac,
untouched.

Two caveats. Arbhar has to enumerate the adapter — class-compliant and
bus-powered, so a known one rather than the cheapest — and **the adapter
and the flash drive share the one port**, so stick and MIDI alternate
unless a hub happens to work there, which is worth one try and no more.
That alternation is the practical argument for the CV-gated live capture
above: it is the harvest that needs no swap.

**The stick is offline, and MIDI cannot load it.** There is no CC that loads
a layer or a scene; the menu is Shift, Capture, Strike. So the integrated
tool cannot push audio into the module live *by MIDI*. The live route that
does exist is the module's own: Itajara plays a loop into Arbhar's input and
**Capture** is gated — by hand, or by CV from the ES-9 through `es9-daemon`,
which is the rig's existing way to gate a module — with CC 23 having chosen
the layer first. That is a harvest without a stick, at the price of a
DAC→ADC trip and the input limiter, and without scenes or `preset.txt`.
Both routes are worth having; the stick is the exact one and the gate is the
immediate one.

**What the join actually buys is knowing what is in the layers.** Two
separate tools — a harvester and a pedal — would each work. The integrated
one knows *which loop is in which layer*, because it put it there, and so:

- **the pedal face can show resolved meaning**, the Hedra treatment: layer β
  is *loop 3, four bars of the DI*, not "layer 2"; the scan position is a
  bar and beat of that loop, not 0–127; length is a beat or a bar of *that*
  loop's tempo, because the manifest carries it. That is the app's whole
  argument about pedals, applied to a module whose state is even more
  hidden than a looper's.
- **layer select follows the surface.** Selecting a loop on the Looper page
  or the Twister sends CC 23 for the layer it went to, so the module and the
  screen agree about which sound is under the knob.
- **a board preset carries an Arbhar scene's parameters** — the CC half of
  a scene — without the stick, because that is what board presets already
  do for every pedal.

**What it is, then.** Not a second tool: the pedal registry entry, the
harvest target of §4–5, and a **scene index** in the store that records
which take went to which scene and layer. The index is the seam between the
two halves and is small — a manifest cross-reference — and the rest is
machinery that exists.

**To verify first, in this order:** the MIDI channel (or omni), the physical
route (TRS type A from a dedicated port), and that CC 23 with the Layer knob
fully anticlockwise selects layers cleanly. Then the ± pairs, whose "127 on
both cancels to 0" note suggests the summing is literal.

---

## 9. Open questions

1. **Windowed overdub: refuse, or crop?** Refuse is proposed; crop is the
   heavier verb and can wait for a reason.
2. **Where the peaks message lives** — one-off reply, as proposed, or a field
   in the snapshot recomputed on `born` change. One-off is cheaper on the
   wire and the decoder already handles the ack path, which is the same shape.
3. ~~**Arbhar**: per-layer buffer length, and whether libraries 2–6 are
   selectable from the panel.~~ Answered from the firmware 2.0 manual, §2:
   ten seconds plus a three-second tail, and one library per stick. What
   remains is whether a scene's `preset.txt` with `Load Layers` is honoured
   when the file carries no other parameters — one stick and one boot will
   tell.
4. **Morphagene**: set-as-reel, loop-as-reel, or both. Both is one flag.
5. **msm's two stale UIs** — the Rample sidebar (`SP1.wav`, 260 kits) and the
   Morphagene sidebar (`mg1`–`mg32`) disagree with the Rust, which is the
   executable truth. Fix or delete before `harvest` reads either.
6. **msm-web on `:3000`** collides with Minard's API. Not this project's
   problem, but it is the reason §4 shells to the CLI rather than posting to it.
7. ~~**Arbhar's MIDI channel, or omni.**~~ Moot: the MIDI half is dropped
   (§8). Recorded there if it is ever reopened.
8. **Live capture by CV gate**: worth building beside the stick route, or
   is the stick enough? The gate is cheap if `es9-daemon` already has a
   spare bus; the question is whether the DAC→ADC trip is acceptable for
   material meant to be granulated anyway. With the MIDI half gone this is
   the only route that skips the stick, which makes it more interesting,
   not less.
