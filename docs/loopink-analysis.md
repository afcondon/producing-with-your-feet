# LOOPINK MACHINE Analysis

Analysis of a commercially-available LoopyPro live looping template. The template didn't work well in practice (too complex for foot/hand hybrid control, heavily iPad-touch-oriented) but contains useful ideas.

## Template Structure

- **Scene**: 46x28 grid, single scene
- **9 groups** (color-coded): RED KICK, AMBER SNARE, CYAN HAT, BLUE TONE, PURPLE BASS, GREEN TUNE, YLW (overflow), C WHITE (noise), SILREC
- **31 tracks**, **90 widgets**, **998 control bindings**
- External MIDI: ROLI LUMI Keys BLOCK (153 SysEx bindings for visual feedback), midiDRUMs sequencer
- Audio output configs for iConnectivity AUDIO4c, MOTU UltraLite-mk5, Expert Sleepers ES-9

## Core Architecture: 6 Instruments x 4 Layers

The main grid is 6 instrument columns (kick, snare, hat, tone, bass, tune) by 4 layer rows (AM, B, C, D):

| Row | Purpose | Key settings |
|-----|---------|-------------|
| AM | Accumulator — the "live" layer | Most have overdub ~1.01 (accumulate with slight fade) |
| B | Second layer | Some `recIfEmpty`, some overdub=0 (replace mode) |
| C | Third layer | Mixed settings |
| D | Fourth layer | Most have `recIfEmpty` + overdub |

Plus a YLW row (6 more tracks: Y-RED through Y-GRN) and a full-width NOISE track.

## Interaction Model

### Swipe Gestures (400 of 998 bindings)

The template is designed around iPad touch gestures, not MIDI controllers:

| Gesture | Action | Count |
|---------|--------|-------|
| Swipe Left/Right | **Peel Track** (layer undo/redo) | 82 |
| Swipe Up/Down | **Track Merge** (combine layers) | 78 |
| Swipe Up/Down | **Track Parameter** (volume/overdub ramps) | 76 |
| Swipe Up/Down | **Record/Stop** | 60 |
| Swipe Down | **Clear Track** | 19 |
| Swipe Down | **Solo** (row-level) | 12 |
| Swipe Up | **Mute** | 12 |

### Widget Controls (right panel)

- MASTER volume slider, BPM slider
- OVERDUB feedback slider
- PHASE lock, phase align (phi)
- ALL (stop all)
- Reverse: YY (double reverse), Y (reverse)
- Loop math: divide, X multiply, R/2 (rate halve), RX2 (rate double)
- MERGE button, edit button
- RISER slider (effect automation)
- SELECTOR (track selection)
- CLR LOOP

### Row Operations (dot buttons along left edge)

The `*` buttons at the left of each row provide batch operations on all 6 tracks in that row — solo, mute, clear. This is a useful pattern: operate on a layer across all instruments.

### Bottom Strip

6 columns of 3 buttons each: CLR LOOP, merge (arrows), separator, repeat per instrument. A MIDI destination toggle grid at the very bottom.

## Ideas Worth Adopting

### 1. Rate/Speed Manipulation (high priority)
- `R/2` and `RX2` buttons for instant rate halving/doubling
- These are immediate, one-tap transformations
- **For us**: shift+press mappings on bottom encoders

### 2. Beat Quantization Presets (high priority)
- 7 presets across two rows: 0% (off), 16-div at 25/50/75%, 32-div at 25/50/75%
- The percentage maps to both `randomness` and `swing` parameters
- **For us**: dropdown or radio group in the panel — more readable than Twister buttons

### 3. Overdub Feedback Control (high priority)
- Three distinct modes in use:
  - `0` = replace mode (new recording replaces old)
  - `1.0` = full overdub (new adds on top, nothing fades)
  - `~1.01` = accumulate with very slight emphasis on newer material
  - `~3.65` = heavy emphasis on new (old fades fast) — used on Y-BLU
- **For us**: shift+rotate on a top encoder, or a slider in the panel

### 4. Divide/Multiply Loop Length (medium priority)
- Divide and multiply as instant actions
- **For us**: shift+press on bottom encoders

### 5. Reverse (medium priority)
- Single reverse and double-reverse
- **For us**: shift+press or toggle in the panel

## Ideas Less Relevant to Our Setup

### Merge/Peel Framework
- 133 merge + 82 peel bindings — the core workflow of this template
- Powerful for iPad touch (swipe between adjacent tracks) but doesn't map well to a Twister
- Ableton handles our layering/arrangement needs
- Could revisit if we find we need non-destructive layer management

### Tempo Control
- 16 preset tempos (87-174 BPM) — useful for standalone iPad looping
- We sync to Ableton via Link clock, so tempo lives there
- Note: we need Ableton Link clock reaching the MC6 for pedal sync

### LUMI Keys Visual Feedback
- 153 SysEx messages for color-coding keys
- Interesting concept (visual feedback on the controller itself) but we use the Twister LEDs + web panel instead

## What's Missing from LOOPINK

The template is surprisingly **thin on record mode configuration**:
- Uses `recIfEmpty` on some tracks
- Varies `overdubFeedback` per track
- Has some `Assign Clip Settings` bindings for beat quantization
- But barely touches: count-in/count-out, retrospective recording, audio threshold recording, record end actions, tail recording, intro recording, pre-set duration, phase locking strategies

This is exactly the area where we need to go deeper via the LoopyPro manual.

## File Format Notes

The `.lpproj` file is a ZIP containing:
- `Project.sqlite` — all objects (tracks, groups, widgets, effects, etc.) in `objects` table with JSON `contents` column
- `Control Profile.lpcontrolprofile/` — plist files mapping triggers to actions
- `Resources/` — audio files (.aiff) and module state
- `Info.plist` — version info
- `Resources.plist` — resource manifest
- `Thumbnail.jpg` — visual preview

Our `.lpproj` generator already creates this format. The control profile plist is where MIDI bindings live.
