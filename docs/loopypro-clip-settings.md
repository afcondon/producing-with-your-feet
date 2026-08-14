# LoopyPro Clip Settings Reference

Comprehensive reference for LoopyPro clip configuration, compiled from the manual (loopypro.com/manual/), wiki (wiki.loopypro.com), and forum research. Focus is on settings relevant to MIDI-controlled live looping with the Explorer app.

## Settings Hierarchy

LoopyPro uses a three-tier override system:

1. **Global** (project-wide default)
2. **Color Group** (overrides global for all clips in that color)
3. **Individual Clip** (overrides both)

Each override has two toggles: one to enable the override, one to set the value. Overrides must be removed manually — the "Assign Clip Settings" action can change values but cannot add or remove overrides.

**UI implication**: Any clip settings panel should make the active level visible.

## Complete Clip Settings Inventory

### Playback Settings

| Setting | Values | Description |
|---------|--------|-------------|
| **Loop / One-Shot** | boolean | Loops cycle continuously; one-shots play once and stop |
| **Phase Locked** | boolean | ON: playhead advances with transport even when stopped (play/stop = unmute/mute). OFF: always starts from beginning. Ideal for loops that are multiples of each other |
| **Play Quantization** | None / Master / Custom | When playback starts relative to clock |
| **Stop Quantization** | None / Master / Loop / Custom | When stop takes effect. Loop = at end of clip's own cycle |
| **One-Shot Quantization** | None / Master / Custom | Separate play quantization for one-shots |
| **Beat Quantization (WARP)** | divisions, strength, randomness, swing | Non-destructive playback-time quantization. Adjusts transient timing without modifying data |

### Recording Settings

| Setting | Values | Description |
|---------|--------|-------------|
| **Record Count-In** | None / Master / Loop / Custom | When recording begins. Loop = Master for first, loop-aligned for overdubs |
| **Record Count-Out** | None / Master / Loop / Custom | When recording ends |
| **Auto Count-Out** | boolean | ON: recording ends automatically. OFF: waits for tap, then stops at next aligned boundary |
| **Record End Action** | Play / Stop / Overdub | What happens when recording finishes |
| **Record If Empty** | boolean | Whether tapping an empty clip initiates recording |
| **Overdub Feedback** | 0.0 -- ~4.0 | 0 = replace, 1.0 = standard overdub, >1.0 = emphasize new |
| **Pre-Set Duration** | bars/beats/seconds | Fixed recording length |
| **Length Quantization** | bar multiples / subdivisions | Constrains loop lengths for rhythmic consistency |
| **Retrospective Recording** | boolean | Captures previously-played audio (clock must be running) |
| **Retrospective Quantization** | Immediate / Quantized | Immediate = ends at tap. Quantized = last complete clock cycle |
| **Audio Threshold Recording** | boolean | Recording waits for audio level. **Overrides count-in to None** |
| **Intro Recording** | boolean | Captures pre-loop audio during count-in. **Requires count-in (incompatible with threshold)** |
| **Tail Recording** | boolean | Captures decay after loop ends. Plays after loop but doesn't affect loop length |
| **Record On Press** | boolean | Record starts on press vs release (timing precision) |

## Recording Modes — How They Interact

```
Audio Threshold -> when recording starts (audio level trigger)
                   OVERRIDES count-in to None
Count-In       -> when recording starts (clock alignment)
                   required by Intro Recording
Intro Recording -> captures pre-loop audio DURING count-in
                   INCOMPATIBLE with threshold (needs count-in)
Count-Out      -> when recording ends (clock alignment)
Auto Count-Out -> whether recording ends automatically
Pre-Set Duration -> specific length for auto count-out
Tail Recording -> captures post-loop audio AFTER recording ends
                   independent of all other settings
Record End Action -> what happens after recording (play/stop/overdub)
Retrospective  -> entirely different paradigm (captures past audio)
                   operates on empty clips only, clock must be running
```

### Practical Combinations

| Workflow | Count-In | Count-Out | Auto | Threshold | Notes |
|----------|----------|-----------|------|-----------|-------|
| Free looping (first loop sets tempo) | None | None | off | off | Classic mode |
| Quantized loops | Master | Master | on | off | Fixed-length, clock-aligned |
| Free entry, loop-aware exit | None | Loop | off | off | Start anywhere, end at loop boundary |
| Hands-free recording | n/a | Master | on | **on** | Play and it records |
| Pre-set length | Master | Master | on | off | Plus pre-set duration |
| Retrospective capture | n/a | n/a | n/a | off | Different paradigm entirely |
| With intro/pickup | Master | Master | on | off | Plus intro recording on |

### Overdub Feedback Semantics

| Value | Behavior | Use case |
|-------|----------|----------|
| 0.0 | Replace — new completely overwrites old | Re-recording a part |
| 0.5 | Fast fade — old decays quickly | Building up then evolving |
| 1.0 | Standard overdub — equal layers | Traditional loop layering |
| ~1.01 | Accumulate with slight emphasis on new | LOOPINK's default — subtle evolution |
| ~3.65 | Heavy emphasis on new — old fades fast | LOOPINK used on one track |

## Continuous Parameters (MIDI-controllable)

| Parameter | Range | Behavior |
|-----------|-------|----------|
| **Volume** | 0.0 -- 1.0+ | Output level |
| **Balance** | -1.0 -- +1.0 | Left/right pan |
| **Speed** | variable | Time-stretch (pitch constant). Artifacts possible |
| **Rate** | variable, negative = reverse | Varispeed (pitch varies with speed, like tape) |
| **Pitch** | semitones | Pitch shift (speed constant). Artifacts possible |
| **Overdub Feedback** | 0.0 -- ~4.0 | See table above |

All support save/restore to 128 slots and ramp times for smooth transitions.

## Actions Reference (MIDI-triggerable)

### Clip Actions
Play/Stop, Solo, Mute, Record, Adjust Parameter (volume/balance/speed/rate/pitch/overdub), Adjust Playhead, Clear, Merge/Move, Multiply Length, Divide Length, Select, Phase Align, Reverse, Peel/Replace Layers, Cancel Count Ins/Outs

### Group Actions
Play/Stop, Solo, Mixer Solo, Mute, Adjust Parameter

### Clock Actions
Toggle Pause, Seek Timeline, Phase Align, Tap Tempo, Adjust Tempo, Adjust Beats Per Bar, Reset Clock, Set Master Length, Toggle Metronome, Toggle Ableton Link, Toggle MIDI Clock Sync

### The "Assign Clip Settings" Action
Changes clip setting values during performance. Can target a specific clip, a color group, or all clips. This is the bridge between the UI and MIDI control — any setting exposed in our panel could be sent as an Assign Clip Settings payload via MIDI CC mapping.

**Limitations**: Cannot remove overrides (only change values). When targeting "all clips", individual-level overrides take precedence and are not affected.

## Beat Quantization Presets (from LOOPINK)

| Preset | Divisions | Randomness | Swing |
|--------|-----------|------------|-------|
| Off | n/a | 0% | 0% |
| 16th tight | 16 | 25% | 25% |
| 16th medium | 16 | 50% | 50% |
| 16th loose | 16 | 75% | 75% |
| 32nd tight | 32 | 25% | 25% |
| 32nd medium | 32 | 50% | 50% |
| 32nd loose | 32 | 75% | 75% |

## Follow Actions

Automatic responses to clip state changes. Triggers: Begin Record, End Record, Begin Play, Stop Clip, Clear Clip, Amplitude Envelope. Can fire any action in the system with quantization or delay.

## Key Design Insights for Our UI

1. **Recording mode is the highest-leverage setting** — the combination of count-in, count-out, auto count-out, and record end action defines the looping workflow. This should be front and center.

2. **Beat quantization is a compound control** — divisions + strength + randomness + swing. Presets are more practical than 4 sliders. Dropdown with named presets (Off, 16th Tight, 16th Loose, etc.).

3. **Overdub feedback needs labeled ranges** — 0 (Replace), 0.5 (Fade), 1.0 (Standard), >1.0 (Accumulate). A slider with semantic labels.

4. **Incompatible combinations exist** — threshold overrides count-in; intro requires count-in. UI should make these constraints visible.

5. **Assign Clip Settings is how we push config to LoopyPro** — our panel dropdowns can map directly to Assign Clip Settings MIDI actions sent to LoopyPro.

6. **The three-tier hierarchy matters** — we should decide whether our UI configures the global level, the group level, or individual clips. For a Twister-based workflow, global + per-group makes most sense.

7. **Not everything needs the Twister** — mode selection and configuration are better as panel dropdowns. Continuous parameters (volume, speed, overdub feedback) work well on encoders.
