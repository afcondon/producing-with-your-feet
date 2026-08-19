# MC6 SysEx Direct Programming

## Status: Implemented (2026-03-01)

All phases complete. The webapp can program MC6 presets directly via SysEx — no Morningstar Editor or backup export/import cycle needed.

## Architecture

### MIDI Connections

Bidirectional MC6 connection via WebMIDI:
- `mc6Input` — receives switch presses (relayed to LoopyPro, board recall triggers)
- `mc6Output` — sends SysEx for programming presets

Both auto-selected on startup using `mc6Input.match` pattern from `config/rig.json`.

### SysEx Protocol (`src/Data/MC6/SysEx.purs`)

**Frame builder**: `sysexFrame deviceId funcIds payload` — builds complete SysEx message with Morningstar manufacturer ID (00 21 24), 6 function bytes, payload, XOR checksum, and F7 terminator.

**Session commands** (deviceId 0x00):
- `sysexConnect` / `sysexDisconnect` — enter/leave editor session

**Upload protocol** (deviceId 0x03 for MC6MK2):
- `sysexStartUpload` — MC6 responds with "ready" (F1=7, F2=0, F3=33)
- `sysexCompleteUpload` — MC6 commits data
- `sysexPresetData bankNum presetNum shortName longName messages` — full preset with TLV payload
- `sysexClearPreset bankNum presetNum` — send empty preset

**TLV encoding** (Tag 7F):
| Type | Length | Contents |
|------|--------|----------|
| 00 | 3 | Header: bankNum, presetNum, isExp |
| 01 | 9 | Message record (x16 slots) |
| 02 | 8 | Short name (space-padded) |
| 03 | 8 | Toggle name (space-padded) |
| 04 | 24 | Long name (space-padded) |
| 05 | 4 | Preset config (zeros) |

Messages are always padded to 16 slots. Complete TLV blob sent every time (header + all messages + names + config).

### MC6 Message Types (`src/Data/MC6/Message.purs`)

Constructors for the message types we generate:
- `ccMessage ch cc val action` — CC on channel
- `pcMessage ch program action` — Program Change
- `bankJumpMessage bank action` — Jump to bank 0-29
- `delayMessage ms` — Delay between messages
- `engagePresetMessage preset action` — Engage preset 0-11

### Upload Session Wrapper (`src/Component/App.purs`)

`withEditorSession output action` wraps SysEx sends:
1. Connect, wait 500ms
2. Start upload, wait 500ms
3. Execute action (send preset data)
4. Wait 300ms, complete upload
5. Wait 500ms, disconnect

Fixed delays (no ACK parsing yet). Reliable in practice.

## Board-to-MC6 Flow

### Strategy: Direct Programming (Strategy B from original plan)

Each board preset is programmed as a full MC6 preset containing the actual pedal messages (PC + bypass CCs), not webapp-mediated triggers. The MC6 can recall boards independently of the webapp.

**`boardToMC6Messages`** converts a board preset to MC6 messages:
- For each pedal in the board (skipping EngageNoChange):
  - PC message if preset has a `savedSlot` (program number)
  - Bypass CC(s) if pedal is EngageOff (single or dual engage)
- Messages indexed 0-15 (max 16 per MC6 preset)

### Auto-Sync on Assignment

When a board is assigned to an MC6 switch (`handleAssignBoardToSwitch`):
1. Updates `mc6Assignments` in state and localStorage
2. Calls `syncSwitchToMC6` which immediately SysEx-programs the MC6
3. MC6 display shows the board name; pressing the switch sends all pedal messages

When unassigned or deleted: `syncSwitchToMC6 _ _ Nothing` sends a clear preset.

### Bank Management

- 5 board banks available (MC6 banks, selectable via `mc6BoardBankNum`)
- Bank-level clear: `handleClearMC6Bank` removes all assignments for active bank and SysEx-clears all 9 switches
- Bank up/down via MC6's native two-switch combos

### Backup Export (Legacy)

`injectBoardTriggers` still available for JSON backup export — injects CC triggers into `mc6Banks` loaded from a backup file. This was the pre-SysEx workflow; kept for compatibility.

## Key Files

| File | Role |
|------|------|
| `src/Data/MC6/SysEx.purs` | SysEx frame builder, TLV encoders, session commands |
| `src/Data/MC6/Types.purs` | MC6Message, MC6Preset, MC6NativeBank, action/type enums |
| `src/Data/MC6/Message.purs` | Message constructors (CC, PC, bank jump, delay, engage) |
| `src/Data/MC6/Backup.purs` | MC6 backup JSON codec |
| `src/Engine.purs` | AppState, MidiConnections (mc6Input/mc6Output), MC6Assignment |
| `src/Engine/Storage.purs` | localStorage persistence for assignments |
| `src/Component/App.purs` | SysEx session management, board-to-MC6 conversion, auto-sync |
| `src/Component/Boards/View.purs` | Board builder/list UI, MC6 switch assignment dropdown |

## Resolved Questions

- **TLV completeness**: Full blob required (header + all 16 message slots + names + config). Confirmed working.
- **MC6 output discovery**: Same device name as mc6Input. Auto-selected from available outputs.
- **ACK parsing**: Not implemented. Fixed delays work reliably. Future optimization if needed.

---

## Correction and inventory (2026-08-19)

The status above — "Implemented … no Morningstar Editor or backup
export/import cycle needed" — is true of *presets* and false of the device.
Two things were found by writing six banks and reading the device back.

### The bank number in a preset frame is ignored

`sysexPresetData` puts `bankNum` in the header TLV and the device pays it no
attention: an upload lands on whichever bank **the editor is currently on**.
Six banks addressed to 22–27 all went to bank 19, on top of each other. The
device accepts every frame and says nothing.

`Component.App.uploadBanks` is the fix — jump with `sysexEditorBankChange`,
wait for the device to confirm via `mc6CurrentBank`, then write, and report
any unconfirmed bank as refused rather than writing it. It also needs **a
session per bank**: committing an upload leaves the editor in a state that no
longer answers a bank change.

### What we can write is one thing out of eight

Writing is `sysexPresetData` and `sysexClearPreset`. That is the entire write
surface. Reading — via the device's connect-time dump, or an editor backup —
covers far more:

| On the device | Can read | Can write |
|---|---|---|
| Preset: 12 switches, names, messages | yes | **yes** |
| Per-message name (`mi`) | yes (backup) | no — no TLV for it |
| Bank name | yes | no |
| `bankClearToggle` | yes | no |
| Bank-level messages (`bankMsgArray`) | yes | no |
| Expression presets (`expPresetArray`) | yes | no |
| `omniports` | yes | no |
| `midi_channels` (13 named) | yes | no |
| `general_configurations` | yes | no |
| `waveform_engines`, `sequencer_engines` | yes | no |
| `scroll_counters`, `resistor_ladder_aux` | yes | no |
| `midi_events`, `bank_arrangement` | yes | no |

### Therefore: a factory reset is not reversible by this app

Most acutely **`omniports`**. Both ports are `type: 8` with fixed switch
assignments (41/42/43 and 38/39/40) — that setting is *what makes the FS3X
switches exist*. Without it there are six switches, not twelve, and the loop
bank's way home (switch G) is among the six that vanish.

So "Year Zero" needs a two-stage restore: factory reset, then load a
synthesised full backup **through Morningstar's editor**, which can write
everything; our SysEx owns bank and preset edits from then on. Which reverses
an earlier judgement — synthesising a backup is not about upload speed, it is
the only channel that carries the settings.

### Numbers worth having

- `longPressTime: 12` — the device's own long-press threshold. The app-side
  hold timer for the loop banks has to agree with this, and it is readable
  rather than guessable.
- `bankChangeDisplayTime: 60`, `switchSensitivity: 2`, `midiSendDelay: 0`.
- The device's `midi_channels` table **disagrees with `config/pedals/*.json`**:
  the device labels 15 LoopyPro and 16 Habit, the app puts Habit on 15 and
  nothing on 16. The labels are cosmetic on the device — routing is
  `sendToPort` — but it is exactly the second-source-of-truth this work exists
  to remove, and `Data.Looper.Banks` claims channel 16.

## The SysEx frame map, as far as it is known (2026-08-19)

Long-term this matters more than any one feature: if Morningstar stop
supporting the MC6, what we have written down *is* the device. So this table
is kept honest about the difference between what has been confirmed against
bytes and what is inferred from a size and a plausible story.

Function codes are bytes 6 and 7 of the frame (`F1`, `F2`), after the
Morningstar header `F0 00 21 24 <dev>`.

### Host → device (what we can send)

| F1 F2 | Meaning | How known |
|---|---|---|
| `00 1B` | connect / enter editor session | confirmed, in daily use |
| `00 1C` | disconnect | confirmed |
| `00 1F` | editor bank change (+ bank, `01`) | confirmed — it is what makes uploads land |
| `00 2B` | request all preset names | confirmed |
| `07 00 30` | start upload | confirmed |
| `07 00 31` | complete upload | confirmed |
| `07 00 33` | request full dump | confirmed |
| `07 11` | preset data (TLV) | confirmed |

That is the entire known write surface, and it writes one kind of thing.

### Device → host (what it volunteers)

Captured in `test/mc6-connect-dump-20260816.json`, 119 frames on connect.

| F1 F2 | Bytes | Carries | How known |
|---|---|---|---|
| `00 7D` | 18 | editor mode on/off | confirmed, decoded by `Data.MC6.Read` |
| `03 20` | 674 | **midi_channels** | confirmed — MC6, MOOD, Clean, Hedra, Flint, Lex, Brig, Habit, LoopyPro all legible as ASCII |
| `03 23` | 82 | **omniports** | confirmed — payload holds 41/42/43 and 38/39/40, the fixed switch assignments in the backup |
| `03 25` | 55 | sequencer_engines | probable — carries 119, 39, 127, matching `arr` |
| `03 21` | 50 | general_configurations | inferred from size |
| `03 22` | 58 | ? | unknown |
| `03 24` | 35 | waveform_engines | inferred — 4 engines × 4 fields |
| `03 26` | 67 | scroll_counters | inferred |
| `03 27` | 242 | midi_events or resistor_ladder_aux | inferred, the two large tables |
| `03 28` | 50 | ? | unknown |
| `03 29` | 52 | ? | unknown |
| `03 00`, `03 01` | 18 | device identity? | unknown |
| `06 01` | 272 | preset data (messages) | confirmed, decoded by `Data.MC6.Dump` |
| `06 02` | 245 | preset data (names) | confirmed |
| `08 00` | 31 | ? | unknown |
| `09 01` | 150 | switch names of the current bank | confirmed, decoded by `Data.MC6.Read` |
| `10 04`, `10 05` | 18 | ? | unknown |
| `11 00` | 50 | ? | unknown |
| `11 03` | 37 | ? | unknown |
| `11 05` | 828 | **bank_arrangement** — all 30 bank names | confirmed, all legible |

**The read half is nearly free.** The device volunteers every settings section
on connect and the app currently acknowledges each frame, tallies it by
function code, and throws the payload away. Decoding `03 2x` is a
parsing job against bytes already in the repo — no device required.

### The write half needs one experiment

Note the pattern that already holds for presets: the device *sends* preset
data as `06 01`/`06 02` and *accepts* it as `07 11`. Read and write are
different codes. So the settings write codes are very likely a parallel
family, plausibly `07 2x` against `03 2x` — but **that must not be swept**.
Sweeping unknown reads costs nothing; sweeping unknown writes puts arbitrary
payloads into a device's flash.

The right experiment is the one that produced the read protocol: run
Morningstar's editor with a MIDI monitor between it and the device, change
one bank name, and capture what it sends. One capture per settings section we
want, each cheap and each answering exactly one question.

Priority, by how often the thing changes: bank names and bank-level messages
first, then waveform engines and scroll counters. Omniports last — a port
type is set once in the life of the board.

## The settings frames, decoded (2026-08-19)

Decoded from `test/mc6-connect-dump-20260816.json` alone — the device
volunteers all of this on connect, so no device was needed and nothing was
sent to one. Cross-checked field by field against the March backup's
`controller_settings`, which is what makes these confirmations rather than
readings.

### Frame envelope

```
F0 00 21 24 <dev> <dir> <F1> <F2> 00 00 00 00 00 00 <lenHi> <lenLo>  <payload…>  <xor> F7
                                                    └── bytes 14-15
```

**Bytes 14–15 are the total frame length as two septets**, `hi*128 + lo`.
Confirmed twice: `05 22` on a 674-byte frame (5×128+34), `01 16` on the
150-byte switch-names frame (1×128+22). Byte 5 is `03` on frames from the
device and `00` on frames we send.

Payloads come in two shapes. Large sections are **TLV**, a run of
`7F <index> <length> <bytes…>` starting at offset 16. Small ones are **flat**:
a count, then that many fixed-width records.

### The sections

| Frame | Shape | Contents | Status |
|---|---|---|---|
| `03 20` | TLV, 48 | 16 × 12-char channel name; 16 × `[0, portHi, portLo, remap]`; 16 × 16 bytes (unknown) | confirmed — names legible, `15*128+127 = 2047` matches `sendToPort` |
| `03 21` | flat, 32 | general configuration | confirmed — 2, 60, 12, 1 all present as `switchSensitivity`, `bankChangeDisplayTime`, `longPressTime`, `numMidiCable` |
| `03 22` | flat, 40 | `0, 29`, nine zeros, then 1…29 — bank arrangement | **inferred** — the shape fits and nothing else does, but the leading pad is unexplained |
| `03 23` | flat, 64 | count, then per port: `portNum, type, tip×3, ring×3, tipRing×3` | confirmed — `2, 0,8,41,127,127,42,127,127,43,127,127, 1,8,38,…` is the backup's omniports verbatim |
| `03 24` | flat, 17 | count, then per engine: `num, min, max, type` | confirmed — 4 engines, `20,100,4` and `0,127,2` match |
| `03 25` | flat, 37 | count, then per engine: `len`, then 16-step `arr` | confirmed — engine 0's array matches the backup byte for byte |
| `03 26` | flat, 49 | count 16, then per counter: `min, max, start` | confirmed — 16 scroll counters, 0/127/0 |
| `03 27` | TLV, 16 × 11 | midi events: `numberFrom/To, channelFrom/To, typeFrom/To, valueFrom/To`, 3 flags | confirmed — 11 fields, values match |
| `03 28` | flat, 32 | count 8, then per switch: `num, trigger, f1, f2` | confirmed — 8 aux switches, matches `resistor_ladder_aux` |
| `03 29` | flat, 34 | `32, 16`, then 32 × 127 | **unknown** |
| `11 05` | TLV | all 30 bank names | confirmed — legible |
| `09 01` | TLV, 12 × 8 | switch names of the current bank | confirmed, already decoded |

Nine of the ten `03 2x` frames are now accounted for, eight of them
confirmed against independent data. `03 29` and the trailing 16×16 block of
`03 20` are what remain.

### One thing worth noticing while we were in here

Every channel's `sendToPort` is 2047 — all ports — except **channel 16, which
is 2034**. That is 2047 minus 13, so three ports are switched off for that
channel alone. Channel 16 is the one `Data.Looper.Banks` took for the switch
namespace, on the strength of a comment saying it was free, and the device
labels it `Habit`. Whatever the truth, channel 16 is not an unremarkable
empty channel on this device.
