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
