# Auracle X — what is reachable without the app

`Auracle X` (`com.iconnectivity.auracle`, 2.4.0.640) is the configuration app
for the iConnectivity interfaces, including the **Audio4c** the whole rig's MIDI
passes through. This records what a look inside the bundle established, so the
next person does not repeat it.

The motivation is the same one that drove reading the MC6. The Audio4c decides
whether a message the app sends actually reaches a pedal — routing matrix,
channel filters, channel remaps — and right now that configuration is something
we believe rather than something we check. Unlike the pedals, this device *can*
be asked.

## What it is

A Chromium Embedded Framework app: `Contents/Resources/index.html` plus a 19.7 MB
webpack bundle, `main.js`, over a native host in `Contents/MacOS/Auracle X`. The
React side does no MIDI at all — no Web MIDI, no SysEx bytes. Every device
operation is a call into `window.libiConnectivity.*`, a binding the native host
injects. So the bundle gives us the complete **API surface** and the native
binary holds the **wire format**.

The binary is not stripped. It carries C++ symbols for
`iConnectivity::SysEx1Device`, `iConnectivity::SysEx2Device`,
`SysExMessage`, `SysEx2Message` and `AppleMidiBridge` — one class per protocol
generation, with a method per operation.

## The Audio4c is a SysEx1 device

`isSysEx2()` in the bundle returns true only for
`midi-mioXM`, `midi-mioXL`, `midi-mioXC`, `paudio-1U`, `paudio-2U`.

The Audio4c's productId is `icaudio4c`, so it is **SysEx1** — the older iConfig
protocol, which is the better-documented of the two and the one third-party
implementations target. This is the single most useful fact here: it means the
work is likely confirmation rather than discovery.

## The cheap way in: the backup is a .syx file

There is a Backup/Restore pair in the Presets pane that writes a
`Sysex File|.syx` — a dump of the device's own wire messages rather than an app
format, which would have been the MC6 backup file all over again.

**It is not available to us.** The whole fieldset is guarded:

```js
this.device.isSysEx2() && createElement("fieldset", …, legend "Backup/Restore", …)
```

and the Audio4c is SysEx1, so Auracle simply does not render it. (The
"Currently Loaded: (n) name" heading above it is gated the same way, which is
why the pane looks sparser here than in iConnectivity's screenshots.) There is
no file route to the Audio4c's configuration, and nothing of the sort exists on
disk — no user library, no saved backup, and `~/Library/Application Support/
Auracle X/` holds only a CEF cache, a firmware folder, and a log full of
RTP-MIDI interface churn.

What the Audio4c's Presets pane does have is Save/Load, and both are device
operations, not file operations:

```js
Save: setPresetName(name) -> savePreset(presetNo)
Load: restorePreset(presetNo)
```

So a "preset" here is a numbered slot **inside the interface**, holding the
whole configuration — routing, filters, remaps, mixer. It is not saved anywhere
on the Mac, which is why the app never says where it went.

That is a familiar shape: the Audio4c's presets are in the same position as the
pedals' presets, addressable by number and not by content. The difference, and
the reason this is worth doing at all, is that this device can be *read back*.

## So the way in is their own DevTools — via the debugging port

`ShowDevTools()` is called unconditionally in the root component's `render`, but
it does not produce anything usable: there is no right-click Inspect and no
DevTools window. The framework, though, is stock CEF, and its bundled Chromium
still honours the switch:

```sh
open -a "/Applications/Auracle X.app" --args --remote-debugging-port=9222
curl -s http://127.0.0.1:9222/json          # one page target, "Chromeraptor"
```

That gives a full CDP endpoint on Chrome 116, and `Runtime.evaluate` against the
page target reaches `window.libiConnectivity` directly — all 208 bindings, live,
from a script. `static/auracle-probe.js` is the read-only sweep; the CDP driver
that runs it is a dozen lines of `fetch` plus Node's global `WebSocket`.

**Three things bite, and all three are worked around rather than fixed.**

*The app exits when the CDP client disconnects.* Not when a call fails — a
trivial `1+1` evaluate followed by a socket close kills it just the same. So a
probe gets exactly one connection: do all the work in a single `Runtime.evaluate`
and accept that the app dies when you let go.

*Some bindings crash the native side on a SysEx1 device.* `GetPresetNameMax`
takes the whole app down, and by extension the other preset-*name* calls should
be assumed to as well. This is consistent with the UI hiding every name-related
element behind `isSysEx2()`: on this device those calls are not merely
unavailable, they are unimplemented.

*Which makes losing the result the default.* The workaround is the useful part:
CEF logs page `console.log` output to `~/Library/Logs/Auracle X_debug.log`, so a
probe that logs each result as it goes survives its own crash. That is how
`GetPresetNameMax` was identified — the log ends mid-call, and the last line
names the culprit.

## What the Audio4c is actually set to

Captured 2026-08-16, `test/audio4c-config-20260816.json`. AUDIO4c, serial
000010A8, firmware 1.0.4, hardware 1.5.

**One preset slot.** `GetPresetMax` is 1 — so "Save Preset" has exactly one
place to go, which is why Auracle never asks for a name. There is no library of
interface configurations to recall from; there is one, and saving overwrites it.
That kills the idea of making the interface's routing part of a board preset,
and it makes the one slot worth treating as the reset baseline rather than as
storage.

**Twenty-nine ports**, in three groups:

| ports | kind | what they are |
|---|---|---|
| 1 | DIN | the physical MIDI jack |
| 2–11 | USB-device | the ten MIDI ports one computer sees: `DIN`, `USB2`, `HST1`–`HST8` |
| 12–21 | USB-device | the same ten for the *second* computer: `DIN`, `USB1`, `HST1`–`HST8` |
| 22–29 | USB-host | `HST1`–`HST8`, the devices plugged into the Audio4c's own USB-A jacks |

Ports 3 and 13 are the computer-to-computer bridge — computer one's `USB2` is
computer two, and vice versa.

**The routing is not symmetric, and the asymmetry is the interesting part.**
Host port `HST1` (port 22) fans out to twelve destinations: the DIN, both
computers, both computers' HST1, and every other host port. Every other host
port is point-to-point — `HST3` (24) reaches only `[6, 16]`, its own port on each
of the two computers. So whatever is on HST1 is heard by everything, and
everything else is heard only by the two Macs.

That is worth knowing before we trust any belief about what reaches a pedal:
one host jack is promiscuous and seven are not, and which physical socket a
device is plugged into decides which it gets.

**Filters: `[9]` in and out on all twenty-nine ports, and nothing else.** On a
SysEx1 device the codes are `3` Realtime, `4` MTC Quarter Frame, `5` Song
Position, `6` Song Select, `8` Tune Request, `9` Active Sensing, `10` Reset,
`11` SysEx. So the only thing being dropped anywhere is Active Sensing, which is
what you want dropped.

Two consequences worth stating plainly, because both were previously beliefs.
**SysEx is not filtered on any port** — so nothing in the interface is eating
the MC6 or pedal SysEx we send. And **Realtime is not filtered either**, so MIDI
clock passes everywhere, which matters once Itajara is in the path.

**Per-channel: nothing filtered, nothing remapped, anywhere.** All 29 ports ×
16 channels were read — 464 entries of `GetFilterChannelIn/Out` and
`GetRemapChannelInSysEx1/Out` — and every single one is identical:

```json
[ [], [], { "channel": 1, "filters": [] }, { "channel": 1, "filters": [] } ]
```

Both filter arrays empty means no message type is dropped on any channel of any
port. (On a SysEx1 device the per-channel codes are `1` Note On/Off, `2` Poly
Key, `3` Control Change, `4` Program Change, `5` Channel Pressure, `6` Pitch
Bend — so an empty array is a real answer, not a missing one.)

The remap structs need one step of care, because a constant is exactly what a
broken getter looks like. `filters` in a remap lists *which message types get
remapped* — Auracle's own `ToggleRemap` pushes and pops type codes into that
array — so `filters: []` means the remap is inactive and the `channel` field
never applies. And `channel` reading **1 for every channel asked for** is what
settles it: a real identity mapping would report channel N for channel N, so a
flat 1 is an inert stored default rather than a routing decision. Remapping is
off across the whole device.

This is the reassuring outcome, and it was worth confirming rather than
assuming: a remap is the worst failure mode in the rig, because it delivers to
the *wrong* pedal instead of to none, and the symptom is a pedal that responds
to somebody else's knob.

## What would actually be worth having

Not all 207. The ones that bear on this app:

- `GetPortRoute` / `SetPortRoute` — the routing matrix. Turns "the app's output
  reaches the pedals" from belief into a check.
- `GetFilterChannelIn/Out`, `GetFilterSystemIn/Out` — a stray filter silently
  eats MIDI, which is the worst failure mode we have, because it looks like
  nothing happening.
- `GetRemapChannelIn/Out` — likewise, but worse: a remap makes messages arrive
  at the wrong pedal rather than not at all.
- `RestorePreset` / `GetPresetNumber` / `GetPresetName` — if the interface's
  whole configuration can be recalled by number, it can become part of a board
  preset, and the rig's routing changes with the sound.
- `GetMuteStatus`, `GetVolumeLevel` — a muted output explains silence that
  otherwise reads as a MIDI fault.

## The whole API surface

Extracted from the bundle by matching `libiConnectivity.<name>`. Argument order
is visible in the bundle too: each wrapper is
`device.foo = function(a, cb) { return window.libiConnectivity.Foo(this.id, cb, a) }`
— device id first, callback second, then the operation's own arguments.

### Preset (10)

`BackupPresets`, `GetCurrentPresetName`, `GetPresetMax`, `GetPresetName`, `GetPresetNameMax`, `GetPresetNumber`, `RestorePreset`, `RestorePresets`, `SavePreset`, `SetPresetName`

### Port (11)

`CreateVirtualPort`, `DestroyVirtualPort`, `GetDevMIDIPortInfo`, `GetPortConnectFlags`, `GetPortCount`, `GetPortHasInput`, `GetPortHasOutput`, `GetPortNameIn`, `GetPortNameOut`, `GetPortType`, `SetPortName`

### Route (3)

`AutoRoute`, `GetPortRoute`, `SetPortRoute`

### Filter (8)

`GetFilterChannelIn`, `GetFilterChannelOut`, `GetFilterSystemIn`, `GetFilterSystemOut`, `SetFilterChannelIn`, `SetFilterChannelOut`, `SetFilterSystemIn`, `SetFilterSystemOut`

### Remap (8)

`GetRemapChannelIn`, `GetRemapChannelInSysEx1`, `GetRemapChannelOut`, `GetRemapChannelOutSysEx1`, `SetRemapChannelIn`, `SetRemapChannelInSysEx1`, `SetRemapChannelOut`, `SetRemapChannelOutSysEx1`

### Mixer (28)

`GetHeadphoneSetupStatus`, `GetHighImpedanceStatus`, `GetMixerInputLevel`, `GetMixerInputMute`, `GetMixerInputPan`, `GetMixerInputSolo`, `GetMixerInputStereo`, `GetMixerMeterValue`, `GetMixerMeters`, `GetMixerOutputLevel`, `GetMixerOutputMute`, `GetMuteStatus`, `GetPhantomPowerStatus`, `GetStereoStatus`, `GetVolumeLevel`, `SetHighImpedanceStatus`, `SetMixerInputLevel`, `SetMixerInputMute`, `SetMixerInputPan`, `SetMixerInputSolo`, `SetMixerInputStereo`, `SetMixerOutputLevel`, `SetMixerOutputMute`, `SetMuteStatus`, `SetPhantomPowerStatus`, `SetStereoStatus`, `SetVolumeLevel`, `SetupHeadphones`

### Audio (24)

`AudioAutomationControlAssignCommand`, `GetAudio2Mode`, `GetAudio4Mode`, `GetAudioAutomationControlEntry`, `GetAudioBitDepth`, `GetAudioChannelsData`, `GetAudioClockSource`, `GetAudioPortDestinationMeters`, `GetAudioPortSourceMeters`, `GetAudioSampleRate`, `GetAudioTimeout`, `GetAudioTriggerChannel`, `GetAudioTriggerLevel`, `SetAudioAutomationControlEntry`, `SetAudioBitDepth`, `SetAudioClockSource`, `SetAudioSampleRate`, `SetAudioTimeout`, `SetAudioTriggerChannel`, `SetAudioTriggerLevel`, `SetupAudio2Base`, `SetupAudio2Mode`, `SetupAudio4Base`, `SetupAudio4Mode`

### Eth (19)

`GetEthCurrentIp`, `GetEthDynamicAddressing`, `GetEthIPAddressR`, `GetEthIPAddressX`, `GetEthPortNumber`, `GetEthPortNumberR`, `GetEthPortNumberX`, `GetEthSesnFlags`, `GetEthSesnName`, `GetEthSesnNameN`, `GetEthSesnNameR`, `GetEthSesnNameX`, `SetEthCurrentIp`, `SetEthDynamicAddressing`, `SetEthIPAddressR`, `SetEthPortNumberR`, `SetEthSesnFlags`, `SetEthSesnName`, `SetEthSesnNameR`

### USB (20)

`ClearUSBReservation`, `GetUSBDJackNumber`, `GetUSBDPortCount`, `GetUSBHMIDIPName`, `GetUSBHMIDIPortCount`, `GetUSBHMIDIPortCountIn`, `GetUSBHMIDIPortCountOut`, `GetUSBHMIDISerialNum`, `GetUSBHMIDIVName`, `GetUSBHostPortNumber`, `GetUSBHostProductName`, `GetUSBHostReserve`, `GetUSBHostSerialNumber`, `GetUSBHostVendorName`, `GetUSBReservePortNumber`, `GetUSBReserveProductName`, `GetUSBReserveSerialNumber`, `GetUSBReserveVendorName`, `SetUSBHID`, `SetUSBReservation`

### Failover (28)

`ClearFailoverAlarm`, `GetAutoArm`, `GetFailoverAlarm`, `GetFailoverArmed`, `GetFailoverClearAlarm`, `GetFailoverMidiPanic`, `GetFailoverMode`, `GetMidiRecoveryMode`, `GetMidiTimeout`, `GetMidiTriggerChannel`, `GetMidiTriggerLevel`, `GetRecoveryChannels`, `GetRecoveryFlags`, `GetRecoveryPorts`, `GetTriggerMode`, `SetAutoArm`, `SetFailoverArmed`, `SetFailoverClearAlarm`, `SetFailoverMidiPanic`, `SetFailoverMode`, `SetMidiRecoveryMode`, `SetMidiTimeout`, `SetMidiTriggerChannel`, `SetMidiTriggerLevel`, `SetRecoveryChannels`, `SetRecoveryFlags`, `SetRecoveryPorts`, `SetTriggerMode`

### Control (13)

`GetControlEnable`, `GetControlEvent`, `GetControlInput`, `GetControlInvert`, `GetControlMomentary`, `GetControlState`, `GetPanelButtonConfiguration`, `SetControlEnable`, `SetControlEvent`, `SetControlInput`, `SetControlInvert`, `SetControlMomentary`, `SetPanelButtonConfiguration`

### Scene (5)

`GetAutoSwitchScene`, `GetScene`, `SaveCurrentScene`, `SetAutoSwitchScene`, `SetScene`

### Device (10)

`ConnectSavedSessions`, `GetDevName`, `GetDevices`, `GetHardwareVersion`, `GetProductName`, `GetSerialNumber`, `IsConnected`, `ScanDevices`, `SetDevName`, `ShowDevTools`

### Firmware (9)

`DownloadFirmware`, `FactoryReset`, `GetFirmwareCapable`, `GetFirmwareVersion`, `GetIsBootloader`, `LoadLocalFirmware`, `Reboot`, `RebootBL`, `UpdateFirmware`

### Other (11)

`ClearAll`, `GetFilepath`, `GetMidiGob`, `GetSavedSession`, `RestoreGlobal`, `SaveGlobal`, `SetMidiGob`, `SetSavedSession`, `SetupBase`, `SetupKeyboardGroups`, `SetupOutputs`

