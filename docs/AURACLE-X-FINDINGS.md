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

## So the way in is their own DevTools

With no file to decode, the route is the live protocol — and Auracle hands us a
console onto it. `window.libiConnectivity.ShowDevTools()` is called
unconditionally in the root component's `render`, so DevTools is either already
available in the running app or one right-click away.

From that console every one of the 207 operations can be invoked by hand, with
`window.libiConnectivity.GetPortRoute(id, cb, port)` and friends, while our own
sniffer (`static/sniff.html`) listens to the device. CoreMIDI delivers a
source's data to every connected client, so we see the device's replies to
Auracle even though we cannot see Auracle's requests — and a reply generally
carries the command code that provoked it. One operation at a time, deliberately
triggered, with the reply captured: that is a cleaner correlation than the MC6
sweep ever gave us.

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

