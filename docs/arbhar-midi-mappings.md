# Instruo Arbhar — Full MIDI CC Mappings

Source: shared by Instruo on Discord and a user forum post; not in the official manual

## Important Notes

- +/- controls are equivalent to positive/negative CV voltages
- Sending CC 127 on both reverb+ and reverb- simultaneously cancels out to 0
- All MIDI controls are **summed** with pot and CV settings
- For full MIDI range: set relevant pot fully CCW and disconnect CV

## CC Map

| CC | Parameter | Notes |
|----|-----------|-------|
| 0  | reverb+   | |
| 1  | delay+    | |
| 2  | spray     | |
| 3  | scan      | |
| 4  | length    | |
| 13 | hold+     | |
| 14 | hold-     | |
| 15 | panning+  | |
| 16 | panning-  | |
| 18 | reverb-   | |
| 20 | delay-    | |
| 23 | layer     | |
| 24 | drywet    | |
| 26 | direction | |
| 27 | dub       | |
| 28 | intensity | |
| 29 | texture   | |
| 30 | deviation | |

## What the table does not say

Checked against the firmware 2.0 manual on 2026-09-04: the manual has no
MIDI section at all, so this table is the only spec we hold. Three gaps:

- **No channel.** Whether the module listens on one channel or omni is
  unknown. If omni, it hears every pedal's CCs, and CC 0-4 are reverb,
  delay, spray, scan and length. Settle this before wiring it into the hub.
- **No capture, strike or load.** Nothing here records a layer or loads one
  from the stick; audio gets in by the panel, by the Capture CV input, or by
  the USB stick's `_arbhar_library` / `_arbhar_scenes` folders.
- **Layer (CC 23) is the one that matters for integration.** With the Layer
  knob fully anticlockwise it should select the layer by value, which is
  what lets the app keep the module and the screen agreeing about which
  sound is under the knob. Untested.

## Integration

Arbhar's USB port is a host port, so it takes a class-compliant USB MIDI
*device*: a Twister directly, or a USB-to-DIN MIDI cable whose DIN end sits
on the AUDIO4c's MIDI out, which the app drives as its own port. Not the
MC6's MIDI out, which carries every pedal's CCs. The adapter and the flash
drive share the one port.

**Decided 2026-09-04: no app UI for this interface.** The harvest tool in
`DESIGN-HARVEST.md` is about recording, the stick and loading, which is
where the button combinations cost time; the one thing MIDI adds after
that is polyphony, and a Keystep 37 in the USB port does it without
software. Section 8 of that document keeps the reasoning and the routing
in case it is reopened.
