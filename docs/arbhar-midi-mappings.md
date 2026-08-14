# Instruo Arbhar — Full MIDI CC Mappings

Source: Instruo forum post (user-shared, not in official manual)

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

## Integration Notes

Arbhar accepts MIDI via TRS (Type A). Route from iConnectivity AUDIO4c or MC6 MIDI out.
Could be controlled from the producing-with-your-feet webapp alongside LoopyPro and MFT bindings.
