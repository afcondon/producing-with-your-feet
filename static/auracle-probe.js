// Read the Audio4c's configuration out of Auracle X, from Auracle's own console.
//
// The Audio4c has no backup file -- Auracle's .syx Backup/Restore is gated on
// isSysEx2() and this is a SysEx1 device -- so the only way to see what the
// interface is actually configured to do is to ask it. Auracle already knows
// how; `window.libiConnectivity` is the native binding it uses, and
// ShowDevTools() runs unconditionally in its root render, so the console is
// right there.
//
// HOW TO RUN
//   1. Open Auracle X and let it find the Audio4c.
//   2. Open DevTools (right-click -> Inspect, or it may already be open).
//   3. Paste this whole file into the console and press return.
//   4. It prints a JSON object and puts it on the clipboard via copy().
//
// EVERY CALL HERE IS A Get. Nothing is written, no preset is saved, no route is
// changed. The one hazard in this whole file is running it against the wrong
// device, and it prints which one it found before doing anything else.
//
// Why bother, given the app can't do any of this yet: the routing matrix,
// channel filters and channel remaps decide whether a message we send ever
// reaches a pedal. Right now that is the largest thing in the rig we believe
// rather than check, and unlike the pedals it is knowable.

(async () => {
  const lib = window.libiConnectivity;
  if (!lib) {
    console.error('window.libiConnectivity is missing -- is this Auracle X\'s console?');
    return;
  }

  // Every binding takes (deviceId, callback, ...args) and answers once. Wrapping
  // them as promises is the only reason this reads sequentially.
  const call = (name, id, ...args) =>
    new Promise((resolve) => lib[name](id, resolve, ...args));

  const devices = await new Promise((resolve) => lib.GetDevices(0, resolve));
  if (!devices || !devices.length) {
    console.error('No devices. Is the Audio4c connected and has Auracle finished scanning?');
    return;
  }
  console.log('devices:', devices);

  const id = devices.find((d) => d.productId === 'icaudio4c') || devices[0];
  console.log('probing:', id);

  const out = { device: id, identity: {}, presets: {}, ports: [], systemFilters: [] };

  out.identity = {
    productName: await call('GetProductName', id),
    serialNumber: await call('GetSerialNumber', id),
    deviceName: await call('GetDevName', id),
    firmware: await call('GetFirmwareVersion', id),
    hardware: await call('GetHardwareVersion', id),
  };

  // How many preset slots there really are, which the UI does not say on a
  // SysEx1 device -- it hides both the slot count and the "Currently Loaded"
  // line. If presetMax is 1 that settles it.
  const presetMax = await call('GetPresetMax', id);
  out.presets = {
    max: presetMax,
    current: await call('GetPresetNumber', id),
    names: [],
  };
  for (let n = 1; n <= (presetMax || 0) && n <= 32; n++) {
    out.presets.names.push({ number: n, name: await call('GetPresetName', id, n) });
  }

  const portCount = await call('GetPortCount', id);
  out.portCount = portCount;

  for (let p = 1; p <= (portCount || 0); p++) {
    // `route` is the interesting one: SetPortRoute takes a vector<int>, so this
    // is the list of ports this one feeds. The whole matrix, one row at a time.
    out.ports.push({
      port: p,
      type: await call('GetPortType', id, p),
      nameIn: await call('GetPortNameIn', id, p),
      nameOut: await call('GetPortNameOut', id, p),
      hasInput: await call('GetPortHasInput', id, p),
      hasOutput: await call('GetPortHasOutput', id, p),
      connectFlags: await call('GetPortConnectFlags', id, p),
      route: await call('GetPortRoute', id, p),
    });
    out.systemFilters.push({
      port: p,
      in: await call('GetFilterSystemIn', id, p),
      out: await call('GetFilterSystemOut', id, p),
    });
  }

  // Per-channel filters and remaps are portCount * 16 * 2 round trips, which on
  // a fourteen-port device is several hundred SysEx exchanges. Off by default so
  // the first run stays quick; set this true once the shape above looks right.
  const DEEP = false;
  if (DEEP) {
    out.channels = [];
    for (let p = 1; p <= (portCount || 0); p++) {
      for (let ch = 1; ch <= 16; ch++) {
        out.channels.push({
          port: p,
          channel: ch,
          filterIn: await call('GetFilterChannelIn', id, p, ch),
          filterOut: await call('GetFilterChannelOut', id, p, ch),
          remapIn: await call('GetRemapChannelInSysEx1', id, p, ch),
          remapOut: await call('GetRemapChannelOutSysEx1', id, p, ch),
        });
      }
    }
  }

  console.log(out);
  const text = JSON.stringify(out, null, 2);
  try {
    copy(text);
    console.log(`\n--- ${text.length} bytes on the clipboard ---`);
  } catch (e) {
    console.log('copy() unavailable; the JSON is above.');
  }
  window.__auracle = out;
})();
