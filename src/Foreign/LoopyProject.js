// LoopyProject FFI — generate .lpproj bundle in browser
// Uses sql.js (SQLite WASM) and JSZip loaded from CDN

// ---------------------------------------------------------------------------
// Configuration — matches Data.Loopy (4 groups, 8 loops)
// ---------------------------------------------------------------------------

const MIDI_CHANNEL = 16;
const DEVICE_ID = "f72bd24e";
const DEVICE_NAME = "AUDIO4c USB1";
const PROFILE_NAME = "Explorer";

const AUDIO_IFACE = "AppleUSBAudioEngine:iConnectivity:AUDIO4c:000010A8:1,2";
const AUDIO_SHORT = "AUDIO4c";
const AUDIO_CHANNEL = "#8 channel";

const GROUPS = [
  { label: "Orange", color: [0.996, 0.541, 0.035, 1] },
  { label: "Yellow", color: [1.0, 0.878, 0.090, 1] },
  { label: "Lime",   color: [0.675, 0.973, 0.110, 1] },
  { label: "Blue",   color: [0.220, 0.663, 0.871, 1] },
];

const ACTIONS = {
  record:        { cc: 20, identifier: "Track Record/Stop",  subject: "none" },
  playStop:      { cc: 21, identifier: "Track Play/Stop",    subject: "selected" },
  clear:         { cc: 22, identifier: "Clear Track",         subject: "selected" },
  mute:          { cc: 23, identifier: "Mute",               subject: "selected" },
  solo:          { cc: 24, identifier: "Track Solo",          subject: "selected" },
  multiply:      { cc: 25, identifier: "Multiply Track",      subject: "selected" },
  divide:        { cc: 26, identifier: "Divide Track",        subject: "selected" },
  prev:          { cc: 27, identifier: "Track Select",        subject: "global" },
  next:          { cc: 28, identifier: "Track Select",        subject: "global" },
  undo:          { cc: 57, identifier: "Undo",                subject: "global" },
  redo:          { cc: 58, identifier: "Redo",                subject: "global" },
  play:          { cc: 59, identifier: "Track Play",           subject: "selected" },
  stop:          { cc: 60, identifier: "Track Stop",           subject: "selected" },
  playImmediate: { cc: 61, identifier: "Track Play Immediate", subject: "selected" },
  stopImmediate: { cc: 62, identifier: "Track Stop Immediate", subject: "selected" },
};

const SELECT_CC_BASE = 30;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function ref(type, id) {
  return { _type: type, _value: id };
}

function colorVal(rgba) {
  return { _type: "Color", _value: rgba };
}

function setVal(arr) {
  return { _type: "Set", _value: arr };
}

function rectVal(x, y, w, h) {
  return { _type: "Rect", _value: [x, y, w, h] };
}

function ccTrigger(channel, cc, value) {
  const status = 0xB0 | (channel - 1);
  return [status, cc, value].map((b) => b.toString(16).padStart(2, "0")).join("");
}

// Continuous CC trigger — just status + CC, no value byte (matches any value)
function ccTriggerContinuous(channel, cc) {
  const status = 0xB0 | (channel - 1);
  return [status, cc].map((b) => b.toString(16).padStart(2, "0")).join("");
}

// ---------------------------------------------------------------------------
// Build object graph
// ---------------------------------------------------------------------------

function buildObjectGraph() {
  let nextId = 1;
  const objects = [];

  function addObject(type, contents) {
    const id = nextId++;
    objects.push({ id, type, contents: JSON.stringify(contents) });
    return id;
  }

  const outputConfigs = {
    [AUDIO_SHORT]: "{2-3}",
    [AUDIO_CHANNEL]: "{2-3}",
    [AUDIO_IFACE]: "{2-3}",
  };

  // 1. HardwareAudioSource
  const audioSourceId = addObject("HardwareAudioSource", {
    hidden: true,
    mutedAutomatically: false,
    enabled: true,
    sends: [],
    muted: false,
    gain: 1,
    inputConfigurations: {
      [AUDIO_SHORT]: "{0-1}",
      [AUDIO_CHANNEL]: "{0-1}",
      [AUDIO_IFACE]: "{0-1}",
    },
  });

  // 2. Groups + Tracks
  const groupIds = [];
  const allTrackIds = [];
  const tracksByGroup = [];

  for (let gi = 0; gi < GROUPS.length; gi++) {
    const group = GROUPS[gi];

    const trackAId = addObject("Track", {
      volume: 1, pitch: 0, rate: 1, speed: 1, gain: 1,
      renderedPitch: 0, renderedSpeed: 1, loop: true,
      layers: [], sends: [],
      frame: rectVal(gi, 0, 1, 1),
    });

    const trackBId = addObject("Track", {
      volume: 1, pitch: 0, rate: 1, speed: 1, gain: 1,
      renderedPitch: 0, renderedSpeed: 1, loop: true,
      layers: [], sends: [],
      frame: rectVal(gi, 1, 1, 1),
    });

    allTrackIds.push(trackAId, trackBId);
    tracksByGroup.push([trackAId, trackBId]);

    const groupId = addObject("Group", {
      tracks: setVal([ref("Track", trackAId), ref("Track", trackBId)]),
      volume: 1, rate: 1, speed: 1, gain: 1, inputGain: 1,
      color: colorVal(group.color),
      sends: [],
      previouslyPlayingTracks: setVal([]),
      outputConfigurations: outputConfigs,
    });

    groupIds.push(groupId);
  }

  // 3. Scene
  const sceneTrackRefs = [];
  for (let row = 0; row < 2; row++) {
    for (let col = 0; col < GROUPS.length; col++) {
      sceneTrackRefs.push(ref("Track", tracksByGroup[col][row]));
    }
  }

  const sceneId = addObject("Scene", {
    tracks: sceneTrackRefs,
    rows: 2,
    columns: GROUPS.length,
    widgets: [],
    playGroups: [],
    orientation: 4,
  });

  // 4. Clock
  const clockId = addObject("Clock", {
    isDerivedFromInitialLoop: false,
    masterLength: 2,
    beatsPerBar: 4,
    loopRegionEnabled: false,
    tempo: 0,
  });

  // 5. MasterOutputChannel
  const masterOutputId = addObject("MasterOutputChannel", {
    tracksGain: 1,
    gain: 1,
    postFaderEffects: [],
  });

  // 6. Sequence
  const seqTrackIds = [];
  for (const trackId of allTrackIds) {
    const stId = addObject("TrackPlaybackSequenceTrack", {
      track: ref("Track", trackId),
      segments: [],
    });
    seqTrackIds.push(stId);
  }

  const sequenceId = addObject("Sequence", {
    enabled: false,
    sort: 1,
    sequenceTracks: seqTrackIds.map((id) => ref("TrackPlaybackSequenceTrack", id)),
  });

  // 7. Project
  addObject("Project", {
    version: 1,
    scenes: [ref("Scene", sceneId)],
    currentScene: ref("Scene", sceneId),
    groups: groupIds.map((id) => ref("Group", id)),
    audioSources: [ref("HardwareAudioSource", audioSourceId)],
    clock: ref("Clock", clockId),
    masterOutputChannel: ref("MasterOutputChannel", masterOutputId),
    sequence: ref("Sequence", sequenceId),
    activeControlProfileNames: setVal([PROFILE_NAME]),
    midiSources: [],
    audioBuses: [],
    auxiliaryOutputBuses: [],
    effects: [],
    mixerScrollOffset: 0,
    lastConnectedAudioInterface: `${AUDIO_IFACE}:${AUDIO_IFACE}`,
    storedUserDefaults: {
      recordCountOut: -2,
      tailRecording: false,
      overdubCountIn: -1000,
      sceneSwitchQuantization: -2,
      retrospectiveQuantization: true,
      playToggleOnTap: true,
      playQuantization: -2,
      timeFitType: 1,
      audioUnitHostSync: false,
      minimumDetectionTempo: 75,
      timeFitOneShots: 2,
      simultaneousRecording: false,
      recordTracksIfEmpty: true,
      overdubCountOut: -1000,
      orientationLock: true,
      metronomeVolume: 1,
      maximumDetectionTempo: 150,
      retrospectiveLength: -1,
      quantizedRecording: true,
      audioThresholdRecording: false,
      audioThresholdRecordingThreshold: 0.025,
      resetGesturesAndFollowActions: true,
      padLoopLength: false,
      oneShotAudioThresholdRecording: true,
      recordOnPress: false,
      MIDIClockSyncStartStop: true,
      fadeOutDuration: 0,
      synchronizeLoops: true,
      recordEndAction: 0,
      stopQuantization: -2,
      oneShotQuantization: 0,
      pauseClockWhenIdle: false,
      recordCountIn: -2,
      recordAutoCountOut: false,
      fadeInDuration: 0,
      midiControlEventReplacementMode: 1,
      retrospectiveRecording: false,
      recordingWaitsForPlayback: false,
      extendedMixer: false,
      automaticLoopDetection: false,
      showMixerOnSequenceScreen: true,
      loopCrossfadeDuration: 0.05,
      introRecording: false,
      showMixerOnPlayScreen: true,
      mergeDragEnabled: true,
      recordPhaseShift: true,
      holdToPlayOneShots: true,
      automaticallyEndDetectedLoops: false,
      metronomeSound: "1",
    },
  });

  return { objects, allTrackIds };
}

// ---------------------------------------------------------------------------
// Build MIDI bindings plist
// ---------------------------------------------------------------------------

function buildBindings(allTrackIds) {
  function bindingEntry(trigger, actionId, subject, params) {
    params = params || {};
    const paramKeys = Object.entries(params);
    let paramsXml;
    if (paramKeys.length === 0) {
      paramsXml = "\t\t\t\t\t<dict/>";
    } else {
      const inner = paramKeys
        .map(([k, v]) => {
          if (typeof v === "boolean") {
            return `\t\t\t\t\t\t<key>${k}</key>\n\t\t\t\t\t\t<${v}/>`;
          }
          const valTag = typeof v === "number" ? "integer" : "string";
          return `\t\t\t\t\t\t<key>${k}</key>\n\t\t\t\t\t\t<${valTag}>${v}</${valTag}>`;
        })
        .join("\n");
      paramsXml = `\t\t\t\t\t<dict>\n${inner}\n\t\t\t\t\t</dict>`;
    }

    const subjectXml = subject === null ? "" :
      `\n\t\t\t\t\t<key>Subject</key>\n\t\t\t\t\t<string>${subject}</string>`;

    return `\t\t<dict>
\t\t\t<key>Actions</key>
\t\t\t<array>
\t\t\t\t<dict>
\t\t\t\t\t<key>Identifier</key>
\t\t\t\t\t<string>${actionId}</string>
\t\t\t\t\t<key>Parameters</key>
${paramsXml}${subjectXml}
\t\t\t\t\t<key>Timing</key>
\t\t\t\t\t<string>Sequential</string>
\t\t\t\t</dict>
\t\t\t</array>
\t\t\t<key>Trigger</key>
\t\t\t<string>${trigger}</string>
\t\t</dict>`;
  }

  const bindings = [];

  // Action bindings
  for (const [key, action] of Object.entries(ACTIONS)) {
    const trigger = ccTrigger(MIDI_CHANNEL, action.cc, 127);
    let subject;
    if (action.subject === "none") subject = null;
    else if (action.subject === "global") subject = "";
    else subject = "Selected Track";

    let params = {};
    if (key === "solo") {
      params = { Solo: true, "Record If Empty": false };
    }

    bindings.push(bindingEntry(trigger, action.identifier, subject, params));
  }

  // Select-loop bindings
  for (let i = 0; i < allTrackIds.length; i++) {
    const trigger = ccTrigger(MIDI_CHANNEL, SELECT_CC_BASE + i, 127);
    const loopyPos = (i % 2) * GROUPS.length + Math.floor(i / 2);
    bindings.push(bindingEntry(trigger, "Track Select", String(loopyPos)));
  }

  // Volume per loop (CC 40-47) — continuous, bound to specific tracks
  const VOLUME_CC_BASE = 40;
  for (let i = 0; i < allTrackIds.length; i++) {
    const trigger = ccTriggerContinuous(MIDI_CHANNEL, VOLUME_CC_BASE + i);
    const loopyPos = (i % 2) * GROUPS.length + Math.floor(i / 2);
    bindings.push(bindingEntry(trigger, "Track Parameter", String(loopyPos), { Parameter: "Volume" }));
  }

  // Parameter CCs for selected loop (from loopConfigBank)
  // Speed (53), Reverse (56), FadeIn (54), FadeOut (55), Overdub (80)
  const PARAM_BINDINGS = [
    { cc: 53, parameter: "Speed",              subject: "selected" },
    { cc: 56, parameter: "Reverse",            subject: "selected" },
    { cc: 54, parameter: "Fade In Duration",   subject: "selected" },
    { cc: 55, parameter: "Fade Out Duration",  subject: "selected" },
    { cc: 80, parameter: "Overdub Level",      subject: "selected" },
    { cc: 81, parameter: "Phase Lock",         subject: "selected" },
    { cc: 82, parameter: "Threshold Recording", subject: "selected" },
  ];

  for (const param of PARAM_BINDINGS) {
    const trigger = ccTriggerContinuous(MIDI_CHANNEL, param.cc);
    const subject = param.subject === "selected" ? "Selected Track" : "";
    bindings.push(bindingEntry(trigger, "Track Parameter", subject, { Parameter: param.parameter }));
  }

  // Pan per loop (CC 70-77) — continuous, bound to specific tracks
  const PAN_CC_BASE = 70;
  for (let i = 0; i < allTrackIds.length; i++) {
    const trigger = ccTriggerContinuous(MIDI_CHANNEL, PAN_CC_BASE + i);
    const loopyPos = (i % 2) * GROUPS.length + Math.floor(i / 2);
    bindings.push(bindingEntry(trigger, "Track Parameter", String(loopyPos), { Parameter: "Pan" }));
  }

  return bindings;
}

function buildControllerProfile(bindings) {
  return `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
\t<key>Bindings</key>
\t<array>
${bindings.join("\n")}
\t</array>
\t<key>Device</key>
\t<string>${DEVICE_ID}</string>
\t<key>Device Name</key>
\t<string>${DEVICE_NAME}</string>
\t<key>Type</key>
\t<string>MIDI</string>
</dict>
</plist>
`;
}

function buildInfoPlist() {
  return `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
\t<key>Build</key>
\t<string>725</string>
\t<key>Date</key>
\t<date>${new Date().toISOString()}</date>
\t<key>Version</key>
\t<integer>0</integer>
</dict>
</plist>
`;
}

function buildResourcesPlist() {
  return `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<array/>
</plist>
`;
}

// ---------------------------------------------------------------------------
// FFI export — generate and download .lpproj
// ---------------------------------------------------------------------------

export const generateAndDownloadImpl = function (projectName) {
  return function (onError) {
    return function (onSuccess) {
      return function () {
        (async function () {
          try {
            // Check dependencies
            if (typeof initSqlJs === "undefined") {
              throw new Error("sql.js not loaded — check CDN script tag");
            }
            if (typeof JSZip === "undefined") {
              throw new Error("JSZip not loaded — check CDN script tag");
            }

            // Initialize sql.js
            const SQL = await initSqlJs({
              locateFile: function (file) {
                return "https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.10.3/" + file;
              }
            });

            // Build the project
            const { objects, allTrackIds } = buildObjectGraph();
            const bindings = buildBindings(allTrackIds);

            // Create SQLite database
            const db = new SQL.Database();
            db.run("CREATE TABLE objects (id INTEGER PRIMARY KEY, type TEXT, contents BLOB);");
            db.run("CREATE TABLE meta (name TEXT UNIQUE, value BLOB);");
            db.run("CREATE INDEX types ON objects (type);");
            db.run("INSERT INTO meta (name, value) VALUES ('sequence', 67);");

            for (const obj of objects) {
              db.run(
                "INSERT INTO objects (id, type, contents) VALUES (?, ?, ?);",
                [obj.id, obj.type, obj.contents]
              );
            }

            const dbData = db.export();
            db.close();

            // Build zip
            const zip = new JSZip();
            zip.file("Info.plist", buildInfoPlist());
            zip.file("Resources.plist", buildResourcesPlist());
            zip.file("Project.sqlite", dbData);

            const profilePath = "Control Profiles/" + PROFILE_NAME +
              ".lpcontrolprofile/" + DEVICE_ID + ".MIDI." + DEVICE_NAME + ".controllerprofile";
            zip.file(profilePath, buildControllerProfile(bindings));

            const blob = await zip.generateAsync({ type: "blob" });

            // Download
            var url = URL.createObjectURL(blob);
            var a = document.createElement("a");
            a.href = url;
            a.download = projectName + ".lpproj";
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);

            onSuccess()();
          } catch (e) {
            onError(e)();
          }
        })();
      };
    };
  };
};
