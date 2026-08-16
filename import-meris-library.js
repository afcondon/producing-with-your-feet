#!/usr/bin/env node
// Turn the Meris editors' factory libraries into presets this app can import.
// Run: node import-meris-library.js   -> static/meris-presets-import.json
//
// The two Meris pedals are the reason slot references exist (Data.Preset):
// their sixteen presets have names, but only inside Meris's own editor, so the
// app could point at slot 9 without being able to say what slot 9 was. It
// turns out the editors ship their factory library as plain JSON inside the
// application bundle -- names, descriptions and every parameter value -- which
// is a library of real sounds rather than a list of numbers.
//
// Two things this is NOT:
//
//   * It is not the contents of the pedals. This is what Meris shipped; the
//     sixteen slots on the board were chosen by hand years ago. So nothing
//     here sets `savedSlot`, even though the descriptions name a factory slot
//     -- claiming slot 9 holds "SubTerra" would be exactly the kind of belief
//     that DESIGN-v2 spends its length trying to keep honest. The factory
//     number is kept in the description, where it informs without asserting.
//
//   * It is not the whole patch. Meris stores every parameter twice, as a
//     ToeUp/ToeDown pair bracketing an expression sweep, and this app has one
//     value per CC. We take ToeUp -- the heel, the sound at rest, what you
//     hear with the pedal not touched -- and drop the toe half. If expression
//     is ever modelled, the other half is still sitting in the bundle.

const fs = require('fs');
const path = require('path');

// Parameter names as the Meris editors write them, against the CCs this app
// already declares. Every one of these was checked against config/pedals/*.json
// and the script refuses to run if that stops being true.
const PEDALS = [
  {
    pedalId: 'hedra',
    app: '/Applications/MIDI apps/HedraEdit.app',
    ccs: {
      Key: 16,
      MicroTune: 17,
      Mix: 18,
      Pitch_1: 19,
      Pitch_2: 20,
      Pitch_3: 21,
      ScaleType: 22,
      PitchCorrectionAndGlide: 23,
      DelayFeedback: 24,
      TimeDivision_1: 25,
      TimeDivision_2: 26,
      TimeDivision_3: 27,
      HalfSpeed: 9,
      DelayMode: 29,
      PitchControlSmoothing: 30,
      Tempo: 15,
    },
  },
  {
    pedalId: 'mercury7',
    app: '/Applications/MIDI apps/Mercury7Edit.app',
    ccs: {
      SpaceDecay: 16,
      Modulate: 17,
      Mix: 18,
      LoFrequency: 19,
      PitchVector: 20,
      HiFrequency: 21,
      PreDelay: 22,
      ModSpeed: 23,
      PitchVectorMix: 24,
      Density: 25,
      AttackTime: 26,
      VibratoDepth: 27,
      Swell: 28,
      Algorithm: 29,
    },
  },
];

// Bypass is deliberately not mapped. It is in every patch, and importing it
// would mean auditioning a preset could silently switch the pedal off. The app
// treats engage as board-level state, and `autoEngageIfNeeded` turns a pedal on
// precisely when a recalled preset says nothing about it -- which is the
// behaviour we want here.
const DROPPED = new Set(['Bypass', 'Unused1', 'Unused2']);

// The instructional entries the editors ship in place of a tutorial. Not sounds.
const isInstructional = (name) => /^AA\d+ /.test(name);

// Fixed so re-running produces a byte-identical file; the import path dedupes
// on id, so a second import of an unchanged library is a no-op.
const STAMP = '2026-08-16T00:00:00.000Z';

const slug = (s) =>
  s.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');

function declaredCCs(pedalId) {
  const cfg = JSON.parse(
    fs.readFileSync(path.join(__dirname, 'config', 'pedals', `${pedalId}.json`), 'utf8')
  );
  const out = new Set();
  for (const section of cfg.sections || []) {
    for (const control of section.controls || []) {
      if (typeof control.cc === 'number') out.add(control.cc);
    }
  }
  return out;
}

const presets = [];
const report = [];

for (const pedal of PEDALS) {
  const libPath = path.join(pedal.app, 'Contents', 'Resources', 'default_library.json');
  const library = JSON.parse(fs.readFileSync(libPath, 'utf8'));
  const declared = declaredCCs(pedal.pedalId);

  // Fail rather than import a preset addressed at a CC the pedal config has
  // never heard of; a wrong CC is a wrong sound, sent silently.
  for (const [param, cc] of Object.entries(pedal.ccs)) {
    if (!declared.has(cc)) {
      throw new Error(`${pedal.pedalId}: ${param} -> CC ${cc}, which config/pedals/${pedal.pedalId}.json does not declare`);
    }
  }

  let count = 0;
  for (const [name, entry] of Object.entries(library)) {
    if (isInstructional(name)) continue;

    const values = {};
    for (const [key, value] of Object.entries(entry.patch || {})) {
      if (key.endsWith('_ToeDown')) continue;
      const param = key.replace(/_ToeUp$/, '');
      if (DROPPED.has(param)) continue;
      const cc = pedal.ccs[param];
      // A parameter we have no CC for means Meris changed the library and we
      // would be importing a partial sound while looking complete.
      if (cc === undefined) {
        throw new Error(`${pedal.pedalId}: patch "${name}" has unmapped parameter ${param}`);
      }
      values[cc] = value;
    }

    presets.push({
      id: `meris-${pedal.pedalId}-${slug(name)}`,
      pedalId: pedal.pedalId,
      name,
      description: entry.descr || '',
      notes: `Meris factory library, from ${path.basename(pedal.app)}. Heel (toe-up) values; the toe-down half of each expression pair is not captured.`,
      values,
      info: {},
      created: STAMP,
      modified: STAMP,
    });
    count += 1;
  }
  report.push(`${pedal.pedalId}: ${count} presets, ${Object.keys(pedal.ccs).length} parameters mapped`);
}

const out = path.join(__dirname, 'static', 'meris-presets-import.json');
fs.writeFileSync(out, JSON.stringify(presets, null, 2) + '\n');
for (const line of report) console.log(line);
console.log(`wrote ${presets.length} presets to ${path.relative(__dirname, out)}`);
