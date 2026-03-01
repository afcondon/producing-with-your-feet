#!/usr/bin/env node
// Generate config JSON files from compiled PureScript pedal definitions.
// Run: node generate-config.js
// Requires: spago build first

const fs = require('fs');
const path = require('path');

// Import compiled PureScript modules
const Encode = require('./output/Config.Encode/index.js');
const Registry = require('./output/Pedals.Registry/index.js');
const Argonaut = require('./output/Data.Argonaut.Core/index.js');

const configDir = path.join(__dirname, 'config');
const pedalsDir = path.join(configDir, 'pedals');

// Ensure directories exist
fs.mkdirSync(pedalsDir, { recursive: true });

// Generate each pedal JSON
const pedalEntries = [];
for (const def of Registry.pedals) {
  const json = Encode.pedalDefToJson(def);
  const jsonStr = JSON.stringify(JSON.parse(Argonaut.stringify(json)), null, 2);
  const id = def.meta.id;  // PedalId newtype wraps a string
  const filename = `${id}.json`;
  const filepath = path.join(pedalsDir, filename);
  fs.writeFileSync(filepath, jsonStr + '\n');
  console.log(`  wrote config/pedals/${filename}`);
  pedalEntries.push({
    file: `pedals/${filename}`,
    channel: def.meta.defaultChannel
  });
}

// Generate rig.json
const rigConfig = {
  name: "Andrew's Board",
  storagePrefix: "pedal-explorer-",
  pedals: pedalEntries,
  midiRouting: {
    pedalOutput: { match: "Morningstar" },
    twisterInput: { match: "Midi Fighter Twister" },
    twisterOutput: { match: "Midi Fighter Twister" },
    loopyOutput: { match: "AUDIO4c" },
    loopyChannel: 16,
    mc6Input: { match: "Morningstar" }
  },
  slotRanges: [
    { brand: "Meris", range: { start: 0, count: 16 } },
    { brand: "Strymon", range: { start: 50, count: 26 } },
    { brand: "Chase Bliss", range: { start: 1, count: 122 } }
  ],
  looper: "loopers/loopypro.json",
  controller: "controllers/mc6-banks.json"
};

fs.writeFileSync(
  path.join(configDir, 'rig.json'),
  JSON.stringify(rigConfig, null, 2) + '\n'
);
console.log('  wrote config/rig.json');

console.log(`\nGenerated ${pedalEntries.length} pedal configs + rig.json`);
