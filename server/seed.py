#!/usr/bin/env python3
"""Seed pwyf-store from the committed snapshots.

Two sources, and they do not share ids — importing the Chase Bliss file
regenerates uuids, so the same preset appears under different ids in each.
Identity is therefore (pedalId, name), and where both have a preset the
snapshot copy wins: it carries savedSlot and hand-written notes, and the
board presets reference *its* ids. Preferring the import copy would leave
every patch pointing at presets that no longer exist.

Usage:  python3 server/seed.py [store-dir] [--dry-run]
"""
import json
import os
import sys

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SNAP = os.path.join(REPO, "static", "preset-backup-2026-04-14")
IMPORT = os.path.join(REPO, "static", "cb-presets-import.json")
DEFAULT_STORE = "/Users/afc/work/afc-work/infovore-larder-db/data/producing-with-your-feet"


def load(path, default):
    try:
        with open(path) as f:
            return json.load(f)
    except FileNotFoundError:
        return default


def key(p):
    return (p.get("pedalId"), (p.get("name") or "").strip().lower())


def write_json(path, obj):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        json.dump(obj, f, indent=2, sort_keys=True)
        f.write("\n")


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    dry = "--dry-run" in sys.argv
    store = args[0] if args else DEFAULT_STORE

    snapshot = load(os.path.join(SNAP, "pedal-explorer-presets.json"), [])
    imported = load(IMPORT, [])
    patches = load(os.path.join(SNAP, "pedal-explorer-board-presets.json"), [])
    assigns = load(os.path.join(SNAP, "pedal-explorer-mc6-assignments.json"), [])

    seen = {key(p) for p in snapshot}
    extra = [p for p in imported if key(p) not in seen]
    presets = snapshot + extra

    print(f"store        : {store}")
    print(f"snapshot     : {len(snapshot)} presets (preferred)")
    print(f"import-only  : {len(extra)} presets added")
    print(f"presets total: {len(presets)}")
    print(f"patches      : {len(patches)}")
    print(f"assignments  : {len(assigns)}")

    # Every presetId a patch points at must exist, or the patch is broken.
    ids = {p["id"] for p in presets}
    dangling = [
        (pat.get("name"), pedal, e.get("presetId"))
        for pat in patches
        for pedal, e in (pat.get("pedals") or {}).items()
        if e.get("presetId") and e["presetId"] not in ids
    ]
    if dangling:
        print(f"\nWARNING: {len(dangling)} patch entries reference missing presets:")
        for name, pedal, pid in dangling[:10]:
            print(f"  {name} / {pedal} -> {pid}")
    else:
        print("\nreferential check: every patch reference resolves ✓")

    if dry:
        print("\n(dry run — nothing written)")
        return

    for p in presets:
        write_json(os.path.join(store, "presets", p["pedalId"], p["id"] + ".json"), p)
    for pat in patches:
        write_json(os.path.join(store, "patches", pat["id"] + ".json"), pat)
    write_json(os.path.join(store, "assignments.json"), assigns)

    print(f"\nwrote {len(presets)} presets, {len(patches)} patches, assignments.json")


if __name__ == "__main__":
    main()
