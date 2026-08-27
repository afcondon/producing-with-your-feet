-- | Itajara — the looper, as an entry in the pedal registry.
-- |
-- | **A name, a channel and a CC map, and deliberately nothing else since
-- | 2026-08-27.** It used to carry a donut layout as well, and drawing it was
-- | half of what this module did.
-- |
-- | What survives is the load-bearing part (`DESIGN-LOOPER` §2): because
-- | Itajara is a pedal addressed by CC, the MC6 assignment UI, board presets,
-- | the switch editor and the whole footswitch path work on it without any of
-- | them knowing it has no MIDI port. That was the point of registering it, and
-- | it costs almost no code — which is what this file is now.
-- |
-- | The surface is the Looper page, drawn from the daemon's snapshot. See
-- | `layout` below for why a second one was removed rather than updated.
module Pedals.Itajara (pedal) where

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi (CC, unsafeCC)
import Data.Pedal (PedalDef, PedalId(..))
import Data.Pedal.Engage (EngageConfig(..))
import Color (fromHexString)

cc :: Int -> CC
cc = unsafeCC

pedal :: PedalDef
pedal =
  { meta:
      { id: PedalId "itajara"
      , name: "Itajara"
      , shortName: "Ij"
      , brand: "Hylograph"
      , color: fromHexString "#3a7d6c"
      , defaultChannel: 13
      , saveInstructions: Nothing
      }
  , engage: SingleEngage (cc 7)
  , baseline: Map.empty
  , resetOrder: []
  , twister: Nothing
  , modes: Nothing
  , sections: []
  -- | **No donut, since 2026-08-27.** It had one — six knobs over a HATS tree,
  -- | as handsome as any pedal in the rig — and it described a machine this app
  -- | had outgrown: one loop, one length, a layer count. What Itajara is now is
  -- | eight loops, a tape mode, per-loop feedback and tone, and a controller
  -- | with four pages.
  -- |
  -- | The Looper page draws all of that from the daemon's snapshot. A second
  -- | picture of the same engine, built from CC values the app *sent*, can only
  -- | be the slower of the two and will sometimes disagree with it — which is
  -- | the exact failure the snapshot-only rule exists to prevent.
  -- |
  -- | **The pedal itself stays**, and that is the load-bearing half
  -- | (`DESIGN-LOOPER` §2): the MC6 addresses Itajara by CC exactly as it
  -- | addresses Habit, so the assignment UI, board presets and the whole
  -- | footswitch path go on working. Only the face is gone.
  , layout: Nothing
  }
