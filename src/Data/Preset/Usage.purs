-- | **How pedal presets and board presets refer to each other**, for the
-- | Presets page (plan `pwyf-layout.md`): which boards use a preset, what a
-- | board's chip says about one pedal, what every pedal is now, and whether a
-- | pedal has moved off the preset it was recalled from.
-- |
-- | Pure, so the page's claims are tested rather than eyeballed. The page is
-- | one direction of travel (shape a pedal, keep it, combine pedals into a
-- | board), and each step reads the one before it through this module.
module Data.Preset.Usage
  ( Chip(..)
  , boardsUsingPreset
  , chipFor
  , chipText
  , isPresetModified
  , inferEngageState
  , boardNow
  ) where

import Prelude

import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue, unsafeMidiValue)
import Data.Pedal (PedalDef, PedalId)
import Data.Pedal.Engage (EngageConfig(..), EngageState(..))
import Data.Preset (BoardPreset, BoardPresetEntry, PedalPreset, PresetId)
import Data.Tuple (Tuple(..))
import Engine (EngineState)

-- | Every board that recalls this preset. Shown beside the preset so that
-- | deleting or overwriting one is not a surprise to a board that uses it.
boardsUsingPreset :: PresetId -> Array BoardPreset -> Array BoardPreset
boardsUsingPreset presetId =
  Array.filter \board ->
    any (\entry -> entry.presetId == Just presetId) (Map.values board.pedals)

-- | What a board does to one pedal, as its chip says it.
data Chip
  -- | The board leaves this pedal alone (also: the pedal is not in the board).
  = ChipNoChange
  -- | Only switches it: on, off, or one side of a dual pedal.
  | ChipEngage EngageState
  -- | Recalls a preset, and then switches it as the entry says.
  | ChipPreset PedalPreset EngageState
  -- | Names a preset that has since been deleted. Said, rather than drawn as
  -- | "no change", because recalling the board will not do what it once did.
  | ChipMissingPreset EngageState

chipFor :: Array PedalPreset -> Maybe BoardPresetEntry -> Chip
chipFor presets = case _ of
  Nothing -> ChipNoChange
  Just entry -> case entry.presetId of
    Just presetId -> case Array.find (\p -> p.id == presetId) presets of
      Just preset -> ChipPreset preset entry.engage
      Nothing -> ChipMissingPreset entry.engage
    Nothing -> case entry.engage of
      EngageNoChange -> ChipNoChange
      engage -> ChipEngage engage

-- | The chip's words. A dual pedal's sides are named by its own labels
-- | (MOOD's "Wet" and "ML"), as the engage menu names them.
chipText :: EngageConfig -> Chip -> String
chipText config = case _ of
  ChipNoChange -> "\x2014\x2014"
  ChipEngage engage -> engageWord config engage
  ChipPreset preset _ -> preset.name
  ChipMissingPreset _ -> "missing preset"

engageWord :: EngageConfig -> EngageState -> String
engageWord config = case _ of
  EngageOn -> case config of
    SingleEngage _ -> "On"
    DualEngage _ -> "Both"
  EngageOff -> "Off"
  EngageA -> case config of
    DualEngage { a } -> a.label
    SingleEngage _ -> "On"
  EngageB -> case config of
    DualEngage { b } -> b.label
    SingleEngage _ -> "On"
  EngageNoChange -> "\x2014\x2014"

-- | Check if engine state has diverged from a preset's stored values.
-- |
-- | A slot reference holds no values, so it never reads as diverged: the app
-- | has nothing to compare it with.
isPresetModified :: EngineState -> PedalPreset -> Boolean
isPresetModified engine preset =
  case Map.lookup preset.pedalId engine of
    Nothing -> false
    Just ps ->
      let presetEntries = Map.toUnfoldable preset.values :: Array (Tuple CC MidiValue)
      in any (\(Tuple cc val) -> Map.lookup cc ps.values /= Just val) presetEntries

-- | Infer engage state from engine CC values for a pedal.
inferEngageState :: PedalDef -> Map CC MidiValue -> EngageState
inferEngageState def vals = case def.engage of
  SingleEngage cc ->
    if Map.lookup cc vals > Just (unsafeMidiValue 63) then EngageOn else EngageOff
  DualEngage { a, b } ->
    let aOn = Map.lookup a.cc vals > Just (unsafeMidiValue 63)
        bOn = Map.lookup b.cc vals > Just (unsafeMidiValue 63)
    in case aOn, bOn of
         true, true   -> EngageOn
         true, false  -> EngageA
         false, true  -> EngageB
         false, false -> EngageOff

-- | **What every pedal is now**, as a board: its engage state read off the
-- | engine, and the preset it was last recalled from. What "Save the board
-- | as…" and "Overwrite" keep, as the old builder grid's Save New did (it
-- | synced engage from the engine and carried the preset last chosen).
-- |
-- | A pedal the engine does not know is left out, which a board reads as
-- | "no change".
boardNow :: PedalRegistry -> EngineState -> Map PedalId PresetId -> Map PedalId BoardPresetEntry
boardNow registry engine recalled =
  Map.fromFoldable $ Array.mapMaybe entryFor (CRegistry.registryPedals registry)
  where
  entryFor def = do
    ps <- Map.lookup def.meta.id engine
    pure $ Tuple def.meta.id
      { presetId: Map.lookup def.meta.id recalled
      , engage: inferEngageState def ps.values
      }
