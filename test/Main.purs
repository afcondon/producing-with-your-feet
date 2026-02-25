module Test.Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Midi (makeCC, makeMidiValue, makeProgramNumber)
import Data.Pedal (PedalId(..))
import Data.Pedal.Engage (EngageState(..))
import Effect (Effect)
import Effect.Console (log)
import Engine (initEngine)
import Engine.Storage (engineToJson, parseEngine, parseCardOrder, parsePresets, parseBoardPresets, parseEngageState)
import Pedals.Registry as Registry

-- Golden fixture: JS-format engine state with 3 pedals, numeric-string CC keys
engineFixture :: String
engineFixture = """{"mood":{"channel":3,"values":{"14":64,"15":100,"16":50},"info":{}},"flint":{"channel":2,"values":{"13":80,"14":127,"15":0},"info":{}},"onward":{"channel":1,"values":{"14":32,"15":90,"16":60},"info":{}}}"""

cardOrderFixture :: String
cardOrderFixture = """["onward","mood","flint"]"""

presetsFixture :: String
presetsFixture = """[{"id":"preset-1","pedalId":"mood","name":"Ambient Wash","description":"Lush reverb with slow modulation","notes":"","values":{"14":64,"15":100,"16":50},"savedSlot":null,"created":"2025-01-15T10:30:00Z","modified":"2025-01-15T10:30:00Z"},{"id":"preset-2","pedalId":"flint","name":"Spring Clean","description":"Bright spring reverb","notes":"Good for country","values":{"13":80,"14":127},"savedSlot":2,"created":"2025-01-16T14:00:00Z","modified":"2025-02-01T09:15:00Z"}]"""

boardPresetsFixture :: String
boardPresetsFixture = """[{"id":"board-1","name":"Full Board","description":"All pedals on","notes":"","pedals":{"mood":{"presetId":"preset-1","engage":"on"},"flint":{"presetId":null,"engage":"off"},"onward":{"presetId":null,"engage":"no-change"}},"created":"2025-01-20T12:00:00Z","modified":"2025-01-20T12:00:00Z"}]"""

assert :: String -> Boolean -> Effect Unit
assert label ok = log $ (if ok then "PASS" else "FAIL") <> " - " <> label

main :: Effect Unit
main = do
  log "Running pedal definition tests..."

  -- All 12 pedals registered
  let count = Array.length Registry.pedals
  assert "Pedal count is 12" (count == 12)

  -- Each pedal has a non-empty baseline
  let allHaveBaseline = Array.all (\p -> not (Map.isEmpty p.baseline)) Registry.pedals
  assert "All pedals have baselines" allHaveBaseline

  -- Each pedal has at least one section
  let allHaveSections = Array.all (\p -> not (Array.null p.sections)) Registry.pedals
  assert "All pedals have sections" allHaveSections

  -- findPedal works
  assert "findPedal MOOD" (isJust (Registry.findPedal (PedalId "mood")))
  assert "findPedal nonexistent returns Nothing" (isNothing (Registry.findPedal (PedalId "nonexistent")))

  log ""
  log "Running decoder tests..."

  -- 1. Parse engine fixture
  let mEngine = parseEngine engineFixture
  assert "Engine fixture parses" (isJust mEngine)
  case mEngine of
    Nothing -> log "  (skipping engine detail checks)"
    Just engine -> do
      assert "Engine has 3 pedals" (Map.size engine == 3)
      -- Check mood channel
      case Map.lookup (PedalId "mood") engine of
        Nothing -> assert "Mood pedal found" false
        Just mood -> do
          assert "Mood channel is 3" (mood.channel == 3)
          case makeCC 14 >>= \cc -> Map.lookup cc mood.values of
            Nothing -> assert "Mood CC14 found" false
            Just mv -> assert "Mood CC14 value is 64" (mv == unsafeMV 64)
          case makeCC 15 >>= \cc -> Map.lookup cc mood.values of
            Nothing -> assert "Mood CC15 found" false
            Just mv -> assert "Mood CC15 value is 100" (mv == unsafeMV 100)
      -- Check onward channel
      case Map.lookup (PedalId "onward") engine of
        Nothing -> assert "Onward pedal found" false
        Just onward -> do
          assert "Onward channel is 1" (onward.channel == 1)
          case makeCC 16 >>= \cc -> Map.lookup cc onward.values of
            Nothing -> assert "Onward CC16 found" false
            Just mv -> assert "Onward CC16 value is 60" (mv == unsafeMV 60)

  -- 2. Round-trip: encode then parse initEngine
  let roundTripped = parseEngine (stringify (engineToJson initEngine))
  assert "Round-trip: encode then parse initEngine" (roundTripped == Just initEngine)

  -- 3. Parse card order
  let mOrder = parseCardOrder cardOrderFixture
  assert "Card order parses" (isJust mOrder)
  assert "Card order is [onward, mood, flint]"
    (mOrder == Just [PedalId "onward", PedalId "mood", PedalId "flint"])

  -- 4. Parse presets
  let mPresets = parsePresets presetsFixture
  assert "Presets fixture parses" (isJust mPresets)
  case mPresets of
    Nothing -> log "  (skipping preset detail checks)"
    Just presets -> do
      assert "2 presets parsed" (Array.length presets == 2)
      case Array.index presets 0 of
        Nothing -> assert "First preset exists" false
        Just p1 -> do
          assert "First preset id" (p1.id == "preset-1")
          assert "First preset pedalId" (p1.pedalId == PedalId "mood")
          assert "First preset name" (p1.name == "Ambient Wash")
          assert "First preset savedSlot is Nothing" (isNothing p1.savedSlot)
      case Array.index presets 1 of
        Nothing -> assert "Second preset exists" false
        Just p2 -> do
          assert "Second preset id" (p2.id == "preset-2")
          assert "Second preset savedSlot is Just 2" (p2.savedSlot == makeProgramNumber 2)
          assert "Second preset notes" (p2.notes == "Good for country")

  -- 5. Parse board presets
  let mBoards = parseBoardPresets boardPresetsFixture
  assert "Board presets fixture parses" (isJust mBoards)
  case mBoards of
    Nothing -> log "  (skipping board preset detail checks)"
    Just boards -> do
      assert "1 board preset parsed" (Array.length boards == 1)
      case Array.index boards 0 of
        Nothing -> assert "First board exists" false
        Just b -> do
          assert "Board id" (b.id == "board-1")
          assert "Board has 3 pedal entries" (Map.size b.pedals == 3)
          case Map.lookup (PedalId "mood") b.pedals of
            Nothing -> assert "Mood board entry found" false
            Just e -> do
              assert "Mood engage is On" (e.engage == EngageOn)
              assert "Mood presetId is Just preset-1" (e.presetId == Just "preset-1")
          case Map.lookup (PedalId "flint") b.pedals of
            Nothing -> assert "Flint board entry found" false
            Just e -> do
              assert "Flint engage is Off" (e.engage == EngageOff)
              assert "Flint presetId is Nothing" (isNothing e.presetId)
          case Map.lookup (PedalId "onward") b.pedals of
            Nothing -> assert "Onward board entry found" false
            Just e -> assert "Onward engage is NoChange" (e.engage == EngageNoChange)

  -- 6. Engage state mapping — all 5 values
  assert "parseEngageState on" (parseEngageState "on" == Just EngageOn)
  assert "parseEngageState off" (parseEngageState "off" == Just EngageOff)
  assert "parseEngageState a" (parseEngageState "a" == Just EngageA)
  assert "parseEngageState b" (parseEngageState "b" == Just EngageB)
  assert "parseEngageState no-change" (parseEngageState "no-change" == Just EngageNoChange)
  assert "parseEngageState invalid" (isNothing (parseEngageState "invalid"))

  log ""
  log "Done."
  where
  unsafeMV :: Int -> _
  unsafeMV n = case makeMidiValue n of
    Just mv -> mv
    Nothing -> unsafeMV 0 -- unreachable for valid test values
