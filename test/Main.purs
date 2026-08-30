module Test.Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Int (round)
import Data.Int.Bits (xor)
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Midi (makeCC, makeMidiValue, makeProgramNumber)
import Data.Pedal (PedalId(..))
import Data.Pedal.Engage (EngageConfig(..), EngageState(..), bypassCCs)
import Effect (Effect)
import Effect.Console (log)
import Config.Registry as CRegistry
import Engine (initEngineFromPedals, pedalsOnChannel)
import Engine.Storage as Storage
import Engine.Storage (engineToJson, parseEngine, parseCardOrder, parsePresets, parseBoardPresets, parseEngageState)
import Data.MC6.Assign as Assign
import Data.MC6.Board as Board
import Data.MC6.ControlBank as ControlBank
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..), MC6MsgType(..), MC6TogglePosition(..), mc6ActionToInt)
import Data.MC6.Model as Model
import Data.MC6.Reserved as Reserved
import Data.MC6.Settings as Settings
import Test.MC6Capture as Capture
import Data.Looper as Looper
import Component.Looper.Slots as Slots
import Foreign.LooperSocket (isWriting, phaseOf, phaseName, allPhases, LoopPhase(..)) as LooperSock
import Data.Looper.Banks as LB
import Data.MC6.Diagnostics as Diagnostics
import Data.Looper.Machine as Machine
import Data.Looper.Recipes as Recipes
import Data.Looper.Sheet as Sheet
import Data.Looper.Twister as LoopTw
import Data.Looper.Verb as LoopVerb
import Data.Enum as Enum
import Data.Foldable (for_)
import Data.String as String
import Data.String.Common (joinWith)
import Data.Monoid (power)
import Data.String.CodeUnits as StringCU
import Data.MC6.Read as Read
import Data.MC6.SysEx as SysEx
import Data.MC6.Dump as Dump
import Data.MC6.Global as Global
import Data.MC6.Stamp as Stamp
import Data.MC6.Survey as Survey
import Data.MC6.Verb as Verb
import Data.Tuple (Tuple(..))
import Pedals.Registry as Registry
import Data.Twister.Scene as Scene
import Data.Twister.Scenes as Scenes
import Component.Looper.TwisterCard as Card
import Engine.Twister as ETw

-- Golden fixture: JS-format engine state with 3 pedals, numeric-string CC keys
engineFixture :: String
engineFixture = """{"mood":{"channel":3,"values":{"14":64,"15":100,"16":50},"info":{}},"flint":{"channel":2,"values":{"13":80,"14":127,"15":0},"info":{}},"onward":{"channel":1,"values":{"14":32,"15":90,"16":60},"info":{}}}"""

cardOrderFixture :: String
cardOrderFixture = """["onward","mood","flint"]"""

presetsFixture :: String
presetsFixture = """[{"id":"preset-1","pedalId":"mood","name":"Ambient Wash","description":"Lush reverb with slow modulation","notes":"","values":{"14":64,"15":100,"16":50},"savedSlot":null,"created":"2025-01-15T10:30:00Z","modified":"2025-01-15T10:30:00Z"},{"id":"preset-2","pedalId":"flint","name":"Spring Clean","description":"Bright spring reverb","notes":"Good for country","values":{"13":80,"14":127},"savedSlot":2,"created":"2025-01-16T14:00:00Z","modified":"2025-02-01T09:15:00Z"}]"""

boardPresetsFixture :: String
boardPresetsFixture = """[{"id":"board-1","name":"Full Board","description":"All pedals on","notes":"","pedals":{"mood":{"presetId":"preset-1","engage":"on"},"flint":{"presetId":null,"engage":"off"},"onward":{"presetId":null,"engage":"no-change"}},"created":"2025-01-20T12:00:00Z","modified":"2025-01-20T12:00:00Z"}]"""

-- | One frame the MC6 actually sent, from the editor-handshake capture:
-- | the twelve switch names of wire bank 19. Kept verbatim so the decoder is
-- | tested against the device rather than against our own encoder.
capturedBankSwitchesFrame :: Array Int
capturedBankSwitchesFrame =
  [ 240, 0, 33, 36, 3, 3, 9, 1, 19, 0, 0, 0
  , 0, 0, 1, 22, 127, 0, 8, 82, 101, 99, 32, 32
  , 32, 32, 32, 127, 1, 8, 77, 117, 108, 116, 105, 112
  , 108, 121, 127, 2, 8, 84, 97, 107, 101, 32, 32, 32
  , 32, 127, 3, 8, 85, 110, 100, 111, 32, 32, 32, 32
  , 127, 4, 8, 67, 108, 101, 97, 114, 32, 32, 32, 127
  , 5, 8, 60, 32, 66, 97, 99, 107, 32, 32, 127, 6
  , 8, 67, 108, 105, 99, 107, 32, 32, 32, 127, 7, 8
  , 77, 111, 110, 105, 116, 111, 114, 32, 127, 8, 8, 32
  , 32, 32, 32, 32, 32, 32, 32, 127, 9, 8, 32, 32
  , 32, 32, 32, 32, 32, 32, 127, 10, 8, 32, 32, 32
  , 32, 32, 32, 32, 32, 127, 11, 8, 32, 32, 32, 32
  , 32, 32, 32, 32, 102, 247
  ]

-- | Rewrite one of our own preset writes as the device's dump frame.
-- |
-- | Only F2 changes: our writer already sends F1=7, F2=17 — which is exactly the
-- | editor's own upload code — and the device's dump records are F1=7, F2=1. The
-- | payload is byte-identical either way, which is why the encoder is a fair
-- | fixture for the decoder.
asDumpFrame :: Int -> Array Int -> Array Int
asDumpFrame f2 bytes = fromMaybe bytes (Array.updateAt 7 f2 bytes)

dumpPreset :: Array Int -> Maybe Dump.DumpPreset
dumpPreset bytes = case Dump.decodeDumpFrame bytes of
  Just (Dump.DumpPresetFrame p) -> Just p
  _ -> Nothing

-- | A bare Morningstar frame with the given function bytes and no payload.
frameWith :: Int -> Int -> Int -> Array Int
frameWith f1 f2 f3 =
  [ 0xF0, 0x00, 0x21, 0x24, 0x03, 0x00, f1, f2, f3, 0, 0, 0, 0, 0, 0, 0, 0, 0xF7 ]

assert :: String -> Boolean -> Effect Unit
assert label ok = log $ (if ok then "PASS" else "FAIL") <> " - " <> label

-- Right-align a bank number in two columns, so the map reads as a table.
pad :: String -> String
pad s = if String.length s >= 2 then s else " " <> s

intAbs :: Int -> Int
intAbs n = if n < 0 then negate n else n

-- Left-align to a fixed width, so a four-by-four grid prints as a grid.
padTo :: Int -> String -> String
padTo n s = s <> power " " (max 0 (n - String.length s))


main :: Effect Unit
main = do
  log "Running pedal definition tests..."

  -- Twelve boxes on the floor, plus Itajara.
  --
  -- Itajara is the app's own looper rather than a pedal we bought, so its
  -- definition is split differently: the baseline and the sections live in
  -- config/pedals/itajara.json and the PureScript entry carries only the donut
  -- layout. The two assertions below are about pedals that ship their data in
  -- PureScript, so they exclude it rather than being weakened for it.
  let count = Array.length Registry.pedals
  assert "Pedal count is 13 (12 hardware + Itajara)" (count == 13)

  let hardwarePedals = Array.filter (\p -> p.meta.id /= PedalId "itajara") Registry.pedals
  assert "12 hardware pedals" (Array.length hardwarePedals == 12)

  -- Each hardware pedal has a non-empty baseline
  let allHaveBaseline = Array.all (\p -> not (Map.isEmpty p.baseline)) hardwarePedals
  assert "All hardware pedals have baselines" allHaveBaseline

  -- Each hardware pedal has at least one section
  let allHaveSections = Array.all (\p -> not (Array.null p.sections)) hardwarePedals
  assert "All hardware pedals have sections" allHaveSections

  -- **Itajara must NOT bring a layout**, which is the reverse of what this
  -- asserted until 2026-08-27.
  --
  -- It had one, and the assertion was right at the time: the Looper page drew
  -- the donut and would have rendered nothing without it. The page now draws
  -- eight loops from the daemon's snapshot, and a second face built from the CC
  -- values the app *sent* would be the slower of two pictures of one engine —
  -- the failure the snapshot-only rule exists to prevent. So the layout is gone
  -- and this test guards the decision rather than the old shape.
  --
  -- The pedal itself stays registered, and the two assertions either side of
  -- this one are the ones that matter for that: thirteen pedals, and channel 13
  -- Itajara's alone.
  assert "Itajara has no layout — its surface is the Looper page"
    (Array.any (\p -> p.meta.id == PedalId "itajara" && isNothing p.layout) Registry.pedals)

  -- findPedal works (via registry)
  let reg = CRegistry.mkRegistry Registry.pedals [] { pedalOutput: { match: "" }, twisterInput: { match: "" }, twisterOutput: { match: "" }, mc6Input: { match: "" } }
  assert "findPedal MOOD" (isJust (CRegistry.findPedal reg (PedalId "mood")))
  assert "findPedal nonexistent returns Nothing" (isNothing (CRegistry.findPedal reg (PedalId "nonexistent")))

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

  -- 2. Round-trip: encode then parse initEngineFromPedals
  let testEngine = initEngineFromPedals Registry.pedals
      roundTripped = parseEngine (stringify (engineToJson testEngine))
  assert "Round-trip: encode then parse engine" (roundTripped == Just testEngine)

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

  -- The MC6 message budget.
  --
  -- DESIGN-v2 §5 originally costed a board at "twelve pedals, twelve messages,
  -- four spare", which was wrong: a dual-engage pedal needs two CCs to bypass
  -- unless it declares a whole-pedal bypass, and four of the thirteen are dual.
  -- These pin the arithmetic, because the failure mode is silent — SysEx pads
  -- with `Array.take 16` and the overflow is simply dropped.
  let dualPedals = Array.filter (\p -> case p.engage of
        DualEngage _ -> true
        _ -> false) Registry.pedals
  assert "4 pedals are dual-engage" (Array.length dualPedals == 4)

  assert "a single-engage bypass costs 1 message"
    (Array.all (\p -> case p.engage of
      SingleEngage _ -> Array.length (bypassCCs p.engage) == 1
      _ -> true) Registry.pedals)

  assert "Flint declares a whole-pedal bypass, so costs 1"
    (maybe false (\p -> Array.length (bypassCCs p.engage) == 1)
      (CRegistry.findPedal reg (PedalId "flint")))
  assert "MOOD declares a whole-pedal bypass, so costs 1"
    (maybe false (\p -> Array.length (bypassCCs p.engage) == 1)
      (CRegistry.findPedal reg (PedalId "mood")))

  -- Unverified pedals stay correct-but-expensive rather than guessed at.
  assert "Onward has no declared whole-pedal bypass, so costs 2"
    (maybe false (\p -> Array.length (bypassCCs p.engage) == 2)
      (CRegistry.findPedal reg (PedalId "onward")))

  -- The case that motivated all of it: every pedal switched off at once.
  let allOffBoard =
        { id: "budget-test", name: "all off", description: "", notes: ""
        , pedals: Map.fromFoldable
            (map (\p -> Tuple p.meta.id { presetId: Nothing, engage: EngageOff })
              Registry.pedals)
        , created: "", modified: ""
        }
      allOffCost = Board.boardMessageCount reg [] (Just 20) allOffBoard
  assert ("all-thirteen-off board fits in " <> show Board.messageLimit
           <> " (costs " <> show allOffCost <> ")")
    (allOffCost <= Board.messageLimit)
  assert "boardFits agrees with the count"
    (Board.boardFits reg [] (Just 20) allOffBoard == (allOffCost <= Board.messageLimit))

  -- Observing the MC6 (DESIGN-v2 §3).
  --
  -- The app keeps its picture of the pedals true by overhearing what the MC6
  -- sends them, and the whole mechanism turns on mapping a MIDI channel back to
  -- a pedal. Getting that wrong is invisible: nothing errors, the belief just
  -- quietly stops matching the rig.
  let liveEngine = initEngineFromPedals Registry.pedals
  assert "every pedal is reachable by its own channel"
    (Array.all
      (\p -> Array.elem p.meta.id (pedalsOnChannel p.meta.defaultChannel liveEngine))
      Registry.pedals)

  -- Channel 1 is the app's own board-recall relay and channel 13 is Itajara's;
  -- a hardware pedal appearing on either would have its CCs swallowed by an
  -- earlier branch of the handler and never observed.
  assert "no hardware pedal sits on the board-recall channel (1)"
    (Array.all (_ == PedalId "itajara") (pedalsOnChannel 1 liveEngine))
  assert "channel 13 belongs to Itajara alone"
    (pedalsOnChannel 13 liveEngine == [ PedalId "itajara" ])

  assert "an unused channel matches nothing" (Array.null (pedalsOnChannel 9 liveEngine))

  -- Every pedal on a distinct channel, or observation cannot tell them apart.
  let channels = map (\p -> p.meta.defaultChannel) Registry.pedals
  assert "all pedal channels are distinct"
    (Array.length (Array.nub channels) == Array.length channels)

  log ""
  log "Running verb classification tests..."

  -- The point of classification is that nothing has to be re-authored to
  -- benefit, so the test subject is the control bank as it already exists.
  let bank = ControlBank.exampleControlBank
      verbOf sw = Verb.classify reg 1 sw.messages
      verbs = map verbOf bank.switches

  assert "every switch in the default bank classifies"
    (Array.length verbs == Array.length bank.switches)

  -- Eight of the nine are pedal actions; the ninth is the empty return switch,
  -- which only becomes a bank jump when the bank is compiled.
  let actions = Array.filter (case _ of
        Verb.Action _ -> true
        _ -> false) verbs
  assert "8 of the 9 default switches are pedal actions" (Array.length actions == 8)
  assert "nothing in the default bank is unclassifiable"
    (Array.null (Array.filter (_ == Verb.Raw) verbs))

  -- Toggle and momentary must not be confused: reading them backwards would
  -- turn a hold into a latch.
  let shapesOf s = Array.filter (case _ of
        Verb.Action a -> a.shape == s
        _ -> false) verbs
  assert "6 toggling actions" (Array.length (shapesOf Verb.Toggling) == 6)
  assert "2 momentary actions" (Array.length (shapesOf Verb.Momentary) == 2)

  -- Actions are the only timing-critical verb, which is what forces them onto
  -- a short press (DESIGN-CONTROLS §2).
  assert "actions are timing-critical, navigation is not"
    (Verb.isTimingCritical (Verb.Action { pedalId: PedalId "mood", cc: 105, shape: Verb.Toggling })
      && not (Verb.isTimingCritical (Verb.Navigation (Verb.ToBank 1))))

  -- Scope orders the ladder, which is what a colour legend reads.
  assert "verb scope rises with reach"
    (Verb.verbScope Verb.Blank
       < Verb.verbScope (Verb.Navigation Verb.BankUp)
       && Verb.verbScope (Verb.Navigation Verb.BankUp)
            < Verb.verbScope (Verb.Action { pedalId: PedalId "mood", cc: 1, shape: Verb.OneShot })
       && Verb.verbScope (Verb.Action { pedalId: PedalId "mood", cc: 1, shape: Verb.OneShot })
            < Verb.verbScope (Verb.PedalPreset { pedalId: PedalId "mood", program: 3 })
       && Verb.verbScope (Verb.PedalPreset { pedalId: PedalId "mood", program: 3 })
            < Verb.verbScope (Verb.Scene { cc: 20 }))

  assert "an empty switch is Blank, not Raw"
    (Verb.classify reg 1 [] == Verb.Blank)
  assert "a bank jump classifies as navigation"
    (Verb.classify reg 1 [ MC6Msg.bankJumpMessage 22 ActionPress ] == Verb.Navigation (Verb.ToBank 22))
  assert "a CC on the relay channel is a scene, not a pedal action"
    (Verb.classify reg 1 [ MC6Msg.ccMessage 1 20 127 ActionPress ] == Verb.Scene { cc: 20 })

  log ""
  log "Running MC6 survey tests..."

  let cards = Survey.survey reg 1 [ ControlBank.exampleControlBank ] [] [] Map.empty Map.empty

  assert "the survey covers all 30 banks" (Array.length cards == Survey.bankCount)
  assert "a known bank has 12 slots"
    (Array.all (\c -> Array.length c.slots == 12) (Survey.knownBanks cards))
  -- An unknown bank has *no* slots rather than twelve blank ones. Twelve blanks
  -- would assert twelve empty switches, which is precisely the claim we cannot
  -- make about a bank nobody has looked at.
  assert "an unknown bank has no slots at all"
    (Array.all (\c -> Array.null c.slots)
      (Array.filter (\c -> c.provenance == Survey.Unknown) cards))

  -- The distinction the whole view turns on: we authored one bank, and know
  -- nothing whatever about the other twenty-nine. Painting those as empty
  -- would be a lie about most of the instrument.
  assert "only the authored bank is known" (Array.length (Survey.knownBanks cards) == 1)
  -- Read from the bank rather than restated. These said `20` in four places,
  -- which is the same defect the reserved-bank table exists to stop: a number
  -- written down twice is a number that can disagree with itself, and when the
  -- default moved off the probe's bank these failed for a reason that had
  -- nothing to do with what they are testing.
  let defaultBankNum = ControlBank.exampleControlBank.mc6BankNumber
  assert "unknown banks are Unknown, not empty"
    (Array.all (\c -> c.provenance == Survey.Unknown)
      (Array.filter (\c -> c.bankNumber /= defaultBankNum) cards))

  -- The default bank's nine switches pad out to twelve.
  case Array.find (\c -> c.bankNumber == defaultBankNum) cards of
    Nothing -> assert "the default control bank is surveyed" false
    Just c -> do
      assert "the default control bank is surveyed" (c.provenance == Survey.Authored)
      assert "its name survives the survey" (c.name == "Default Controls")
      assert "padding to 12 is Blank, not Raw"
        (Array.length (Array.filter (_ == Verb.Blank) c.slots) == 4)

  -- Navigation edges: the default bank's return switch carries no jump until
  -- the bank is compiled, so an uncompiled survey has no edges to draw.
  assert "no phantom navigation edges" (Array.null (Survey.navigationEdges cards))

  let navCard =
        { bankNumber: 1, name: "Boards", provenance: Survey.Authored
        , slots: [ Verb.Navigation (Verb.ToBank 20), Verb.Blank ]
        , observedNames: [], agrees: Nothing, claimants: [] }
  assert "a bank jump becomes a graph edge"
    (Survey.navigationEdges [ navCard ] == [ Tuple 1 20 ])
  assert "unknown banks contribute no edges"
    (Array.null (Survey.navigationEdges
      [ { bankNumber: 5, name: "", provenance: Survey.Unknown
        , slots: [ Verb.Navigation (Verb.ToBank 3) ]
        , observedNames: [], agrees: Nothing, claimants: [] } ]))

  log ""
  -- Reading the device outranks anything we merely believe.
  let readNames = Map.fromFoldable [ Tuple 11 "LoopyPro", Tuple 19 "Ableton" ]
      readSwitches = Map.singleton 19 [ "Rec", "Multiply", "Take" ]
      readCards = Survey.survey reg 1 [ ControlBank.exampleControlBank ] [] [] readNames readSwitches
      at n = Array.filter (\c -> c.bankNumber == n) readCards
  assert "a bank the device named is Observed, not Unknown"
    (map _.provenance (at 11) == [ Survey.Observed ])
  assert "and takes its name from the device"
    (map _.name (at 11) == [ "LoopyPro" ])
  assert "reading lifts three banks out of Unknown"
    (Array.length (Survey.knownBanks readCards) == 3)
  assert "an unread, unauthored bank stays Unknown"
    (map _.provenance (at 5) == [ Survey.Unknown ])
  -- Silence rather than a clean bill: nothing to compare means Nothing.
  assert "with nothing to compare, agreement is Nothing"
    (Array.all (\c -> c.agrees == Nothing) (Array.filter (\c -> c.bankNumber /= 20) readCards))

  log ""
  log "Running global-switch tests..."

  let bankA = ControlBank.exampleControlBank
      bankB = bankA { id = "b", mc6BankNumber = 21 }
      backSwitch =
        { id: "global-G", slot: 6, label: "< Back", longName: "Back to Board Bank"
        , toToggle: false, messages: [ MC6Msg.bankJumpMessage 1 ActionPress ] }

  -- Applied on the way out, not stored: the authored page keeps saying what its
  -- author wrote.
  assert "a global fills its slot"
    (map _.label (Array.index (Global.applyGlobals [ backSwitch ] bankA).switches 6)
      == Just "< Back")
  assert "and does not disturb the others"
    (map _.label (Array.index (Global.applyGlobals [ backSwitch ] bankA).switches 0)
      == map _.label (Array.index bankA.switches 0))
  -- The rule the whole design rests on: no page can refuse.
  assert "every page takes it, with no way to opt out"
    (map _.label (Array.index (Global.applyGlobals [ backSwitch ] bankB).switches 6)
      == Just "< Back")
  assert "applying nothing changes nothing"
    (Global.applyGlobals [] bankA == bankA)
  assert "globalAt finds by slot, not by index"
    (map _.label (Global.globalAt [ backSwitch ] 6) == Just "< Back"
      && Global.globalAt [ backSwitch ] 5 == Nothing)

  -- Promote and dissolve are duals. If this ever fails, one of them has grown a
  -- side effect the other does not undo, and the two-concept story is a lie.
  let banks2 = [ bankA, bankB ]
      promoted = Global.promote 0 (Global.toSwitch backSwitch) []
      roundTrip = Global.dissolve 0 promoted banks2
  assert "promote puts the switch on the slot it names"
    (map _.slot (Array.head promoted) == Just 0)
  assert "dissolve leaves no global behind"
    (Array.null roundTrip.globals)
  assert "and writes the copy onto every page"
    (Array.all (\cb -> map _.label (Array.index cb.switches 0) == Just "< Back")
      roundTrip.banks)
  assert "dissolving what was never global is a no-op"
    (let r = Global.dissolve 3 [] banks2 in r.banks == banks2 && Array.null r.globals)
  -- The other exit. Promote writes nothing to the pages, so its undo must not
  -- either — otherwise a global made by mistake could only be removed by
  -- stamping the mistake onto all thirty pages.
  assert "discard removes the global and touches no page"
    (Array.null (Global.discard 0 promoted))
  assert "promote then discard is the identity"
    (Global.discard 0 (Global.promote 0 (Global.toSwitch backSwitch) []) == [])

  -- A stamp is a copy, not a link: only the named pages change, and nothing
  -- records that they came from the same place.
  let stamped = Global.stampTo 2 (Global.toSwitch backSwitch) [ 21 ] banks2
  assert "a stamp lands on the pages it names"
    (map (\cb -> map _.label (Array.index cb.switches 2)) (Array.index stamped 1)
      == Just (Just "< Back"))
  assert "and leaves the others alone"
    (Array.index stamped 0 == Just bankA)

  -- Retiring the shared-switch era. The page that refused keeps what it refused
  -- the global *for*; the pages that accepted keep the global's content. Same
  -- bytes out either way, which is the only acceptable outcome for a migration
  -- nobody asked for.
  let refuser = bankB { switches = fromMaybe bankB.switches
                          (Array.updateAt 6 { label: "Tuner", longName: "Tuner"
                                            , toToggle: false, messages: [] } bankB.switches) }
      retired = Global.retireOverrides [ [], [ 6 ] ] [ backSwitch ] [ bankA, refuser ]
  assert "a global any page refused is dissolved"
    (Array.null retired.globals)
  assert "the accepting page keeps the global's content"
    (map (\cb -> map _.label (Array.index cb.switches 6)) (Array.index retired.banks 0)
      == Just (Just "< Back"))
  assert "and the refusing page keeps its own"
    (map (\cb -> map _.label (Array.index cb.switches 6)) (Array.index retired.banks 1)
      == Just (Just "Tuner"))
  assert "with nothing refused, nothing is disturbed"
    (let r = Global.retireOverrides [ [], [] ] [ backSwitch ] banks2
     in r.globals == [ backSwitch ] && r.banks == banks2)

  -- The migration off the hardcoded return switch, now strict: agreement makes
  -- a global, disagreement makes copies.
  let unanimous = Global.migrateReturns 1
        [ bankA, bankA { id = "b", mc6BankNumber = 1 }, bankA { id = "c", mc6BankNumber = 2 } ]
  assert "pages that agree on a return slot get one global"
    (map _.slot (Array.head unanimous.globals) == Just 6)
  assert "and keep the name a page gave it"
    (map _.label (Array.head unanimous.globals) == Just "< Back")
  assert "and their own switches are untouched"
    (unanimous.banks == [ bankA, bankA { id = "b", mc6BankNumber = 1 }, bankA { id = "c", mc6BankNumber = 2 } ])

  -- The odd page is why this cannot be a global: a global at slot 6 would give
  -- page d a second way back, silently.
  let mixed = Global.migrateReturns 1
        [ bankA
        , bankA { id = "b", mc6BankNumber = 1 }
        , bankA { id = "d", mc6BankNumber = 3, returnSwitchIndex = 0 }
        ]
  assert "pages that disagree get no global at all"
    (Array.null mixed.globals)
  assert "each page gets the jump written where it already kept it"
    (map (\cb -> map (_.messages >>> Array.length) (Array.index cb.switches 0))
      (Array.index mixed.banks 2) == Just (Just 1))
  assert "and the modal slot is left as that page had it"
    (map (\cb -> map (_.messages >>> Array.length) (Array.index cb.switches 6))
      (Array.index mixed.banks 2) == Just (Just 0))
  assert "migration of nothing produces nothing"
    (Array.null (Global.migrateReturns 1 []).globals)

  -- A bank the device described but this app has never written. The survey used
  -- to label these "not read" because its slot array is built from messages, and
  -- a read brings names only — so a successful read looked like no read at all.
  let namesOnly = Survey.survey reg 1 []
        [] []
        (Map.fromFoldable [ Tuple 7 "Presets", Tuple 8 "" ])
        (Map.fromFoldable [ Tuple 7 [ "Home", "Resetter" ] ])
      cardAt n = Array.find (\c -> c.bankNumber == n) namesOnly
  assert "a bank the device named is known even with no messages for it"
    (map _.provenance (cardAt 7) == Just Survey.Observed)
  assert "and carries the names it gave, so the card has something to show"
    (map _.observedNames (cardAt 7) == Just [ "Home", "Resetter" ])
  -- The distinction that makes the third state necessary: named but no switch
  -- set is still known, and must not fall back to "not read".
  assert "a bank named but with no switch set is still not Unknown"
    (map _.provenance (cardAt 8) == Just Survey.Observed)
  assert "a bank nothing said anything about stays Unknown"
    (map _.provenance (cardAt 3) == Just Survey.Unknown)
  -- The two counts the header shows must be able to differ. A single connect
  -- names every bank and returns exactly one switch set, so a summary built from
  -- provenance alone reports 30 and 30 over cards that show nothing.
  assert "names read and switch sets read are different counts"
    (Array.length (Survey.knownBanks namesOnly) == 2
      && Array.length (Array.filter (\c -> Array.any (_ /= "") c.observedNames) namesOnly) == 1)

  log ""
  log "Running MC6 read-request tests..."

  -- Byte layout taken from Morningstar's own editor bundle: F1..F6 live at
  -- offsets 6..11, and the checksum is an XOR of everything before the last two
  -- bytes. Pinned here because the request was reconstructed from their code and
  -- a silent drift in our framing would look like a device that stopped
  -- answering.
  let req5 = SysEx.frameBytes (SysEx.sysexRequestPresetNames 5)
  assert "a bank request is a Morningstar frame"
    (Array.take 5 req5 == [ 0xF0, 0x00, 0x21, 0x24, 0x03 ])
  assert "carrying F1=0 F2=64 and the bank in F3"
    (Array.slice 6 9 req5 == [ 0x00, 0x40, 5 ])
  assert "and ending with a checksum then F7"
    (Array.last req5 == Just 0xF7 && Array.length req5 == 18)
  -- The checksum the device validates: XOR of every byte up to the slot itself.
  assert "whose checksum is the XOR the device expects"
    (Array.index req5 16
      == Just (Array.foldl xor 0 (Array.take 16 req5) `mod` 128))
  -- The two dump requests differ by one in F3, and the wrong one answers with
  -- silence rather than an error — which is exactly how the first attempt at
  -- this looked like a decoder fault. 51 is all banks, 50 is the current one.
  assert "the all-banks dump is F1=7 F2=0 F3=51"
    (Array.slice 6 9 (SysEx.frameBytes SysEx.sysexRequestFullDump) == [ 0x07, 0x00, 0x33 ])
  assert "and the single-bank dump is its neighbour, 50"
    (Array.slice 6 9 (SysEx.frameBytes SysEx.sysexRequestBankDump) == [ 0x07, 0x00, 0x32 ])
  -- Flow control, not courtesy: the device waits to be told each frame landed,
  -- and echoing the checksum is what says which frame is meant.
  assert "an acknowledgement echoes the checksum it received"
    (Array.slice 6 9 (SysEx.frameBytes (SysEx.sysexAcknowledge 0x66)) == [ 0x00, 0x7F, 0x66 ])

  -- The setting that makes a held session usable: off, and the device stops
  -- blocking its own bank jump and MIDI clock while an editor is connected.
  -- Pinned because it is the one MC6 value the app writes without being able to
  -- read it back, so a drift here would change a controller setting to something
  -- nobody asked for and nothing would say so.
  assert "switch-press-load off is F1=3 F2=49 F3=0"
    (Array.slice 6 9 (SysEx.frameBytes (SysEx.sysexSwitchPressLoad false))
      == [ 0x03, 0x31, 0x00 ])
  assert "and on is the same frame with the flag set"
    (Array.slice 6 9 (SysEx.frameBytes (SysEx.sysexSwitchPressLoad true))
      == [ 0x03, 0x31, 0x01 ])

  assert "asking for every bank at once is F2=43 with no bank"
    (Array.slice 6 9 (SysEx.frameBytes SysEx.sysexRequestAllPresetNames) == [ 0x00, 0x2B, 0x00 ])
  -- Bank 0 must be requestable: an off-by-one here would silently read 1..29 and
  -- report a hole at the one bank that is always in use.
  assert "bank 0 is a legal request, not an empty one"
    (Array.slice 6 9 (SysEx.frameBytes (SysEx.sysexRequestPresetNames 0)) == [ 0x00, 0x40, 0x00 ])
  assert "and the last bank is reachable too"
    (Array.slice 6 9 (SysEx.frameBytes (SysEx.sysexRequestPresetNames 29)) == [ 0x00, 0x40, 29 ])

  -- The dump decoder is the exact mirror of an encoder that has been writing to
  -- this hardware for months, so the encoder is the right fixture: anything the
  -- device would accept from us must read back as what we meant.
  let dumpMsgs =
        [ MC6Msg.ccMessage 3 105 127 ActionPress
        , MC6Msg.bankJumpMessage 4 ActionPress
        ]
      encoded = SysEx.frameBytes (SysEx.sysexPresetData 22 6 "Br Tap" "Brig Tap Tempo" true dumpMsgs)
      -- Ours go out as F1=7 F2=17 (the editor's upload code); the device's dump
      -- records are F1=7 F2=1. Same payload, so only F2 needs rewriting.
      asDump = asDumpFrame 0x01 encoded
  assert "a dump frame decodes to the preset it encoded"
    (dumpPreset asDump # map (\d -> Tuple d.bankNumber d.presetNum)
      # (_ == Just (Tuple 22 6)))
  assert "with its names and toggle flag intact"
    (map _.shortName (dumpPreset asDump) == Just "Br Tap"
      && map _.longName (dumpPreset asDump) == Just "Brig Tap Tempo"
      && map _.toToggle (dumpPreset asDump) == Just true)
  -- The whole point of the dump over a name read: messages come back.
  assert "and every message, which is what names could never give us"
    (map _.messages (dumpPreset asDump) == Just dumpMsgs)
  assert "a frame that is not a dump frame at all is refused"
    (Dump.decodeDumpFrame capturedBankSwitchesFrame == Nothing)
  -- The device announces the end of a dump. Recognising it is what stops a read
  -- from having to guess at silence — a guess that truncated one at 221 of 450.
  assert "the all-banks completion frame ends the dump"
    (Dump.decodeDumpFrame (frameWith 0x07 0x00 0x02) == Just Dump.DumpFinished
      && Dump.decodeDumpFrame (frameWith 0x07 0x00 0x01) == Just Dump.DumpFinished)
  assert "and its start frame is not mistaken for the end"
    (Dump.decodeDumpFrame (frameWith 0x07 0x00 0x00) == Just Dump.DumpStarted)
  -- A dump costs a minute of the hardware's time and 450 frames, so it has to
  -- survive a reload. Round-tripped through the real codec rather than eyeballed,
  -- because a lossy save would look exactly like a device that read badly.
  let dumpedBank =
        { bankNumber: 22
        , bankName: ""
        , bankClearToggle: false
        , presets:
            [ { presetNum: 0, shortName: "Br Tap", toggleName: "", longName: "Brig Tap"
              , toToggle: true, toggleGroup: 0, messages: dumpMsgs }
            , { presetNum: 6, shortName: "< Back", toggleName: "", longName: ""
              , toToggle: false, toggleGroup: 0, messages: [] }
            ]
        }
  assert "a dumped bank survives being saved and loaded"
    (Storage.parseDumpedBanks (Storage.dumpedBanksToJsonString [ dumpedBank ])
      == Just [ dumpedBank ])

  assert "bank records are counted, not discarded"
    (Dump.decodeDumpFrame (frameWith 0x07 0x02 0x0b) == Just (Dump.DumpBankFrame 11))
  -- 30 banks x (12 presets + 2 expression + 1 bank record).
  assert "the expected frame count is the arithmetic, not a magic number"
    (Dump.expectedFrames == 30 * (12 + 2 + 1))
  -- Ordering is not promised by the protocol, and a shuffled bank would look
  -- like a device fault rather than a decoder one.
  let shuffled =
        [ { bankNumber: 3, presetNum: 5, isExp: false, shortName: "F", toggleName: ""
          , longName: "", toToggle: false, toggleGroup: 0, messages: [] }
        , { bankNumber: 3, presetNum: 0, isExp: false, shortName: "A", toggleName: ""
          , longName: "", toToggle: false, toggleGroup: 0, messages: [] }
        , { bankNumber: 3, presetNum: 1, isExp: true, shortName: "EXP", toggleName: ""
          , longName: "", toToggle: false, toggleGroup: 0, messages: [] }
        ]
  assert "presets are gathered into banks in switch order, not arrival order"
    (map (\b -> map _.shortName b.presets) (Array.head (Dump.presetsToBanks shuffled))
      == Just [ "A", "F" ])
  assert "and expression presets are left out of the twelve"
    (map (\b -> Array.length b.presets) (Array.head (Dump.presetsToBanks shuffled))
      == Just 2)

  log ""
  log "Running MC6 navigation-graph tests..."

  -- A small hand-built instrument, because the point of these functions is to
  -- catch shapes that no real bank set has yet: home reaches 1 and 2, bank 7 is
  -- programmed but nothing points at it, and bank 2 has no way out.
  let card n slots =
        { bankNumber: n, name: "", provenance: Survey.Authored
        , slots, observedNames: [], agrees: Nothing, claimants: [] }
      jump n = Verb.Navigation (Verb.ToBank n)
      graph =
        [ card 0 [ jump 1, jump 2 ]
        , card 1 [ jump 0 ]
        , card 2 [ Verb.Blank ]
        , card 7 [ jump 0 ]
        ]

  assert "home reaches itself and everything it jumps to"
    (Survey.reachableFrom 0 graph == Set.fromFoldable [ 0, 1, 2 ])
  assert "a bank nothing points at is stranded"
    (Survey.stranded 0 graph == [ 7 ])
  assert "a bank with no jump of its own is a dead end"
    (Survey.deadEnds graph == [ 2 ])
  -- Reachability must not loop forever on a cycle, which 0 <-> 1 is.
  assert "a two-bank cycle terminates and is fully reachable"
    (Survey.reachableFrom 1 graph == Set.fromFoldable [ 0, 1, 2 ])
  -- The accusation only applies to banks we know something about; an unknown
  -- bank may be reachable by a jump we have not read.
  assert "unknown banks are never accused of being stranded"
    (Array.null (Survey.stranded 0
      [ { bankNumber: 9, name: "", provenance: Survey.Unknown
        , slots: [], observedNames: [], agrees: Nothing, claimants: [] } ]))

  log ""
  log "Running MC6 read-protocol tests..."

  -- Decoded against a real frame the device sent, lifted verbatim out of
  -- test/mc6-editor-handshake-20260816.json. Testing the decoder on invented
  -- bytes would only prove it agrees with the encoder we also wrote.
  case Read.decodeReply capturedBankSwitchesFrame of
    Just (Read.BankSwitches wireBank names) -> do
      assert "the captured frame reports wire bank 19 (editor 20)" (wireBank == 19)
      assert "and carries all twelve switch names" (Array.length names == 12)
      assert "which are our looper transport"
        (Array.take 6 names == [ "Rec", "Multiply", "Take", "Undo", "Clear", "< Back" ])
      assert "trailing empty switches come back empty, not spaces"
        (Array.drop 8 names == [ "", "", "", "" ])
    _ -> assert "captured frame decodes as BankSwitches" false

  assert "a non-Morningstar SysEx frame is rejected"
    (Read.decodeReply [ 0xF0, 0x7E, 0x00, 0x06, 0x01, 0xF7 ] == Nothing)
  -- `03 20` used to be this test's example of an undecoded code. It is the
  -- channel table, decoded now, so the example moves to `08 00` — which is in
  -- the capture and genuinely still unknown. The assertion is the same one:
  -- what we cannot read is named, not dropped.
  assert "an undecoded function code is named rather than dropped"
    (Read.decodeReply [ 0xF0, 0x00, 0x21, 0x24, 0x03, 0x03, 0x08, 0x00
                      , 0, 0, 0, 0, 0, 0, 0, 18, 0, 0xF7 ]
      == Just (Read.OtherReply 0x08 0x00))

  assert "and the whole 03 2x family now comes back as settings, sub-code intact"
    (Read.decodeReply [ 0xF0, 0x00, 0x21, 0x24, 0x03, 0x03, 0x03, 0x20
                      , 0, 0, 0, 0, 0, 0, 0, 18, 0, 0xF7 ]
      -- Payload is empty: this frame is header and checksum only, which is the
      -- point — the sub-code survives even when there is nothing to decode.
      == Just (Read.ControllerSettings 0x20 []))

  -- Captured from the device on 2026-08-17 while jumping banks. These three
  -- frames were arriving all along and being logged as "not decoded", which is
  -- why a jump that had already happened could not be confirmed: the app was
  -- waiting on the switch-names frame, which says the same thing but only after
  -- the whole controller-settings parade.
  --
  -- Note the bank number is in the payload and F3 is 0 — reading F3, as the
  -- switch-names frame allows, would have given bank 0 for every one of them.
  let bankName = [ 80, 97, 116, 99, 104, 32, 84, 119, 111 ]  -- "Patch Two"
                   <> Array.replicate 15 32
      currentBankFrame =
        [ 0xF0, 0x00, 0x21, 0x24, 0x03, 0x03, 0x06, 0x02, 0, 0, 0, 0, 0, 0, 1, 117 ]
          <> [ 0x7F, 0x00, 0x01, 2 ]
          <> ([ 0x7F, 0x03, 24 ] <> bankName)
          <> [ 0x60, 0xF7 ]
  assert "the device says which bank it moved to, and what it is called"
    (Read.decodeReply currentBankFrame == Just (Read.CurrentBank 2 "Patch Two"))
  assert "and which preset it has in hand, agreeing about the bank"
    (Read.decodeReply
      ([ 0xF0, 0x00, 0x21, 0x24, 0x03, 0x03, 0x06, 0x01, 0, 0, 0, 0, 0, 0, 2, 16 ]
        <> [ 0x7F, 0x00, 0x03, 2, 11, 0 ] <> [ 0x09, 0xF7 ])
      == Just (Read.CurrentPreset 2 11))
  -- The device announces edit mode rather than leaving us to infer it from
  -- having asked — so a session opened by something else is visible too.
  assert "editor mode on is announced as it is entered"
    (Read.decodeReply [ 0xF0, 0x00, 0x21, 0x24, 0x03, 0x03, 0x00, 0x7D
                      , 1, 0, 0, 0, 0, 0, 0, 18, 27, 0xF7 ]
      == Just (Read.EditorMode true))
  assert "and off as it is left"
    (Read.decodeReply [ 0xF0, 0x00, 0x21, 0x24, 0x03, 0x03, 0x00, 0x7D
                      , 0, 0, 0, 0, 0, 0, 0, 18, 26, 0xF7 ]
      == Just (Read.EditorMode false))

  -- No request-frame tests: there are no request frames. Sweeping the
  -- function-code space found nothing that asks for bank data, because the
  -- device volunteers a full dump on connect instead. All this module does is
  -- decode, and the decoder is tested above against the device's own bytes.

  log ""
  log "Gestures, as the device tells them apart..."

  -- There used to be a hundred lines of Mealy-machine tests here, feeding a
  -- recogniser timestamped switch edges and reading gestures out. The MC6 does
  -- that itself, measured, so what is left to test is the *encoding*: that the
  -- three gestures ride on the three actions the device was measured to send,
  -- and that the value says which.

  assert "the three gestures ride on the measured triple of device actions"
    (map LB.gestureAction LB.allGestures
      == [ ActionRelease, ActionDoubleTapRelease, ActionLongPress ])

  assert "and each has its own value, so one CC carries all three"
    (Array.nub (map LB.gestureValue LB.allGestures) == [ 127, 64, 1 ])

  assert "a value decodes back to the gesture that wrote it"
    (Array.all (\g -> LB.gestureFromValue (LB.gestureValue g) == Just g) LB.allGestures)

  -- **The old programming is refused rather than half-understood.** Before the
  -- device did the recognising, a release sent 0 on the same CC. If that were
  -- read as a gesture, a board still holding an old upload would half-work in a
  -- way nobody could see; refusing it puts a line in the console instead.
  assert "and a release from the old press/release programming is not a gesture"
    (LB.gestureFromValue 0 == Nothing
      && LB.decodeSwitch LB.switchChannel (LB.switchCC LB.LoopBank 0) 0 == Nothing)

  let loopBankSwitch i = do
        cb <- Array.head (LB.banks { base: 22, boardBank: 1 })
        Array.index cb.switches i
      -- Which actions carry a CC on the app's own channel, and with what value.
      ccsOf sw = map (\m -> Tuple m.action m.data2)
        (Array.filter (\m -> m.msgType == MsgCC && m.channel == LB.switchChannel)
          sw.messages)
      jumpsOf sw = map _.action
        (Array.filter (\m -> m.msgType == MsgBankJump) sw.messages)

  -- A loop switch is the fully loaded case: tap selects, double overdubs, hold
  -- opens the config bank.
  -- **The one switch in the family that carries three meanings**, and it is J:
  -- tap stops the set, double starts it again from the top, hold is the way
  -- out. All three stay on the release side, because a press would stop the set
  -- before the device knew you were holding to leave.
  --
  -- The double really is a double now — 64 rather than the tap's own 127 —
  -- because there is a duty on it. The old way-out switch had no double and got
  -- the tap's value as a fallback, so a fumble did the tap once instead of
  -- answering with silence; that fallback still exists and is checked below.
  assert "the switch with three gestures sends all three, one per action, on the release side"
    (let set = do
           b <- LB.banks { base: 22, boardBank: 1 }
           Array.take 1 (Array.drop 9 b.switches)
     in Array.length set == Array.length LB.allSlots
          && Array.all
               (\sw -> ccsOf sw == [ Tuple ActionRelease 127
                                   , Tuple ActionDoubleTapRelease 64
                                   , Tuple ActionLongPress 1
                                   ])
               set)

  -- And only the hold navigates, so only the hold carries a jump. Stopping the
  -- set does not move the board, which is the difference from the switch this
  -- replaced: there, both the tap and its double were bank jumps.
  assert "the set switch jumps on the hold alone"
    (let set = do
           b <- Array.drop 1 (LB.banks { base: 22, boardBank: 1 })
           Array.take 1 (Array.drop 9 b.switches)
     in Array.all (\sw -> jumpsOf sw == [ ActionLongPress ]) set)

  -- **One meaning, one message, at press-down.** Measured: the MC6 fires Press
  -- the instant the foot lands, whatever else is bound. It is the *release*
  -- that waits, because the release is what has to be decided. So a switch with
  -- nothing to decide has nothing to wait for — Click answers immediately, and
  -- two presses in quick succession are simply two presses.
  assert "a switch carrying one meaning reports at press-down and only there"
    (map ccsOf (loopBankSwitch 11) == Just [ Tuple ActionPress 127 ])

  -- **A CC and a bank jump never share an action.** That configuration has cost
  -- this project twice: once it ate the release, once it ate the CC itself and
  -- selecting a loop silently did nothing. The report goes at press-down and the
  -- board moves when the foot lifts, so the app is told before the thing it is
  -- being told about happens.
  -- The config bank's first switch, since a loop switch stopped being one
  -- meaning and gained its double. Same shape: one meaning, and it is a place —
  -- so the CC reports at press-down and the board moves on the lift.
  assert "a press-side navigating switch reports on the press and jumps on the release"
    (let cfg = do
           b <- Array.index (LB.banks { base: 22, boardBank: 1 })
                  (LB.slotIndex LB.ConfigBank)
           Array.index b.switches 0
     in map ccsOf cfg == Just [ Tuple ActionPress 127 ]
          && map jumpsOf cfg == Just [ ActionRelease, ActionDoubleTapRelease ])

  -- **The fallback, which now applies only on the release side.** A switch that
  -- carries a hold is on the release, and the device suppresses Release on a
  -- double whether or not anything is bound to it — so a fumbled double would
  -- answer with silence. It gets the tap's own value instead. The way out of a
  -- sub-bank is such a switch: tap for Loops, hold for Board.
  -- **A loop switch's jump is on the release alone now**, because its double
  -- means something: undo that loop. The fallback that used to put the tap's
  -- own destination on `DoubleTapRelease` applies to a switch carrying a hold
  -- and no double, and nothing in the family is shaped that way any more — the
  -- builder still does it, and nothing exercises it.
  -- **Two gestures and no jump at all.** Which also means a loop switch stopped
  -- putting a CC and a bank jump on the same action — the arrangement this
  -- project has twice watched eat one or the other, and the risk I flagged when
  -- these switches moved to the release side. It went away by itself when the
  -- tap stopped navigating.
  assert "a loop switch reports two gestures and goes nowhere"
    (let places = do
           b <- Array.take 1 (LB.banks { base: 22, boardBank: 1 })
           Array.take LB.loopSwitches b.switches
     in Array.length places == LB.loopSwitches
          && Array.all
               (\sw -> Array.null
                         (Array.filter (\m -> m.msgType == MsgBankJump) sw.messages)
                         && ccsOf sw == [ Tuple ActionRelease 127
                                        , Tuple ActionDoubleTapRelease 64
                                        ])
               places)

  -- **The CCs before the jumps.** A jump that goes out first means the message
  -- after it is emitted from the bank the board has already reached — which is
  -- how a press on one bank and its release on another came to be seen, and a
  -- hold nobody made came to be fired.
  assert "and every CC in a preset is written before every bank jump"
    (Array.all
      (\cb -> Array.all
        (\sw ->
          let kinds = map _.msgType sw.messages
              jumpAt = Array.findIndex (_ == MsgBankJump) kinds
              ccAfter = Array.findLastIndex (_ == MsgCC) kinds
          in case jumpAt, ccAfter of
               Just j, Just c -> c < j
               _, _ -> true)
        cb.switches)
      (LB.banks { base: 22, boardBank: 1 }))

  -- A hold that is an app-side action leaves the MC6 doing nothing at all,
  -- which is right: the app is the only thing that can act on it.
  -- Choosing a loop *is* opening its page, so the jump is a consequence of the
  -- duty rather than a second thing the switch happens to carry.
  -- **Choosing a loop goes nowhere, and the hold is what opens its page.** The
  -- tap used to make the jump; it was taking a foot off the Loops page for
  -- nothing, since the verbs live on the toolbar now and the next loop you
  -- want is on the page you were already standing on.
  assert "selecting a loop goes nowhere, on any gesture it carries"
    (LB.sendsTo LB.LoopBank 0 LB.Tap == Nothing
      && LB.sendsTo LB.LoopBank 0 LB.Double == Nothing
      && LB.sendsTo LB.LoopBank 0 LB.Hold == Nothing
      && LB.sendsTo LB.LoopPage 5 LB.Tap == Just (LB.ToSlot LB.ConfigBank)
      && LB.sendsTo LB.ConfigBank 0 LB.Tap == Just (LB.ToSlot LB.QuantiseBank)
      && LB.sendsTo LB.ConfigBank 0 LB.Hold == Nothing)

  -- **The two the pedal cannot reach are exactly the two the Grab bank aims
  -- at**, and both lists come from `loopRows` rather than from each other. A
  -- literal on either side would let the grid grow a column and leave one of
  -- them describing the old shape — which is the failure that would look like
  -- a routing bug, since every loop would still be reachable from *somewhere*.
  assert "the grab loops are the fourth column, and nothing overlaps"
    (LB.grabLoops == [ 7, 3 ]
      && Array.null (Array.intersect LB.grabLoops LB.switchLoops)
      && Array.sort (LB.grabLoops <> LB.switchLoops)
        == Array.range 0 (LB.nLoops - 1))

  -- The Grab bank's own layout has to agree with `grabSwitchForLoop`, because
  -- the screen prints the letter from one and the foot presses the other.
  -- Getting this wrong prints "A" beside loop 4 and sends you to loop 8, and
  -- the two would still agree about which loops are involved.
  assert "and each of them is under the switch its letter claims"
    (Array.all
      (\l -> map (\i -> LB.dutyAt LB.GrabBank i) (Array.fromFoldable (LB.grabSwitchForLoop l))
               == [ Just (LB.SelectLoop l) ])
      LB.grabLoops
      && LB.grabSwitchForLoop 0 == Nothing
      && map (LB.faceLoopKey (LB.face (Just LB.GrabBank))) [ 3, 7 ] == [ "D", "A" ]
      -- And nowhere else, because on every other bank those switches are
      -- something else entirely.
      && map (LB.faceLoopKey (LB.face (Just LB.SpeedBank))) [ 3, 7 ] == [ "4", "8" ])

  -- **One switch shuttles between the two pages that get used**, and it is the
  -- one whose own timing does not matter. Arm waits for your note, so the
  -- double-tap window it now sits behind costs nothing; Record could not have
  -- paid that and is untouched.
  assert "the grab bank is a hold away, and the loops are a hold back"
    (LB.sendsTo LB.LoopBank 6 LB.Hold == Just (LB.ToSlot LB.GrabBank)
      && LB.sendsTo LB.GrabBank 6 LB.Hold == Just (LB.ToSlot LB.LoopBank)
      && LB.sendsTo LB.SpeedBank 6 LB.Hold == Just (LB.ToSlot LB.GrabBank)
      -- Arm itself is unmoved; only what a hold on it means is new.
      && LB.dutyAt LB.GrabBank 6 == Just LB.ArmLoop
      -- And the printed way out is on the bank as well, which is the one you
      -- can find without remembering a hold.
      && LB.sendsTo LB.GrabBank 2 LB.Tap == Just (LB.ToSlot LB.LoopBank))

  log ""
  log "What a gesture means (Data.Looper.Machine)..."

  let idle n = { index: n, state: "idle", layers: 0, loopFrames: 0, loopSecs: 0.0
               , pos: 0, phase: 0.0, armed: false, recording: false, quant: false
               , muted: false, reverse: false, pan: 64, speed: 1.0, pendulum: false
               , oneShot: false, levelArm: false, firing: false
               , chance: 1.0, skipping: false, fadeMs: 0.0, decayDb: 0.0
               , volDb: 0.0, cycles: 0, src: 1, mono: false, revox: false
               , fbDb: -3.0, toneHz: 6500.0, recEnv: []
               , pendingAt: -1, shapes: [] }
      withState n s ls = (idle n) { state = s, layers = ls }
      rigOf ls = { loops: ls, focus: 0, click: false, monitor: false, armDb: -36.0, launchQ: -1 }
      isCommand = case _ of
        Machine.Command _ -> true
        _ -> false

  -- **A loop switch is a place, not a verb.** It used to be seven verbs in a
  -- trenchcoat — record, close, overdub, cancel-arm, fire, stop, start — chosen
  -- from what the daemon last reported, with nothing underfoot saying which was
  -- live. Now it selects the loop and the MC6 opens its page from the jump this
  -- table put on the switch. The verbs are on that page with their names printed.
  --
  -- Switch 0 is A, the near-left switch, and since the surfaces were harmonised
  -- it selects **loop 5** — the bottom-left loop of the grid. The loop it is
  -- pointed at is beside the point here; that it does nothing else is the
  -- claim.
  assert "a loop switch selects and does nothing else, whatever the loop is doing"
    (Array.all
      (\st -> Machine.act (rigOf (Array.replicate 5 st)) (LB.switchGesture LB.LoopBank 0 LB.Tap)
                == [ Machine.Focus 4, Machine.Handled "loop 5" ])
      [ idle 0
      , withState 0 "recordingFirst" 0
      , withState 0 "playing" 3
      , (withState 0 "playing" 1) { muted = true }
      , (idle 0) { armed = true }
      ])

  -- Doing something as well was considered: recording an empty loop on the way
  -- in would save a press. Rejected, because the same switch would then stop a
  -- playing one — so you could not look at a loop without acting on it, which is
  -- the thing the page exists to end.
  assert "and selecting a loop never sends a command"
    (Array.null
      (Array.filter isCommand
        (Machine.act (rigOf [ idle 0, withState 1 "playing" 2 ])
          (LB.switchGesture LB.LoopBank 1 LB.Tap))))

  -- **A grab is four commands and the order is the claim.** The transport
  -- first, because link-spike schedules it for the next bar line and the three
  -- that follow are all trying to land on that same line; then the grid, the
  -- length and the record, which is exactly what a hand sends from the Loops
  -- page. Nothing about it is new to the daemon — what is new is that the beat
  -- starts underneath it.
  assert "a grab starts the session, then opens a take of the length it names"
    (Machine.act ((rigOf [ idle 0, idle 1, idle 2, idle 3 ]) { focus = 3 })
       (LB.switchGesture LB.GrabBank 4 LB.Tap)
      == [ Machine.Command "play1"
         , Machine.Command "3g1"
         , Machine.Command "3len4"
         , Machine.Command "3r"
         ]
      && Machine.act ((rigOf [ idle 0, idle 1, idle 2, idle 3 ]) { focus = 3 })
           (LB.switchGesture LB.GrabBank 1 LB.Tap)
        == [ Machine.Command "play1"
           , Machine.Command "3g1"
           , Machine.Command "3len8"
           , Machine.Command "3r"
           ])

  -- **Layering needs no switch and no branch.** `r` on a loop with material is
  -- an overdub, `g1` on a loop already on the grid is a no-op and `len` on a
  -- loop already that long changes nothing — so the second grab is byte for
  -- byte the first one, and there is no state to read wrong. Grabbing a hat
  -- over a kick is the same press twice.
  assert "and a second grab is the same four commands, over the first"
    (Machine.act ((rigOf [ (withState 0 "playing" 1) { quant = true } ]) { focus = 0 })
       (LB.switchGesture LB.GrabBank 4 LB.Tap)
      == Machine.act ((rigOf [ idle 0 ]) { focus = 0 })
           (LB.switchGesture LB.GrabBank 4 LB.Tap))

  -- Rig-wide, so no loop prefix — the same shape as Start All. A bare command
  -- in a log beside a column of prefixed ones reads like one that forgot its
  -- prefix, which is why this is asserted rather than assumed.
  assert "and Halt stops the session, not a loop"
    (Machine.act ((rigOf [ idle 0, idle 1 ]) { focus = 1 })
       (LB.switchGesture LB.GrabBank 5 LB.Tap)
      == [ Machine.Command "play0" ])

  assert "recording an empty loop opens a take"
    (Machine.act (rigOf [ idle 0 ]) (LB.switchGesture LB.LoopPage 0 LB.Tap)
      == [ Machine.Command "0r" ])

  -- The one that was wrong in use. Undo removes a layer and deliberately keeps
  -- the loop's length, so undoing the last one leaves layers 0, a length, and a
  -- state still reading "playing". Testing emptiness as `state == "idle" &&
  -- layers == 0` made that a playing loop: Record offered stop, and a loop
  -- undone to nothing could never be recorded into from the board again.
  assert "a loop undone to nothing records again, length and state notwithstanding"
    (Machine.act (rigOf [ (withState 0 "playing" 0) { loopFrames = 155215 } ])
       (LB.switchGesture LB.LoopPage 0 LB.Tap)
      == [ Machine.Command "0r" ])

  -- One command, because the engine has one command: `r` opens a first take,
  -- closes it, opens and closes an overdub, and cancels a wait. Four of the old
  -- tap's seven branches were this one verb.
  assert "and closes one that is open, whichever way it is open"
    (Array.all
      (\s -> Machine.act (rigOf [ withState 0 s 1 ]) (LB.switchGesture LB.LoopPage 0 LB.Tap)
               == [ Machine.Command "0r" ])
      [ "recordingFirst", "overdubbing", "multiplying" ])

  -- A listening loop holds the one converter the rig has and locks out the
  -- other five, so taking the wait back has to be reachable.
  assert "and takes back a wait that may never end"
    (Machine.act (rigOf [ (idle 0) { armed = true } ]) (LB.switchGesture LB.LoopPage 0 LB.Tap)
      == [ Machine.Command "0r", Machine.Handled "loop 1 stopped listening" ])

  -- Transport. Explicit h0/h1 rather than a flipping h, because a stopped loop
  -- is invisible and a dropped toggle would leave the app and the engine
  -- disagreeing with nothing on screen to show it.
  assert "Stop/Go stops a playing loop and brings back a stopped one"
    (Machine.act (rigOf [ withState 0 "playing" 1 ]) (LB.switchGesture LB.LoopPage 2 LB.Tap)
      == [ Machine.Command "0h0" ]
      && Machine.act (rigOf [ (withState 0 "playing" 1) { muted = true } ])
           (LB.switchGesture LB.LoopPage 2 LB.Tap)
        == [ Machine.Command "0h1" ])

  -- Not the overload sneaking back: a one-shot is silent between passes by
  -- definition, so it has no playing and stopped to move between.
  assert "and fires a one-shot, which has no playing and stopped to move between"
    (Machine.act (rigOf [ (withState 0 "playing" 1) { oneShot = true } ])
       (LB.switchGesture LB.LoopPage 2 LB.Tap)
      == [ Machine.Command "0f" ])

  assert "and refuses a loop with nothing in it rather than inventing a take"
    (Machine.act (rigOf [ idle 0 ]) (LB.switchGesture LB.LoopPage 2 LB.Tap)
      == [ Machine.Unavailable "loop 1 has nothing to play" ])

  -- Overdubbing onto something you cannot hear is a way to record a mistake
  -- twice, so the loop comes back first.
  assert "Overdub unmutes a stopped loop before going over it"
    (Machine.act (rigOf [ (withState 0 "playing" 1) { muted = true } ])
       (LB.switchGesture LB.LoopPage 1 LB.Tap)
      == [ Machine.Command "0h1", Machine.Command "0r" ])

  -- Starting a first take here would be Overdub quietly becoming Record, which
  -- is the switch immediately to its left.
  assert "and refuses an empty loop rather than becoming Record"
    (Machine.act (rigOf [ idle 0 ]) (LB.switchGesture LB.LoopPage 1 LB.Tap)
      == [ Machine.Unavailable "loop 1 is empty — record it first" ])

  -- The mode and the gesture in one press: `lev1` so the `r` that follows finds
  -- the loop listening and waits for a sound rather than starting on the foot.
  assert "Listen arms and starts waiting, in that order"
    (Machine.act (rigOf [ idle 0 ]) (LB.switchGesture LB.LoopPage 3 LB.Tap)
      == [ Machine.Command "0lev1", Machine.Command "0r" ])

  assert "and says so rather than arming twice"
    (Machine.act (rigOf [ (idle 0) { armed = true } ]) (LB.switchGesture LB.LoopPage 3 LB.Tap)
      == [ Machine.Handled "loop 1 is already listening" ])

  -- **The one that actually bit, and it bit at a glance.** A loop undone to
  -- nothing keeps its length, so recording into it again is an *overdub* with
  -- `layers == 0`. The display asked about emptiness before it asked about
  -- writing, so that loop was drawn as an empty slot while it held the one
  -- converter the rig has. The word underneath said "overdub"; nobody reads the
  -- word, they read the colour.
  assert "a loop that is writing is drawn as writing, layers or no layers"
    (Array.all
      (\s -> Slots.stateClass (withState 0 s 0) == "is-recording"
               && Slots.stateClass (withState 0 s 2) == "is-recording")
      [ "recordingFirst", "overdubbing", "multiplying" ])

  -- And writing outranks stopped, because a loop being recorded into while
  -- silenced is the thing a player most needs told.
  assert "and outranks being stopped, which is the whole reason it is asked first"
    (Slots.stateClass ((withState 0 "overdubbing" 2) { muted = true }) == "is-recording"
      && Slots.stateClass ((withState 0 "playing" 2) { muted = true }) == "is-stopped"
      && Slots.stateClass (idle 0) == "is-empty")

  -- The meaning table and the display have to agree about which states those
  -- are. They did not, and the difference was exactly `overdubbing`.
  assert "the machine and the display ask one predicate, not two lists"
    (Array.all LooperSock.isWriting
       (map (\s -> withState 0 s 0) [ "recordingFirst", "overdubbing", "multiplying" ])
      && not (LooperSock.isWriting (withState 0 "playing" 2))
      && not (LooperSock.isWriting (idle 0)))

  -- **`stateWord`'s guards, in the order that matters.**
  --
  -- The word under a slot is decided by four orthogonal flags before the phase
  -- is consulted at all, and the ordering among them is not arbitrary — each
  -- of these was got wrong at some point and each looks harmless in isolation.
  --
  -- Armed outranks emptiness, and it has to: a level-armed loop is empty by
  -- definition — that is what it is waiting to stop being — so asking the layer
  -- count first made the one state the player most needs to see the one state
  -- that could never be shown.
  assert "listening outranks empty, or arming can never be displayed"
    (Slots.stateWord ((idle 0) { state = "armed" }) == "listening"
      && Slots.stateWord ((idle 0) { state = "armed", pendingAt = 4410 }) == "waiting")

  -- A loop undone to nothing keeps its length and its phase: the engine still
  -- calls it playing and there is nothing to play. The footer still shows the
  -- length, which is the useful half.
  assert "an undone loop reads empty, not playing"
    (Slots.stateWord ((withState 0 "playing" 0) { loopFrames = 155215 }) == "empty"
      && Slots.stateWord (withState 0 "playing" 0) == "")

  -- Three things that are all "not sounding right now" and are deliberately
  -- three different words, because they come back by three different routes.
  assert "stopped, sitting out and ready are told apart"
    (Slots.stateWord ((withState 0 "playing" 2) { muted = true }) == "stopped"
      && Slots.stateWord ((withState 0 "playing" 2) { skipping = true }) == "sitting out"
      && Slots.stateWord ((withState 0 "playing" 2) { oneShot = true }) == "ready"
      && Slots.stateWord ((withState 0 "playing" 2) { oneShot = true, firing = true })
           == "firing")

  assert "and the phases keep the daemon's meaning in shorter words"
    (Slots.stateWord (withState 0 "recordingFirst" 1) == "recording"
      && Slots.stateWord (withState 0 "overdubbing" 2) == "overdub"
      && Slots.stateWord (withState 0 "multiplying" 2) == "multiply"
      && Slots.stateWord (withState 0 "playing" 2) == "playing")

  -- **The one guard on `phaseOf`'s catch-all.**
  --
  -- Unknown words become `Idle`, mirroring `state_name`'s own `_ => "idle"` in
  -- `itajara/src/engine.rs`. That is right for a version skew and wrong for a
  -- typo, and nothing else in the app can tell the two apart — a mistyped
  -- phase name would simply read as idle for ever.
  --
  -- So: every phase, spelled by `phaseName`, must survive the wire and come
  -- back as itself. Fed through `withState` so it travels as the daemon sends
  -- it — a `String` in a snapshot — rather than being compared to a second
  -- copy of the list written here.
  assert "every phase round-trips through the wire word"
    (Array.all
      (\p -> LooperSock.phaseOf (withState 0 (LooperSock.phaseName p) 0) == p)
      LooperSock.allPhases)

  -- Six, and the same six the daemon can emit. A seventh added on one side
  -- only is what this whole type exists to catch.
  assert "and there are six of them"
    (Array.length LooperSock.allPhases == 6
      && Array.length (Array.nub (map LooperSock.phaseName LooperSock.allPhases)) == 6)

  -- A word the daemon has never sent is idle, not a crash and not a seventh
  -- constructor leaking into the display.
  assert "an unrecognised word reads as idle, exactly as the daemon does it"
    (LooperSock.phaseOf (withState 0 "granulating" 0) == LooperSock.Idle)

  log ""
  log "The daemon's vocabulary (Data.Looper.Verb)..."

  -- **Every verb, spelled out once, against `dispatch` in engine.rs.**
  --
  -- These are transcribed from the daemon's match arms, not from `render` — a
  -- test that reads the implementation back to itself would have passed on the
  -- day the two tables disagreed, which is the whole reason this type exists.
  -- If one of these fails, check engine.rs before changing the expectation.
  assert "bare verbs spell as the daemon's dispatch arms"
    (LoopVerb.render LoopVerb.Record == "r"
      && LoopVerb.render LoopVerb.Multiply == "x"
      && LoopVerb.render LoopVerb.Rotate == "o"
      && LoopVerb.render LoopVerb.Dense == "d"
      && LoopVerb.render LoopVerb.Undo == "u"
      && LoopVerb.render LoopVerb.Redo == "y"
      && LoopVerb.render LoopVerb.ForgetLength == "z"
      && LoopVerb.render LoopVerb.Clear == "c"
      && LoopVerb.render LoopVerb.Fire == "f")

  -- The digit is audibility, not hush — `h1` clears `muted`. Getting this
  -- backwards is how Stop All came to arm a trap in every empty slot.
  assert "flags always take the explicit form, and h1 means audible"
    (LoopVerb.render (LoopVerb.Sounding true) == "h1"
      && LoopVerb.render (LoopVerb.Sounding false) == "h0"
      && LoopVerb.render (LoopVerb.OnGrid true) == "g1"
      && LoopVerb.render (LoopVerb.Reversed false) == "rev0"
      && LoopVerb.render (LoopVerb.Pendulum true) == "pend1"
      && LoopVerb.render (LoopVerb.OneShot false) == "one0"
      && LoopVerb.render (LoopVerb.LevelArm true) == "lev1"
      && LoopVerb.render (LoopVerb.Click false) == "k0"
      && LoopVerb.render (LoopVerb.Monitor true) == "m1")

  assert "numeric verbs carry their argument with no separator"
    (LoopVerb.render (LoopVerb.Rate 0.5) == "sp0.5"
      && LoopVerb.render (LoopVerb.Place 64) == "pan64"
      && LoopVerb.render (LoopVerb.Spread 2) == "s2"
      && LoopVerb.render (LoopVerb.Fade 50.0) == "xf50.0"
      && LoopVerb.render (LoopVerb.Decay 3.0) == "dec3.0"
      && LoopVerb.render (LoopVerb.Chance 0.25) == "ch0.25")

  -- **`sp` has to be matched before `s`, and that is the daemon's problem, not
  -- ours — but it is why these two are asserted side by side.** `s` prefix-
  -- matches, so `sp0.5` once read as "sparse, cannot parse the count, use 2"
  -- and quietly did a multiply. Nothing acked it and it did something else
  -- entirely. If either spelling ever changes, this is where to look.
  assert "the two s-verbs stay distinguishable"
    (LoopVerb.render (LoopVerb.Rate 0.5) /= LoopVerb.render (LoopVerb.Spread 2))

  -- The bare verbs the board reaches only through `Machine.act`, pinned here
  -- too so their spelling is checked directly rather than incidentally.
  --
  -- `ClickToggle` used to be in this list, rendering `k`. It was the one
  -- flipping form the app still sent, and it is gone — the machine sets the
  -- click from what the daemon reported. See the note on `Verb.Click`.
  assert "capture and save spell as the daemon expects"
    (LoopVerb.render LoopVerb.ClaimPast == "t"
      && LoopVerb.render (LoopVerb.SaveTake "") == "w"
      && LoopVerb.render (LoopVerb.SaveTake "riff") == "wriff")

  -- A loop prefix on every board command, because the daemon's own selection is
  -- a mode a footswitch could fall out of step with.
  assert "a verb addressed to a loop leads with its index"
    (LoopVerb.at 0 LoopVerb.Record == "0r"
      && LoopVerb.at 3 (LoopVerb.Sounding false) == "3h0"
      && LoopVerb.at 5 (LoopVerb.Rate 1.0) == "5sp1.0")

  -- **Save Take used to write the wrong loop and report success.**
  --
  -- It sent a bare `w`, which the daemon applies to *its* selection — a field
  -- nothing on this surface has ever written, and which has therefore read zero
  -- since the six-loop page was built. So it saved loop 1's layers whatever the
  -- board was focused on, and said so cheerfully, because saving loop 1 is a
  -- perfectly good save.
  --
  -- Tested at focus 3 specifically: at focus 0 the correct and the broken
  -- output are the same string, which is exactly why nobody saw it.
  -- **Off the MC6 since the toolbar stopped duplicating the Twister**, so the
  -- duty is asked for directly. What it pins is unchanged and is the reason the
  -- test exists: a per-loop verb carries the focused loop, because unprefixed
  -- it would reach the daemon's own selection, which nothing here writes.
  assert "Save Take writes the focused loop, not the daemon's idea of selected"
    (Machine.perform ((rigOf [ idle 0, idle 1, idle 2, idle 3 ]) { focus = 3 })
       LB.Focused LB.SaveTake
      == [ Machine.Command "3w" ])

  -- The metronome is global — `sh.click`, not `lp.click` — so this one is right
  -- to stay bare, and a loop index on it would be noise.
  assert "and the click stays unprefixed, because it is not a per-loop thing"
    (Machine.perform ((rigOf [ idle 0, idle 1 ]) { focus = 1 })
       LB.Focused LB.ClickToggle
      == [ Machine.Command "k1" ])

  -- **Set, both ways.** It sent the flipping `k` until `Rig` carried the global
  -- flags; both directions are pinned here because a switch that sets is only
  -- better than one that flips if it reads the current value, and a test that
  -- only ever starts from off cannot tell the two apart.
  assert "and it sets the click from what the daemon reported, in both directions"
    (Machine.perform ((rigOf [ idle 0 ]) { click = true }) LB.Focused LB.ClickToggle
      == [ Machine.Command "k0" ]
      && Machine.perform ((rigOf [ idle 0 ]) { click = false }) LB.Focused LB.ClickToggle
        == [ Machine.Command "k1" ])

  -- **Stop All reaches the loops that have something to stop, and no others.**
  -- Muting an empty loop does nothing audible and leaves it silenced for
  -- whatever is recorded into it next — so a stop anywhere in the set used to
  -- arm a trap in all six, and the next take recorded perfectly and silently.
  assert "stop all reaches every loop that has anything in it"
    (Machine.act (rigOf [ withState 0 "playing" 1, idle 1, withState 2 "playing" 3 ])
       (LB.switchGesture LB.LoopBank 9 LB.Tap)
      == [ Machine.Command "0h0", Machine.Command "2h0" ])

  assert "and reaches nothing at all when there is nothing to stop"
    (Machine.act (rigOf [ idle 0, idle 1 ]) (LB.switchGesture LB.LoopBank 9 LB.Tap) == [])

  -- The other half of the same trap: Record on a loop that was silenced brings
  -- it back first, because working on something you cannot hear is never what
  -- was meant.
  assert "recording a muted loop unmutes it first"
    (Machine.act (rigOf [ (idle 0) { muted = true } ]) (LB.switchGesture LB.LoopPage 0 LB.Tap)
      == [ Machine.Command "0h1", Machine.Command "0r" ]
      && Machine.act (rigOf [ (withState 0 "playing" 2) { muted = true } ])
           (LB.switchGesture LB.LoopPage 0 LB.Tap)
        == [ Machine.Command "0h1", Machine.Command "0r" ])

  assert "but closing a take that is already audible does not touch the mute"
    (Machine.act (rigOf [ withState 0 "recordingFirst" 0 ]) (LB.switchGesture LB.LoopPage 0 LB.Tap)
      == [ Machine.Command "0r" ])

  -- Whatever the engine calls it, no layers means record.
  assert "any state with no layers records"
    (Machine.act (rigOf [ withState 0 "weird" 0 ]) (LB.switchGesture LB.LoopPage 0 LB.Tap)
      == [ Machine.Command "0r" ])

  -- The page acts on the loop the loop bank selected. One page serving six
  -- loops only works because of that, and it is the same arrangement the config
  -- family has always had.
  assert "the page acts on the focused loop, not on the switch pressed"
    (Machine.act ((rigOf [ idle 0, idle 1, withState 2 "playing" 2 ]) { focus = 2 })
       (LB.switchGesture LB.LoopPage 2 LB.Tap)
      == [ Machine.Command "2h0" ])

  assert "and its Config switch only agrees the MC6 changed bank"
    (Machine.act (rigOf [ idle 0 ]) (LB.switchGesture LB.LoopPage 5 LB.Tap)
      == [ Machine.Handled "showing config" ])

  -- Both off the MC6 now and on the Twister's first page, so both are asked
  -- for directly. The property is the one that mattered: the config family acts
  -- on the focused loop, never on whichever switch was pressed.
  -- **A double on a loop switch names its own loop**, which is why `act` grew a
  -- subject after saying for months that it never would. The device suppresses
  -- the tap on a double, so nothing ever says you touched that switch — with
  -- `Focused` this would have undone whichever loop you happened to have chosen
  -- before, silently and on the wrong take.
  --
  -- Switch 3 is loop 1: the six are in grid order, not switch order, because A
  -- is the bottom-left switch and loop 5 is the bottom-left loop.
  assert "a double on a loop switch undoes that loop, and takes it in hand"
    (Machine.act ((rigOf [ idle 0, idle 1, idle 2 ]) { focus = 2 })
       (LB.switchGesture LB.LoopBank 3 LB.Double)
      == [ Machine.Focus 0, Machine.Command "0u" ])

  -- And the tap still means what it meant: choose, and nothing else. It is the
  -- whole of the workflow — stand on Loops, press a loop, record it on `I`,
  -- double back to undo, press the next loop — and the reason the jump moved
  -- to the hold.
  assert "and the tap on the same switch still just chooses it"
    (Machine.act ((rigOf [ idle 0, idle 1, idle 2 ]) { focus = 2 })
       (LB.switchGesture LB.LoopBank 3 LB.Tap)
      == [ Machine.Focus 0, Machine.Handled "loop 1" ])

  assert "undo and clear act on the focused loop"
    (Machine.perform ((rigOf [ idle 0, idle 1, idle 2 ]) { focus = 2 }) LB.Focused LB.Undo
      == [ Machine.Command "2u" ]
      && Machine.perform ((rigOf [ idle 0 ]) { focus = 1 }) LB.Focused LB.ClearLoop
        == [ Machine.Command "1c" ])

  -- The config family acts on the focused loop, which is what a hold sets. One
  -- config bank serving six loops only works because of that.
  assert "reverse and clear act on the focused loop, not the pressed switch"
    (Machine.act ((rigOf [ idle 0, idle 1, idle 2 ]) { focus = 2 }) (LB.switchGesture LB.ConfigBank 3 LB.Tap)
      == [ Machine.Command "2rev1" ]
      && Machine.perform ((rigOf []) { focus = 1 }) LB.Focused LB.ClearLoop
        == [ Machine.Command "1c" ])

  assert "the pan bank places the focused loop across the field"
    (map (\i -> Machine.act ((rigOf []) { focus = 0 }) (LB.switchGesture LB.PanBank i LB.Tap))
       [ 0, 2, 4 ]
      == [ [ Machine.Command "0pan0" ]
         , [ Machine.Command "0pan64" ]
         , [ Machine.Command "0pan127" ] ])

  -- Free and Grid are real; the bar counts have nothing to select, because the
  -- engine's grid is the anchor loop's cycle and not a bar.
  assert "quantise sets the grid flag and is honest about bar counts"
    (Machine.act ((rigOf []) { focus = 3 }) (LB.switchGesture LB.QuantiseBank 0 LB.Tap)
      == [ Machine.Command "3g0" ]
      && Machine.act ((rigOf []) { focus = 3 }) (LB.switchGesture LB.QuantiseBank 1 LB.Tap)
        == [ Machine.Command "3g1"
           , Machine.Handled "on the grid — bar counts need the frame-to-bar join" ])

  assert "and stop-all works the same from any bank"
    (Machine.act (rigOf [ withState 0 "playing" 1 ]) (LB.switchGesture LB.QuantiseBank 9 LB.Tap)
      == [ Machine.Command "0h0" ])

  -- Direction is the sign of speed, not a second control, so the bottom row is
  -- one press that says both things rather than two in the right order.
  -- The legend on the Looper page is what a player reads to find out what the
  -- six unmarked footswitches do, and for a while it was a hand-written copy of
  -- the loop bank's six shown whatever bank the board was on. So with the board
  -- on config it said J was Clear while J was End Stop — which reads exactly
  -- like a switch wired to the wrong place, and is worse than saying nothing.
  assert "the aux legend is the bank's own table"
    (LB.auxLegend LB.LoopBank
      == [ { key: "G", what: "Arm" }, { key: "H", what: "Reverse" }
         , { key: "I", what: "Record" }, { key: "J", what: "Stop All" }
         , { key: "K", what: "Half Spd" }, { key: "L", what: "Overdub" } ])

  -- **The rule about feet.** G to L have no markings, so they are remembered as
  -- positions; a switch that clears a loop on one page and sets an end-state on
  -- the next cannot be learned at all. Everything but the way out is identical
  -- on every bank, and the way out differs only in where "out" is.
  -- **All six now, where it used to be five.** The way out was G's tap and so
  -- was the one position that meant something different depending on where you
  -- were standing; it is J's hold now, and a hold is not what an unmarked
  -- switch is remembered by. Every tap on every bank is the same six.
  assert "the toolbar means the same thing on every bank"
    (Array.all
      (\slot -> map _.what (LB.auxLegend slot)
        == [ "Arm", "Reverse", "Record", "Stop All", "Half Spd", "Overdub" ])
      LB.allSlots)

  -- The second gesture, where a switch carries one. Same six everywhere for
  -- the same reason the first six are: an unmarked switch is a position, and
  -- a position that means different things on different pages cannot be
  -- learned at all.
  -- One switch carries a second gesture now, and it is the set: tap stops it,
  -- double starts it again from the top.
  assert "and so does the second gesture, where there is one"
    (Array.all
      (\slot -> map (map LB.dutyLabel <<< _.double)
        (Array.catMaybes (map (LB.dutiesAt slot) (Array.range 6 11)))
        == [ Nothing, Nothing, Nothing, Just "Start All", Nothing, Nothing ])
      LB.allSlots)

  -- Claiming the past is the live gesture and the one thing no pedal can do;
  -- saving a WAV is never time-critical and was holding the fast slot while
  -- the feature the ring exists for had no switch at all.
  -- The way out is a hold now, so it does not show in the legend at all — the
  -- legend is what the six switches *do*, and holding J is not what J does. It
  -- still differs by where you are standing, which is what this pins.
  assert "and only the way out differs, because only its destination does"
    (map (\slot -> map (map LB.dutyLabel <<< _.hold) (LB.dutiesAt slot 9)) LB.allSlots
      == Array.cons (Just (Just "< Board"))
           (Array.replicate (Array.length LB.allSlots - 1) (Just (Just "< Loops"))))

  -- The code has to say it too, or two tables agree until one of them does not.
  assert "and the meaning table answers the toolbar without consulting the bank"
    (Array.all
      (\slot -> Machine.act ((rigOf [ withState 0 "playing" 1 ]) { focus = 2 })
                  (LB.switchGesture slot 9 LB.Tap)
        == [ Machine.Command "0h0" ])
      LB.allSlots)

  -- **The join that was missing.** The pedal's label and the command on the
  -- wire used to be two tables keyed by a switch number, with nothing but that
  -- number holding them together: the layout said switch 9 was "Clear", the
  -- meaning table said switch 9 sent `c`, and moving Clear would have left a
  -- switch labelled one thing and doing another without failing to compile.
  -- Now both are renderings of one `Duty`, and this is what says so.
  -- A labelled switch that does nothing, and an unlabelled one that does
  -- something, are exactly the two failures the old split allowed.
  assert "every switch's label and its command come from the same value"
    (Array.all
      (\r ->
         let
           labelled = maybe false (\d -> LB.dutyLabel d /= "") (LB.dutyAt r.slot r.i)
           acts = Machine.act ((rigOf []) { focus = 0 }) (LB.switchGesture r.slot r.i LB.Tap)
           blank = Array.any (String.contains (String.Pattern "has nothing on"))
             (map Machine.describe acts)
         in labelled /= blank)
      (do
         slot <- LB.allSlots
         i <- Array.range 0 11
         pure { slot, i }))

  -- The letters are the device's, so they have to come from the same place the
  -- switch numbering does rather than from a second list in the view.
  assert "and the letters run A to L over the twelve switches"
    (Array.mapMaybe LB.switchLetter (Array.range 0 11)
      == [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L" ]
      && LB.switchLetter 12 == Nothing)

  -- Every bank fills all six now, because the toolbar is not optional.
  assert "no bank leaves an unmarked switch nameless"
    (Array.all (\slot -> map _.key (LB.auxLegend slot)
      == [ "G", "H", "I", "J", "K", "L" ]) LB.allSlots)

  -- No reverse row: direction is the sign of speed, so backwards at half speed
  -- is Reverse on the config bank and then a half here.
  assert "the speed bank sends a rate"
    (map (\i -> Machine.act ((rigOf []) { focus = 2 }) (LB.switchGesture LB.SpeedBank i LB.Tap))
       [ 0, 2, 4 ]
      == [ [ Machine.Command "2sp0.25" ]
         , [ Machine.Command "2sp1.0" ]
         , [ Machine.Command "2sp2.0" ] ])

  -- **Set, never flip.** The toggles read the engine's own answer out of the
  -- snapshot and send the explicit form, so a dropped command cannot leave the
  -- app and the engine disagreeing for ever about which way a loop is facing.
  assert "and pendulum is a config switch of its own, sent as a value"
    (Machine.act ((rigOf []) { focus = 4 }) (LB.switchGesture LB.ConfigBank 4 LB.Tap)
      == [ Machine.Command "4pend1" ])

  -- **The mode changes what the switch means, and the switch keeps its name.**
  --
  -- A one-shot is silent between passes by definition, so there is no playing
  -- and stopped for a tap to toggle between — the only thing it can mean is
  -- fire. Which is exactly why the mode rides in the snapshot: what the foot
  -- does depends on a fact only the engine holds, and no amount of remembering
  -- on this side would be as good as being told.
  assert "Stop/Go fires a one-shot, where on any other loop it stops it"
    (Machine.act (rigOf [ (withState 0 "playing" 1) { oneShot = true } ])
       (LB.switchGesture LB.LoopPage 2 LB.Tap)
      == [ Machine.Command "0f" ]
      && Machine.act (rigOf [ withState 0 "playing" 1 ])
           (LB.switchGesture LB.LoopPage 2 LB.Tap)
        == [ Machine.Command "0h0" ])

  -- A level-armed loop waits for a sound that may never come, holding the one
  -- converter the rig has. A press has to be able to take that back, or one
  -- loop can lock out the other five with nothing on screen to blame.
  -- **Stranding a recording is the worst failure this surface has**: one
  -- converter, so a loop left writing locks out all five others, silently, from
  -- a bank you are no longer standing on. It happened twice in one session.
  -- Closing is what the gesture meant either way — a deliberate hold is asking
  -- to configure a loop that has no length yet, and a tap held a little too long
  -- meant to close it.
  -- The hold that used to close a still-writing loop on its way to the config
  -- bank is gone with the hold itself. What replaces it is better: the loop's
  -- page opens on selection with Record right there under A, and the screen says
  -- the loop is still writing.
  assert "Record takes back an arm that is still waiting"
    (Machine.act (rigOf [ (idle 0) { armed = true } ]) (LB.switchGesture LB.LoopPage 0 LB.Tap)
      == [ Machine.Command "0r", Machine.Handled "loop 1 stopped listening" ])

  -- **Through `perform`, not through a switch, since 2026-08-30.** The modes
  -- bank was the price of the Grab bank — seven blocks is all the CC
  -- arithmetic holds — and these five duties are unplaced now, reachable from
  -- the Twister and from nothing underfoot. They are still tested, because
  -- what they compute is not a property of the switch that used to carry them:
  -- a toggle set from what the engine last reported rather than flipped
  -- locally is the rule, and it outlives its bank.
  assert "the modes set their toggles from what the engine last reported"
    (Machine.perform ((rigOf [ (idle 0) { levelArm = true } ]) { focus = 0 })
       LB.Focused LB.OneShot == [ Machine.Command "0one1" ]
      && Machine.perform ((rigOf [ (idle 0) { levelArm = true } ]) { focus = 0 })
           LB.Focused LB.LevelArm == [ Machine.Command "0lev0" ])

  -- Chance steps rather than flipping, and the step is computed from what the
  -- engine last reported — not counted here and not counted on the device. The
  -- MC6's own scroll counters would keep the position on the hardware, and the
  -- hardware is the one thing in this rig that cannot be told it is wrong.
  assert "chance steps down the ladder from wherever the engine says it is"
    (map (\p -> LB.stepChance p) [ 1.0, 0.75, 0.5, 0.25, 0.125 ]
      == [ 0.75, 0.5, 0.25, 0.125, 1.0 ])

  -- A probability that is not on a rung still steps somewhere sensible rather
  -- than falling off the ladder, because it can be set to anything by hand.
  -- A value on no rung is only reachable by typing at the daemon. It goes to
  -- the first rung rather than to whichever one it is nearest, because guessing
  -- is a thing the player would have to learn.
  assert "and a value on no rung starts the ladder again"
    (LB.stepChance 0.6 == 1.0 && LB.stepChance 0.01 == 1.0)

  -- The word lives beside the value in one table, so the switch, the screen and
  -- the wire cannot come to describe different odds.
  -- **Loud is more ink.** The envelope was first drawn as the block's fill with
  -- the waveform in the background colour, which made a loud layer *less* mark
  -- than a quiet one — inverted, and obvious the moment it was on screen. The
  -- floor is because a layer that is quiet is still a layer.
  assert "a louder bucket draws a taller mark, and a silent one is still visible"
    (Slots.waveEdge 255 < Slots.waveEdge 128
      && Slots.waveEdge 128 < Slots.waveEdge 0
      && Slots.waveEdge 255 == 0.0
      && Slots.waveEdge 0 < 0.95
      && Slots.waveEdge 0 > 0.9)

  -- A stepper cannot say where it is, so its long name says where it can go.
  -- The MC6 flashes this on every press and has no way to update it, so the
  -- alternative was a description of the switch you are already standing on.
  assert "a stepper's long name is the ladder itself, and it fits the device"
    (LB.dutyName LB.StepChance == "all 3:4 1:2 1:4 1:8"
      && LB.dutyName LB.StepFade == "hard 10 25 50 100")

  assert "the ladder says its own words"
    (map (\r -> LB.chanceWord r.value) LB.chanceLadder
      == [ "always", "3 in 4", "1 in 2", "1 in 4", "1 in 8" ]
      && LB.chanceWord 0.6 == "60%")

  -- One stepping rule for every ladder, rather than one per parameter: the rung
  -- after the one you are on, and back to the first when there is none.
  assert "the wrap fade walks the same ladder rule, off first and off again"
    (map (\r -> LB.stepFade r.value) LB.fadeLadder
      == [ 10.0, 25.0, 50.0, 100.0, 0.0 ]
      && map (\r -> LB.fadeWord r.value) LB.fadeLadder
        == [ "hard", "10 ms", "25 ms", "50 ms", "100 ms" ])

  assert "and a press sends the fade it stepped to"
    (Machine.perform ((rigOf [ (idle 0) { fadeMs = 25.0 } ]) { focus = 0 })
       LB.Focused LB.StepFade
      == [ Machine.Command "0xf50.0", Machine.Handled "loop 1 wraps 50 ms" ])

  assert "and a press sends the rung it stepped to"
    (Machine.perform ((rigOf [ (idle 0) { chance = 0.5 } ]) { focus = 0 })
       LB.Focused LB.StepChance
      == [ Machine.Command "0ch0.25", Machine.Handled "loop 1 plays 1 in 4" ])

  -- What is not built says what it is waiting for, and says it in the SAME
  -- words on the pedal and on screen. Nothing carries a `NotYet` today — chance
  -- was the last one and it works now — but the vocabulary stays, because a
  -- switch that shrugs must not be tellable from a broken cable only by reading
  -- two files.
  assert "an unimplemented switch names itself and what it waits for"
    (map Machine.describe (Machine.act (rigOf []) (LB.switchGesture LB.SpeedBank 5 LB.Tap))
      == [ "out" ]
      && LB.dutyLabel (LB.NotYet "Groups" "no membership model yet") == "Groups"
      && LB.dutyName (LB.NotYet "Groups" "no membership model yet") == "Groups"
      && LB.dutyLabel (LB.Grid 4) == "4 Bars")

  -- A switch with nothing on it is a different answer from one that is waiting
  -- for the engine, and both are different from silence.
  assert "and a switch with nothing on it says that instead"
    (map Machine.describe (Machine.act (rigOf []) (LB.switchGesture LB.PanBank 99 LB.Tap))
      == [ "pan switch 99 has nothing on it" ])

  -- The letters are only true where the board can reach the loops. With the
  -- board on config, A is Quantise, so labelling a loop "A" there points a foot
  -- at the wrong thing. Loop 1 is switch D and loop 4 has no switch at all.
  assert "a loop is lettered only when the board is on the bank that reaches it"
    (map (LB.faceLoopKey (LB.face (Just LB.LoopBank))) [ 0, 3 ] == [ "D", "4" ]
      && map (LB.faceLoopKey (LB.face (Just LB.ConfigBank))) [ 0, 3 ] == [ "1", "4" ]
      && map (LB.faceLoopKey (LB.face Nothing)) [ 0, 3 ] == [ "1", "4" ])

  -- The device refuses a name it cannot print rather than truncating it, so a
  -- label that does not fit is a build-time problem and not a mystery on the
  -- pedal. Now that the labels are computed from duties rather than written by
  -- hand, that is worth checking here instead of finding out at upload.
  assert "every generated label fits the fields the MC6 has for it"
    (Array.all
      (\b -> Array.all
        (\sw -> String.length sw.label <= 8 && String.length sw.longName <= 24)
        b.switches)
      (LB.banks { base: 22, boardBank: 1 }))

  -- The MC6 numbers from the bottom, so the far row is D E F.
  assert "the board's rows are the device's, not the index order"
    (LB.boardRows == [ [ 3, 4, 5 ], [ 0, 1, 2 ] ])

  assert "every action can say what it did"
    (Array.all (\a -> Machine.describe a /= "")
      [ Machine.Command "0r", Machine.ShowBank LB.ConfigBank, Machine.Focus 0
      , Machine.Unavailable "x", Machine.Handled "y" ])

  -- A loop the snapshot does not contain is not a loop we may guess about.
  assert "a gesture for a loop that is not in the snapshot is refused, not assumed"
    (Machine.act (rigOf []) (LB.switchGesture LB.LoopPage 0 LB.Tap)
      == [ Machine.Unavailable "loop 1 is not in the snapshot" ])

  log ""
  log "Write frames, against Morningstar's own editor..."

  -- Captured with MIDI Monitor spying on destinations while the editor renamed
  -- wire bank 27 to "Twenty Eight" and changed a general setting. Holding our
  -- encoder to those bytes is a stronger check than sending anything: if the
  -- bytes match the editor's, the frame is right without a device having to
  -- accept it, and without a bad guess reaching anybody's flash.

  assert "our bank write is the editor's bank write, byte for byte"
    (SysEx.frameBytes (SysEx.sysexBankData 27 "Twenty Eight" [])
      == Capture.editorFrame_bankWrite)

  -- The name field is 24, which is why this is here rather than assumed: the
  -- longest name on the device is 16 characters, so inferring from data would
  -- have set the limit four characters short of what the device accepts.
  assert "a 24-character bank name still fits the frame it was measured from"
    (Array.length (SysEx.frameBytes (SysEx.sysexBankData 3 "123456789012345678901234" []))
      == Array.length Capture.editorFrame_bankWrite)

  assert "the settings write is bracketed, and the bracket matches"
    (SysEx.frameBytes SysEx.sysexSettingsBegin == Capture.editorFrame_settingsBegin
      && SysEx.frameBytes SysEx.sysexSettingsCommit == Capture.editorFrame_settingsCommit)

  -- The load-bearing one. `04 02` carries the same payload `03 21` returns, so
  -- whatever Data.MC6.Settings decodes can be handed straight back — which is
  -- what makes settings writable rather than merely readable.
  assert "and 04 02 carries exactly what 03 21 returned"
    (SysEx.frameBytes
      (SysEx.sysexSettingsData
        (Array.drop 16 (Array.dropEnd 2 Capture.editorFrame_settingsData)))
      == Capture.editorFrame_settingsData)

  assert "which is the same width as the settings frame the device sends"
    (Array.length (Array.drop 16 (Array.dropEnd 2 Capture.editorFrame_settingsData))
      == Array.length (Array.drop 16 (Array.dropEnd 2 (Capture.settingsFrame 0x21))))

  log ""
  log "Controller settings (Data.MC6.Settings)..."

  -- Against the bytes the device sent, decoded and then checked against the
  -- March backup — two independent descriptions of the same hardware, which is
  -- the only reason any of this counts as confirmed rather than plausible.
  let payloadOf f2 = Array.drop 16 (Array.dropEnd 2 (Capture.settingsFrame f2))
      sectionOf f2 = Settings.decodeSection f2 (payloadOf f2)

  -- Bytes 14-15 are the frame length as two septets. Verified on every settings
  -- frame at once rather than on the two that first suggested it.
  assert "every frame declares its own length, as two septets"
    (Array.all
      (\f2 -> let b = Capture.settingsFrame f2
              in case Array.index b 14, Array.index b 15 of
                   Just hi, Just lo -> hi * 128 + lo == Array.length b
                   _, _ -> false)
      Capture.settingsCodes)

  assert "each sub-code decodes to the section we say it is"
    (map (Settings.sectionName <<< sectionOf) Capture.settingsCodes
      == [ "MIDI channels", "general configuration", "bank order (probably)"
         , "omniports", "waveform engines", "sequencer engines"
         , "scroll counters", "MIDI events", "aux switch ladder"
         , "unknown settings frame 03 41"
         ])

  case sectionOf 0x20 of
    Settings.MidiChannels chs -> do
      assert "sixteen channels come back"
        (Array.length chs == 16)
      -- The reason this decoder exists: the app could not consult the device
      -- about a channel, so it consulted a comment, and the comment was stale.
      assert "and they name the board the device knows"
        (map _.name chs ==
          [ "MC6", "(Brothers)", "MOOD", "Clean", "Hedra", "", "Flint", "Lex"
          , "", "Iridium", "Riverside", "Mercury7", "", "Brig", "Habit", "LoopyPro" ])
      assert "channel 15 is Habit, as the pedal answering on it agrees"
        (map _.name (Array.filter (\c -> c.channel == 15) chs) == [ "Habit" ])
      -- The two-septet mask, and the one channel that is not like the others.
      assert "every channel sends to all ports but one"
        (Array.length (Array.filter (\c -> c.sendToPort == 2047) chs) == 15
          && map _.sendToPort (Array.filter (\c -> c.channel == 16) chs) == [ 2034 ])
      assert "and channel 9, which we took, is unnamed and unrestricted"
        (map (\c -> { n: c.name, p: c.sendToPort })
          (Array.filter (\c -> c.channel == LB.switchChannel) chs)
            == [ { n: "", p: 2047 } ])
    _ -> assert "03 20 is the channel table" false

  -- The section a factory reset would destroy and we could not put back.
  case sectionOf 0x23 of
    Settings.Omniports ports -> do
      assert "both omniports come back, in the FS3X three-switch mode"
        (map _.portType ports == [ 8, 8 ])
      assert "with the fixed switch numbers the backup records"
        (map (\p -> { n: p.portNum, t: p.tip, r: p.ring, tr: p.tipRing }) ports ==
          [ { n: 0, t: [41,127,127], r: [42,127,127], tr: [43,127,127] }
          , { n: 1, t: [38,127,127], r: [39,127,127], tr: [40,127,127] }
          ])
    _ -> assert "03 23 is the omniports" false

  case sectionOf 0x24 of
    Settings.WaveformEngines ws ->
      assert "four waveform engines, matching the backup"
        (map (\w -> [ w.num, w.min, w.max, w.waveform ]) ws
          == [ [0,20,100,4], [1,0,127,2], [2,0,127,5], [3,0,0,0] ])
    _ -> assert "03 24 is the waveform engines" false

  case sectionOf 0x25 of
    Settings.SequencerEngines es ->
      -- Engine 0 matches the March backup byte for byte; engine 1 does not,
      -- because it was edited between March and August. That the first still
      -- matches across five months is the check that counts.
      assert "the first sequencer engine matches the backup step for step"
        (map _.steps (Array.take 1 es)
          == [ [4,0,127,2,0,127,5,0,0,0,0,119,127,39,127,127] ])
    _ -> assert "03 25 is the sequencer engines" false

  case sectionOf 0x26 of
    Settings.ScrollCounters cs ->
      assert "sixteen scroll counters, all 0 to 127 from 0"
        (Array.length cs == 16
          && Array.all (\c -> c.min == 0 && c.max == 127 && c.start == 0) cs)
    _ -> assert "03 26 is the scroll counters" false

  case sectionOf 0x27 of
    Settings.MidiEvents evs ->
      assert "sixteen MIDI event slots with the backup's field order"
        (Array.length evs == 16
          && Array.all (\e -> e.numberFrom == 127 && e.channelFrom == 15
                              && e.typeFrom == 7 && e.flags == [1,1,1]) evs)
    _ -> assert "03 27 is the MIDI events" false

  -- The one that does not add up, reported rather than rounded away.
  case sectionOf 0x28 of
    Settings.AuxLadder aux -> do
      assert "the aux ladder claims eight switches"
        (aux.claimed == 8)
      assert "and the frame carries seven, which we say out loud"
        (Array.length aux.switches == 7 && aux.truncated)
    _ -> assert "03 28 is the aux ladder" false

  case sectionOf 0x21 of
    Settings.General g -> do
      -- Offset 3, confirmed by moving the setting 750 -> 700 with the write
      -- captured and then reading the device back. In this August capture it
      -- reads 2; the editor wrote 4 for 700 ms and the device now reports 4.
      assert "the long-press setting is offset 3, reading 2 in this capture"
        (Settings.longPressSetting g == Just 2)
      -- Offset 13 held 12 here, in the editor's write, and in a read taken with
      -- the setting at a different value. A constant cannot be the field, which
      -- is what retired the earlier guess.
      assert "and offset 13 is constant across captures, so it is not the field"
        (Array.index g.bytes 13 == Just 12)
      assert "bankChangeDisplayTime is 60, on weaker evidence and labelled so"
        (Settings.bankChangeDisplayTimeProbably g == Just 60)
    _ -> assert "03 21 is the general configuration" false

  -- Still not understood, and kept whole rather than dropped.
  case sectionOf 0x29 of
    Settings.UnknownSettings code p ->
      assert "the section we cannot read is kept, not discarded"
        (code == 0x29 && Array.length p == 34)
    _ -> assert "03 29 is still unknown" false

  log ""
  log "Typed model (Data.MC6.Model)..."

  -- Every wire shape in the March backup of the real device: 5760 message
  -- slots across 30 banks. The point of the model is that it can be introduced
  -- without risking a byte of a device somebody has spent years programming,
  -- and that claim is exactly `toWire <<< fromWire == identity` — over data the
  -- device produced, not data we produced.
  let wireSample :: Array _
      wireSample =
        [ { msgType: MsgEmpty, channel: 1, data1: 0, data2: 0, data3: 0, data4: 0
          , action: ActionNone, togglePosition: ToggleOff, msgIndex: 0 }
        , { msgType: MsgCC, channel: 3, data1: 105, data2: 127, data3: 0, data4: 0
          , action: ActionPress, togglePosition: ToggleOn, msgIndex: 1 }
        , { msgType: MsgPC, channel: 8, data1: 12, data2: 0, data3: 0, data4: 0
          , action: ActionPress, togglePosition: ToggleBoth, msgIndex: 2 }
        , { msgType: MsgBankJump, channel: 1, data1: 22, data2: 0, data3: 0, data4: 0
          , action: ActionLongPress, togglePosition: ToggleBoth, msgIndex: 3 }
        -- The device's own bank jump. Same type byte, different shape.
        , { msgType: MsgBankJump, channel: 1, data1: 0, data2: 0, data3: 6, data4: 0
          , action: ActionRelease, togglePosition: ToggleBoth, msgIndex: 4 }
        -- Types 35 and 23, seen on the device and not modelled.
        , { msgType: MsgSongSelect, channel: 1, data1: 1, data2: 76, data3: 0, data4: 0
          , action: ActionPress, togglePosition: ToggleBoth, msgIndex: 5 }
        , { msgType: MsgTogglePreset, channel: 1, data1: 1, data2: 0, data3: 0, data4: 0
          , action: ActionPress, togglePosition: ToggleBoth, msgIndex: 6 }
        -- An empty slot on a channel other than 1: must not round-trip through
        -- `Silent`, which would rewrite the channel.
        , { msgType: MsgEmpty, channel: 4, data1: 0, data2: 0, data3: 0, data4: 0
          , action: ActionNone, togglePosition: ToggleOff, msgIndex: 7 }
        -- A CC carrying a data3 we cannot explain. Reading it as an ordinary CC
        -- would discard that byte on the way back out.
        , { msgType: MsgCC, channel: 3, data1: 105, data2: 127, data3: 9, data4: 0
          , action: ActionPress, togglePosition: ToggleBoth, msgIndex: 8 }
        ]

  assert "every wire shape survives a trip through the model unchanged"
    (Array.all
      (\m -> Model.toWire m.action m.togglePosition m.msgIndex (Model.fromWire m) == m)
      wireSample)

  assert "the shapes we understand are parsed, not left raw"
    (map (Model.isRaw <<< Model.fromWire) wireSample
      == [ false, false, false, false, true, true, true, true, true ])

  assert "a census counts both halves"
    (Model.census (map Model.fromWire wireSample) == { modelled: 4, raw: 5 })

  -- Names are the other silent failure: `SysEx.shortNameTLV` truncates at 8 and
  -- `longNameTLV` at 24 without complaint, so an over-long label reaches the
  -- device meaning something else. Refusing is the whole difference.
  assert "a name that fits is a name"
    (map Model.unShortName (Model.shortName "Loop 1") == Just "Loop 1")

  assert "a name that does not fit is refused, not shortened"
    (isNothing (Model.shortName "Quantise!"))

  assert "shortening is something you have to ask for"
    (Model.unShortName (Model.clipShortName "Quantise!") == "Quantise")

  assert "long names hold 24 and refuse 25"
    (isJust (Model.longName "Loop 1, hold to set up  ")
      && isNothing (Model.longName "Loop 1, hold to set up   "))

  -- 24, from the editor's write frame — not the 16 that the longest name on
  -- this board happens to be. A bound inferred from the largest value you have
  -- seen is a bound that refuses what the device accepts.
  assert "bank names hold the 24 the write frame carries"
    (isJust (Model.bankName "123456789012345678901234")
      && isNothing (Model.bankName "1234567890123456789012345"))

  -- Positions are bounded by the hardware, so an out-of-range one should not be
  -- constructible rather than being caught at the encoder.
  assert "bank numbers stop at the end of the device"
    (isJust (Model.bankNumber 29) && isNothing (Model.bankNumber 30)
      && isNothing (Model.bankNumber (-1)))

  assert "switch indices stop at twelve"
    (isJust (Model.switchIndex 11) && isNothing (Model.switchIndex 12))

  assert "slot indices stop at the sixteen the device keeps"
    (isJust (Model.slotIndex 15) && isNothing (Model.slotIndex 16))

  assert "and the sizes agree with the device we measured"
    (Model.bankCount == 30 && Model.switchesPerBank == 12
      && Model.slotsPerSwitch == 16)

  log ""
  log "Looper bank family (Data.Looper.Banks)..."

  -- The whole claim of the switch namespace is that a press says which bank it
  -- came from. That is one round trip, and it either holds for every switch of
  -- every bank or the claim is not worth making.
  let everySwitch =
        do slot <- LB.allSlots
           i <- Array.range 0 (ControlBank.switchCount - 1)
           pure { slot, i }

  assert "every switch's CC decodes back to itself, for every gesture"
    (Array.all
      (\s -> Array.all
        (\g -> LB.decodeSwitch LB.switchChannel (LB.switchCC s.slot s.i) (LB.gestureValue g)
                 == Just (LB.switchGesture s.slot s.i g))
        LB.allGestures)
      everySwitch)

  assert "no two switches in the family share a CC"
    (let ccs = map (\s -> LB.switchCC s.slot s.i) everySwitch
     in Array.length (Array.nub ccs) == Array.length ccs)

  -- The four CCs at the top of each block exist only to keep the arithmetic
  -- readable. Accepting one would mean the decoder inventing a switch the
  -- device has no way to press.
  assert "the gap above each block is not a switch"
    (Array.all
      (\slot -> Array.all
        (\off -> LB.decodeSwitch LB.switchChannel (LB.switchCC slot 0 + off) 127 == Nothing)
        [ 12, 13, 14, 15 ])
      LB.allSlots)

  assert "CCs below the first block are not switches"
    (Array.all (\cc -> LB.decodeSwitch LB.switchChannel cc 127 == Nothing)
      (Array.range 0 15))

  -- Itajara's own pedal CCs run 1-83 on channel 13, and board recall is on
  -- channel 1. Both overlap this CC range numerically, so the only thing
  -- keeping them apart is the channel.
  assert "the switch channel is nobody else's"
    (LB.switchChannel /= Looper.itajaraChannel
      && LB.switchChannel /= Board.boardRecallChannel)

  -- Asked of the registry rather than of a comment. This namespace first
  -- shipped on channel 16 because `Data.Looper` said 9, 13 and 16 were free;
  -- 13 had since been taken by Itajara, and 16 is LoopyPro on the device. A
  -- comment cannot go stale in a way anything notices. This can.
  assert "and no pedal in the registry answers on it"
    (Array.null (pedalsOnChannel LB.switchChannel liveEngine))

  -- And an honest note about how far that goes. The registry holds *our*
  -- pedals, so it would not have caught the original collision either:
  -- LoopyPro is not a pedal in this app, it is a destination the MC6 routes to,
  -- and only the device's own channel table knows that. Asserting it here would
  -- be asserting a fact nothing in this process holds.
  assert "though the registry does not know what else is on the wire"
    (Array.null (pedalsOnChannel 16 liveEngine))

  assert "a switch CC on another channel is refused"
    (LB.decodeSwitch Looper.itajaraChannel (LB.switchCC LB.LoopBank 0) 127 == Nothing)

  let family = LB.banks { base: 22, boardBank: 1 }
      familySwitches = do
        cb <- family
        Array.mapWithIndex (\i sw -> { cb, i, sw }) cb.switches
      -- Every switch this app can put on the device, diagnostics included. The
      -- byte and width checks below belong to all of them: the probe bank is
      -- uploaded to real hardware by a real button, and it broke twice by being
      -- treated as somehow less real than the rest.
      everyGeneratedSwitch =
        (LB.banks { base: 22, boardBank: 0 } <> [ Diagnostics.gestureProbeBank 20 0 ])
          >>= _.switches

  assert "one bank per slot, on consecutive numbers from the base"
    (map _.mc6BankNumber family
      == Array.range 22 (22 + Array.length LB.allSlots - 1))

  assert "each bank has its full twelve switches"
    (Array.all (\cb -> Array.length cb.switches == ControlBank.switchCount) family)

  -- Names are truncated in silence by `shortNameTLV`/`longNameTLV`, so an
  -- over-long label does not fail, it just arrives on the device meaning
  -- something slightly different from what this table says.
  assert "no label is longer than the device shows"
    (Array.all (\e -> String.length e.sw.label <= 8) familySwitches)

  -- **Capture and Save left the MC6 entirely**, which is the deduplication made
  -- checkable for the two it is most visible on. Both are encoders on the
  -- Twister's first and third pages; keeping them here as well was two places
  -- to learn and one of them slower. If either comes back to a footswitch this
  -- goes red, which is the point — it should be a decision and not a drift.
  assert "capture and saving are the Twister's, not the board's"
    (Array.all
      (\e -> e.sw.label /= "Capture" && e.sw.label /= "Save")
      familySwitches)

  assert "no long name is longer than the device shows"
    (Array.all (\e -> String.length e.sw.longName <= 24) familySwitches)

  -- `sysexPresetData` pads with `Array.take 16` and the device says nothing
  -- about the overflow, which is the same silent-truncation failure again.
  assert "every switch fits the sixteen-message budget"
    (Array.all (\e -> Array.length e.sw.messages <= Board.messageLimit) familySwitches)

  assert "a blank switch carries no messages"
    (Array.all (\e -> e.sw.label /= "" || Array.null e.sw.messages) familySwitches)

  -- Nothing latches on the device: state lives in the app, which is the only
  -- part of this that can see the engine.
  assert "nothing uses the device's own toggle"
    (Array.all (\e -> not e.sw.toToggle) familySwitches)

  -- Every jump this family emits must land inside it or on the board bank. A
  -- jump to an unwritten bank is a foot that goes somewhere and does not come
  -- back.
  assert "every bank jump lands somewhere we wrote"
    (Array.all
      (\m -> m.msgType /= MsgBankJump
               || Array.elem m.data1
                    (Array.range 22 (22 + Array.length LB.allSlots - 1) <> [ 1 ]))
      (Array.concatMap (\e -> e.sw.messages) familySwitches))

  let loopBankSwitches =
        Array.filter (\e -> e.cb.mc6BankNumber == 22 && e.i < LB.loopSwitches) familySwitches

  -- A jump on press emits everything after it from the bank you have already
  -- arrived at. That is how a press on one bank and its release on another came
  -- to be seen, and a hold nobody made came to be fired. The gesture is one
  -- message now, so the ordering is belt as well as braces — kept because the
  -- reason it was needed has not stopped being true of the device.
  assert "and so does every other press-side navigating switch in the family"
    (let
       cfg = LB.banks { base: 22, boardBank: 0 }
       switchA = do
         cb <- Array.find (\b -> b.name == "Loop Cfg") cfg
         Array.index cb.switches 0
       acts = maybe [] (map _.action <<< _.messages) switchA
     -- Quantise carries one meaning: report at press-down, move on the release.
     in acts == [ ActionPress, ActionRelease, ActionDoubleTapRelease ])

  -- **The rule, stated once over the whole family — and it is about the press
  -- specifically.** On the release the two coexist happily and always have: the
  -- way out of a sub-bank carries both and works, because by then the switch is
  -- finished with. On the *press* the board moves while the message list is
  -- still being read, and what comes after it is lost. That cost a release the
  -- first time and the CC itself the second.
  assert "no switch puts a CC and a bank jump on the same press"
    (Array.all
      (\sw ->
        let on k = map (mc6ActionToInt <<< _.action)
                     (Array.filter (\m -> m.msgType == k) sw.messages)
            press = mc6ActionToInt ActionPress
        in not (Array.elem press (on MsgCC) && Array.elem press (on MsgBankJump)))
      everyGeneratedSwitch)

  -- One press, at press-down, that both says which loop and opens its page.
  -- There is no moment in between where the app and the board could disagree
  -- about whose page this is.
  -- A loop switch is a name, not a door. It was a door until 2026-08-30, when
  -- what it opened turned out to be the toolbar's six over again.
  assert "no loop switch sends the board anywhere"
    (Array.length loopBankSwitches == LB.loopSwitches
      && Array.all
        (\e -> not (Array.any (\m -> m.msgType == MsgBankJump) e.sw.messages))
        loopBankSwitches)

  -- **`ActionPress` if and only if the switch carries exactly one gesture.**
  --
  -- The device fires Press at press-down unconditionally, so a switch with a
  -- second meaning would run its tap and then run the hold on top of it. That
  -- is Morningstar's own advice to program the release wherever there is also a
  -- long press, and it is the whole reason the release side exists here. The
  -- converse matters just as much: a switch with nothing to decide has nothing
  -- to wait for, and putting it on the release would cost a double-tap window
  -- for no reason at all.
  let placedSwitches = do
        slot <- LB.allSlots
        cb <- Array.filter
          (\b -> b.mc6BankNumber == 22 + LB.slotIndex slot)
          (LB.banks { base: 22, boardBank: 1 })
        Array.mapWithIndex (\i sw -> { slot, i, sw }) cb.switches
      live = Array.filter (\e -> not (Array.null e.sw.messages)) placedSwitches

  assert "a switch is on the press exactly when it carries one meaning"
    (Array.length live > 0
      && Array.all
        (\e -> Array.any (\m -> m.action == ActionPress) e.sw.messages
                 == maybe false LB.soleGesture (LB.dutiesAt e.slot e.i))
        live)

  -- And the app has to agree, because it is what decides whether the daemon is
  -- told the command is late. A press-down report is not late.
  assert "and the app reads that off the same table"
    (Array.all
      (\e -> LB.firesAtPressDown e.slot e.i LB.Tap
               == Array.any (\m -> m.action == ActionPress) e.sw.messages)
      live
      -- **A loop switch waits now**, since it gained a double: the device has
      -- to see whether a second press is coming before it can say which you
      -- meant. That is a real cost and it was spent deliberately — it is
      -- Record that has to answer when a foot lands, and Record is one gesture
      -- of its own on the toolbar's `I`.
      && not (LB.firesAtPressDown LB.LoopBank 0 LB.Tap)
      && LB.firesAtPressDown LB.LoopBank 8 LB.Tap
      -- **And Arm waits too**, since `G` gained the hold onto the Grab bank.
      -- The same cost, spent on the one duty in the toolbar that can afford
      -- it: Arm listens for your note, so when the foot landed is not what
      -- starts the recording. That sentence is why the hold went on `G` and
      -- not on `I`.
      && not (LB.firesAtPressDown LB.LoopBank 6 LB.Tap)
      && LB.firesAtPressDown LB.SpeedBank 0 LB.Tap
      -- Only a gesture the switch actually carries can fire at press-down, and
      -- a hold never does: it is a hold.
      && not (LB.firesAtPressDown LB.LoopBank 0 LB.Hold)
      -- The set switch carries three, so it waits.
      && not (LB.firesAtPressDown LB.ConfigBank 9 LB.Tap))

  -- One CC per gesture, all on the switch's own number: the number says where
  -- the press came from and the value says which gesture it was.
  -- Its own number for both gestures, and the value says which — the number
  -- says where the press came from, the value says what kind it was. That is
  -- what lets one CC per switch carry a tap and a double without a second
  -- block of CCs for the doubles.
  assert "and reports both gestures on its own CC, on the release side"
    (Array.all
      (\e -> map (\m -> Tuple m.data1 (Tuple m.data2 m.action))
               (Array.filter (\m -> m.msgType == MsgCC) e.sw.messages)
               == [ Tuple (LB.switchCC LB.LoopBank e.i)
                      (Tuple (LB.gestureValue LB.Tap) ActionRelease)
                  , Tuple (LB.switchCC LB.LoopBank e.i)
                      (Tuple (LB.gestureValue LB.Double) ActionDoubleTapRelease)
                  ])
      loopBankSwitches)

  -- The engine has six loops; the bank offers six places to put a foot. These
  -- are two copies of one number, and the daemon cannot be asked from here.
  assert "the loop bank offers as many loops as the engine has"
    (LB.loopSwitches == 6)

  -- **Seven bits, or the frame stops being a frame.**
  --
  -- A SysEx message is bytes below 0x80; anything with the high bit set is a
  -- status byte and ends the message where it stands. So a CC number of 130 in
  -- a preset frame does not produce a wrong CC, it produces a *truncated
  -- upload* — and the switches after it never arrive, silently, looking for all
  -- the world like a device that has stopped acking. That is exactly what the
  -- gesture probe did on its first outing.
  --
  -- Every value that reaches a frame is checked here rather than at the one
  -- call site that got it wrong.
  assert "no generated message carries a byte that would end a SysEx frame"
    (let
       everyMessage = everyGeneratedSwitch >>= _.messages
       inSeven n = n >= 0 && n <= 127
     in Array.all
          (\m -> inSeven m.data1 && inSeven m.data2 && inSeven m.data3
                   && inSeven m.data4 && inSeven m.channel)
          everyMessage)

  -- **And neither does a name.** The same failure, one field over, and it cost
  -- the same twenty minutes: an em dash in a probe switch's long name is a
  -- character above 127, so the frame carrying it truncates exactly as an
  -- out-of-range CC does — the write stalls, the device stops acking, and
  -- nothing says why. The message-byte check above cannot see it, because the
  -- character never becomes a message.
  --
  -- Typographic punctuation is right everywhere else in this codebase, which is
  -- precisely why it reaches for an em dash without being asked. The device
  -- takes ASCII.
  assert "and no label or long name carries a character the device cannot send"
    (Array.all
      (\sw -> Array.all (\c -> c >= 32 && c <= 126)
        (map Enum.fromEnum
          (StringCU.toCharArray (sw.label <> sw.longName))))
      everyGeneratedSwitch)

  -- Checked over every generated bank rather than over the looper family alone.
  -- The probe bank was outside the length tests, so "Press 60 Rel 61 Dbl 62
  -- Long 63" — thirty characters into a twenty-four character field — has been
  -- silently truncated on the device since the day it was written.
  assert "and every generated name fits the field the device gives it"
    (Array.all
      (\sw -> String.length sw.label <= 8 && String.length sw.longName <= 24)
      everyGeneratedSwitch)

  -- Switches 9-11 are a second FS3X that may not be plugged in. A way out that
  -- lands there is a bank you can walk into and not leave, and you would find
  -- that out with a foot rather than a compiler.
  --
  -- Any action counts. This used to require `ActionPress` and so failed the
  -- moment tap jumps moved to the release, which was the test noticing a change
  -- rather than a fault — the claim is that a way out is *reachable*, and a hold
  -- is as much a way out as a tap.
  -- **The way out is J's hold, on every bank in the family.**
  --
  -- This asked for a jump on a switch at index eight or below — the unit's own
  -- six plus the first expander — so that a board with one FS3X could still
  -- leave a bank. That is no longer true and is no longer meant to be: the
  -- toolbar spans G to L and assumes both expanders, which is what the layout
  -- was reorganised around once it was clear the sloped switches are the ones
  -- worth having. What is checked instead is that every bank has exactly one
  -- way out and it is in the same place, which is the property a foot actually
  -- depends on.
  assert "every bank's way out is J's hold, in the same place on all of them"
    (Array.all
      (\cb ->
        let outs = Array.filter
              (\e -> Array.any (\m -> m.msgType == MsgBankJump) e.sw.messages)
              (Array.filter (\e -> e.cb.mc6BankNumber == cb.mc6BankNumber) familySwitches)
        -- `elem`, not equality: a bank may also carry jumps *inward* — the
        -- config bank's four lead to Quantise, Speed, Modes and Pan. Those are
        -- ways in and this is about the way out, which is one place on all of
        -- them.
        in Array.elem 9 (map _.i outs))
      family)

  -- **A bank is physical storage on a pedal you are standing on.** Two things
  -- claiming one does not fail, it uploads: the second write lands on top of
  -- the first and the device holds whichever went last. The default control
  -- bank and the probe bank both claimed 20 and nothing anywhere compared the
  -- two lists (found 2026-08-23).
  --
  -- Checked against the app's OWN defaults rather than a fixture, because a
  -- fixture would have been written to agree with itself.
  let
    defaultNumbers =
      { board: 1
      , probe: 10
      , looperTransport: 9
      , loopMachineBase: 2
      , diagnostics: 11
      -- From the registry, not a guess. Thirteen pedals at eight to a bank is
      -- two banks — the number this table originally assumed was one.
      , diagnosticsCount: Diagnostics.bypassBankCount reg
      }
    defaultClaims = Reserved.allClaims defaultNumbers [ ControlBank.exampleControlBank ]
    defaultCollisions = Reserved.collisions defaultClaims
  assert
    ( "no MC6 bank is claimed twice"
        <> maybe "" (\d -> "\n       " <> d) (Reserved.describeCollisions defaultCollisions)
    )
    (Array.null defaultCollisions)

  -- The check itself, put in front of a collision it is known to have. A green
  -- check that has never been seen red is a decoration.
  assert "the bank-collision check detects a collision when there is one"
    (Array.length
      (Reserved.collisions
        (Reserved.allClaims defaultNumbers
          [ ControlBank.exampleControlBank { id = "clash", mc6BankNumber = 2 } ]))
      == 1)

  -- The line through the table: machinery below 15, pedal pages at 15 and
  -- above. A convention nothing checks is a convention that lasts until the
  -- next time somebody needs a bank in a hurry.
  let defaultMisplaced = Reserved.misplaced defaultClaims
  assert
    ( "every bank is on its own side of the machinery/pedal line"
        <> maybe "" (\d -> "\n       " <> d) (Reserved.describeMisplaced defaultMisplaced)
    )
    (Array.null defaultMisplaced)

  assert "the line is checked in both directions"
    (Array.length
      (Reserved.misplaced
        (Reserved.allClaims (defaultNumbers { probe = 25 })
          [ ControlBank.exampleControlBank { mc6BankNumber = 3 } ]))
      == 2)

  -- The exemption itself, exercised against a SYNTHETIC external claim rather
  -- than the live list — which is now empty, so testing it directly would pass
  -- while checking nothing at all.
  assert "an external bank is exempt from the machinery/pedal line"
    (Array.null
      (Reserved.misplaced [ { bank: 3, claimant: Reserved.External "somebody else's" } ]))

  -- The whole-map sweep. `clear` is a list of banks about to be ERASED on
  -- hardware, so it is checked here rather than discovered by pressing the
  -- button — a set difference that is only ever exercised by clicking is a set
  -- difference nobody has checked.
  let
    ownedNums =
      map _.mc6BankNumber
        ( LB.banks { base: 2, boardBank: 1 }
            <> [ Looper.looperBank 9 1 ]
            <> [ Diagnostics.gestureProbeBank 10 1 ]
            <> Diagnostics.bypassBanks 11 1 reg
            <> [ ControlBank.exampleControlBank ]
        )
    plan = Reserved.sweep defaultNumbers ownedNums

  -- Against `Survey.bankCount`, NOT `Reserved.mc6BankCount`. The first is
  -- documented as coming from the device's own backup file, where `bankArray`
  -- runs 0 to 29; the second is the constant the sweep uses. Checking the sweep
  -- against its own constant is what let a 1-to-30 range pass — the test and
  -- the bug were written together and agreed with each other, while the device
  -- has no bank 30 and its bank 0 was never being cleared.
  assert "the sweep is in WIRE numbering, 0-based, like the rest of the app"
    (Array.head plan.clear == Just 0 || Array.elem 0 (plan.write <> plan.untouched))
  assert "the sweep never mentions a bank the device does not have"
    (Array.all (\b -> b >= 0 && b < Survey.bankCount)
      (plan.write <> plan.clear <> plan.untouched))
  assert "the sweep's three sets cover every bank exactly once"
    ( Array.sort (plan.write <> plan.clear <> plan.untouched)
        == Array.range 0 (Survey.bankCount - 1)
    )

  -- The board mirror is the only exemption left. Ableton Controls was the
  -- other, at bank 29 — and the device turned out to hold it at 19, so the
  -- exemption was guarding an empty bank while the real one sat in the
  -- clearable range, spared only because that write happened to be refused.
  assert "the sweep never clears the board mirror"
    (not (Array.elem 1 plan.clear))
  assert "the board mirror is the only bank left alone"
    (plan.untouched == [ 1 ])

  -- Everything generated is written, and nothing generated is cleared. The
  -- failure this rules out is the sweep erasing a bank a moment after writing it.
  assert "no bank is both written and cleared"
    (Array.null (Array.intersect plan.write plan.clear))
  assert "every generated bank is in the write set"
    (Array.all (\n -> Array.elem n plan.write) ownedNums)

  -- A cleared bank must READ as agreeing. The device says "EMPTY" where this
  -- app authors "", so the raw comparison called every correctly-blanked bank a
  -- disagreement — with the whole map surveyed, that was almost every card on
  -- screen, and it is indistinguishable at a glance from a write that failed.
  let blank12 = ControlBank.blankBank 7
      deviceEmpty = Array.replicate 12 "EMPTY"
      surveyOf authored observed =
        Survey.survey reg Board.boardRecallChannel authored [] []
          (Map.singleton 7 "") (Map.singleton 7 observed)
  assert "a bank we blanked agrees with a device reporting EMPTY"
    ( case Array.find (\c -> c.bankNumber == 7) (surveyOf [ blank12 ] deviceEmpty) of
        Just c -> c.agrees == Just true
        Nothing -> false
    )
  -- And it must still be able to say no, or the fix is just a blindfold.
  assert "a bank we blanked DISAGREES with a device reporting real switches"
    ( case Array.find (\c -> c.bankNumber == 7)
             (surveyOf [ blank12 ] (Array.replicate 12 "Ht Loop")) of
        Just c -> c.agrees == Just false
        Nothing -> false
    )

  -- **A cleared bank must not be a trap.** The whole-map sweep blanks most of
  -- the device, and a blank bank with no bank-jump on it is one you can walk
  -- into with a foot and not walk out of. Globals are what put the way home on
  -- every page, and the sweep wrote its banks without them — every bank it
  -- touched lost switch G while reading as a successful write (2026-08-23).
  let backGlobal =
        { id: "global-G"
        , slot: 6
        , label: "< Back"
        , longName: "Back to board bank"
        , toToggle: false
        , messages: [ MC6Msg.bankJumpMessage 1 ActionPress ]
        }
      clearedWithGlobals = Global.applyGlobals [ backGlobal ] (ControlBank.blankBank 13)
  assert "a cleared bank still carries the globals, so it has a way out"
    (Array.any (\sw -> Array.any (\m -> m.msgType == MsgBankJump) sw.messages)
      clearedWithGlobals.switches)

  -- And the survey must agree with a device holding exactly that. This is the
  -- pairing that failed: written without globals, checked with them, so the two
  -- differed at one switch and every card went red.
  assert "a cleared bank agrees with a device holding the global at its slot"
    ( case Array.find (\c -> c.bankNumber == 7)
             (surveyOf [ Global.applyGlobals [ backGlobal ] (ControlBank.blankBank 7) ]
                (Array.mapWithIndex (\i _ -> if i == 6 then "< Back" else "EMPTY")
                   (Array.replicate 12 unit))) of
        Just c -> c.agrees == Just true
        Nothing -> false
    )

  -- ── Stamping: making a failed write visible ───────────────────────────────
  --
  -- The whole point is discrimination, so every assertion below is paired with
  -- one that must FAIL to hold. A mark that agrees with everything would be a
  -- blindfold with a serial number on it.
  log ""
  log "  MC6 sweep marks:"

  let marked7 = Stamp.mark 7 (ControlBank.blankBank 3)
      labels cb = map _.label cb.switches

  assert "a cleared bank's blank switches carry bank, slot and run"
    (labels marked7 == map (\l -> "03" <> l <> " r7")
                          [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L" ])
  assert "a cleared bank's name says which sweep cleared it"
    (marked7.name == "CLEAR 03 r7")

  -- The mark has to fit the field the device gives it, or the MC6 truncates it
  -- silently and `03A r100` becomes a claim about a bank that is not there.
  assert "no mark can overflow the MC6's eight-character short name"
    ( Array.all (\n -> Array.all (\l -> String.length l <= 8) (labels (Stamp.mark n (ControlBank.blankBank 29))))
        [ 0, 1, 7, 99, 999, 1000, 1007 ]
    )
  -- And the counter wraps rather than growing, so run 1000 is honestly run 0
  -- instead of dishonestly `03A r100`.
  assert "the run counter wraps at four digits instead of being cut short"
    (labels (Stamp.mark 1007 (ControlBank.blankBank 3)) == labels (Stamp.mark 7 (ControlBank.blankBank 3)))

  -- A switch that sends something is doing a job. Writing a diagnostic mark on
  -- it would put a lie on the pedal: the player reads `03A r7` and the switch
  -- freezes MOOD.
  let silentButBusy =
        { label: "", longName: "", toToggle: false
        , messages: [ MC6Msg.ccMessage 3 105 127 ActionPress ] }
  assert "a switch with messages but no label is left unmarked"
    ((Stamp.slotMark 7 3 0 silentButBusy).label == "")
  assert "a switch with a label of its own is left unmarked"
    ((Stamp.slotMark 7 3 0 { label: "MD Freez", longName: "", toToggle: false, messages: [] }).label == "MD Freez")

  -- Marking happens after the globals, so the way home keeps its own name.
  let markedWithGlobal = Stamp.mark 7 (Global.applyGlobals [ backGlobal ] (ControlBank.blankBank 3))
  assert "a global's slot keeps its label rather than taking a mark"
    (Array.index (labels markedWithGlobal) 6 == Just "< Back")
  assert "and the slots either side of it still take marks"
    (Array.index (labels markedWithGlobal) 5 == Just "03F r7")

  -- A generated bank keeps its name and gains the run, within the device's
  -- twenty-four characters.
  assert "a generated bank's name keeps its meaning and gains the run"
    (Stamp.bankMark 7 3 "Loop Three" == "Loop Three r7")
  assert "a long name is cut to make room rather than losing the run off the end"
    ( let long = Stamp.bankMark 7 3 "A bank name far longer than the device will hold"
      in String.length long <= 24 && String.contains (String.Pattern "r7") long
    )

  -- ── What the marks buy: the survey can now tell runs apart ────────────────
  --
  -- This is the assertion the whole module exists for. Before it, a bank the
  -- sweep failed to write and a bank the sweep wrote correctly last time were
  -- the same screen.
  let surveyMarked authored observed =
        Survey.survey reg Board.boardRecallChannel authored [] []
          (Map.singleton 3 "") (Map.singleton 3 observed)
      agreesAt n surveyed = case Array.find (\c -> c.bankNumber == n) surveyed of
        Just c -> c.agrees
        Nothing -> Nothing

  assert "a bank echoing this run's marks agrees"
    (agreesAt 3 (surveyMarked [ marked7 ] (labels marked7)) == Just true)
  assert "a bank still holding the PREVIOUS run's marks disagrees"
    (agreesAt 3 (surveyMarked [ marked7 ] (labels (Stamp.mark 6 (ControlBank.blankBank 3)))) == Just false)
  -- The factory contents this device keeps handing back for banks 2-6.
  assert "a bank the sweep never reached disagrees"
    (agreesAt 3 (surveyMarked [ marked7 ] (Array.replicate 12 "EMPTY")) == Just false)
  -- A frame that landed one bank over: the MC6 ignores the bank number in an
  -- upload and writes wherever it is standing, so the marks must be able to
  -- say "these switches think they live somewhere else".
  assert "switches marked for another bank disagree with the bank holding them"
    (agreesAt 3 (surveyMarked [ marked7 ] (labels (Stamp.mark 7 (ControlBank.blankBank 4)))) == Just false)

  -- ── Back means back to the block you are in ───────────────────────────────
  --
  -- Coming out of the looper's Speed page you want the Loops grid you came
  -- from, not the instrument's front door. The slot stays the same everywhere,
  -- there is still one definition to edit, and the target comes from the map —
  -- so this is not the per-page override that B7 closed.
  log ""
  log "  MC6 block-scoped way home:"

  let loopHome b = if b > 2 && b < 9 then Just 2 else Nothing
      jumpTargets cb = do
        sw <- cb.switches
        m <- sw.messages
        if m.msgType == MsgBankJump then [ m.data1 ] else []

  assert "a page inside the block goes back to the block's entry"
    (jumpTargets (Global.applyGlobalsTo loopHome [ backGlobal ] (ControlBank.blankBank 5)) == [ 2 ])
  assert "the entry page itself keeps the global's own target"
    (jumpTargets (Global.applyGlobalsTo loopHome [ backGlobal ] (ControlBank.blankBank 2)) == [ 1 ])
  assert "a page outside the block keeps the global's own target"
    (jumpTargets (Global.applyGlobalsTo loopHome [ backGlobal ] (ControlBank.blankBank 20)) == [ 1 ])
  -- The plain form must keep behaving as it always did, since most pages use it.
  assert "applyGlobals is applyGlobalsTo with nothing re-aimed"
    (jumpTargets (Global.applyGlobals [ backGlobal ] (ControlBank.blankBank 5)) == [ 1 ])
  -- Re-aiming must touch the jump and nothing else.
  let ccGlobal = backGlobal { messages = [ MC6Msg.ccMessage 3 105 127 ActionPress ] }
  assert "a global carrying a CC is not re-aimed"
    ( case Array.index (Global.applyGlobalsTo loopHome [ ccGlobal ] (ControlBank.blankBank 5)).switches 6 of
        Just sw -> map _.data1 sw.messages == [ 105 ]
        Nothing -> false
    )

  -- ── Assignments are a source the sweep must compile (B11) ─────────────────
  --
  -- An assignment binds a board to a switch, and until 2026-08-24 the whole-map
  -- write had never heard of them: it wrote a blank over the switch while the
  -- app went on believing a board was there. Silently, because a blank switch
  -- is exactly what a successful clear looks like. It had already happened to
  -- bank 21 switch A.
  log ""
  log "  MC6 assignments in the sweep:"

  let smallBoard =
        { id: "board-small", name: "Wash", description: "", notes: ""
        , pedals: Map.singleton (PedalId "mood") { presetId: Nothing, engage: EngageOff }
        , created: "", modified: ""
        }
      -- Every pedal off at once already fits (asserted above), so the board
      -- that does NOT fit is every pedal recalling a preset AND being switched
      -- off: a program change plus a bypass, two messages apiece.
      pidText (PedalId t) = t
      pedalPresets =
        map (\p -> { id: "p-" <> pidText p.meta.id, pedalId: p.meta.id
                  , name: "", description: "", notes: ""
                  , values: Map.empty, info: Map.empty
                  , savedSlot: makeProgramNumber 1
                  , created: "", modified: "" }) Registry.pedals
      hugeBoard = allOffBoard
        { id = "board-huge", name = "Everything"
        , pedals = Map.fromFoldable
            (map (\p -> Tuple p.meta.id
                    { presetId: Just ("p-" <> pidText p.meta.id), engage: EngageOff })
              Registry.pedals)
        }
      assignEnvOf as bs =
        { registry: reg, presets: pedalPresets, boards: bs, controlBank: Nothing, assignments: as }
      onSlot n i bid = { bankNumber: n, switchIndex: i, boardPresetId: bid }
      blank21 = ControlBank.blankBank 21

  assert "a board assigned to a switch survives the page being blanked"
    ( case Array.index (Assign.applyAssignments
              (assignEnvOf [ onSlot 21 0 "board-small" ] [ smallBoard ]) blank21).switches 0 of
        Just sw -> sw.label == "Wash" && not (Array.null sw.messages)
        Nothing -> false
    )
  -- The failure this replaces: the switch came back blank and nothing said so.
  assert "and the switch would otherwise have been blank"
    (map _.label (Array.index blank21.switches 0) == Just "")
  assert "a switch with no assignment is untouched"
    ( map _.label (Array.index (Assign.applyAssignments
        (assignEnvOf [ onSlot 21 0 "board-small" ] [ smallBoard ]) blank21).switches 1) == Just "" )
  assert "an assignment for another bank does not reach this page"
    ( map _.label (Array.index (Assign.applyAssignments
        (assignEnvOf [ onSlot 20 0 "board-small" ] [ smallBoard ]) blank21).switches 0) == Just "" )
  -- A stale row in the store must not become lost work on the device.
  assert "an assignment naming a board that no longer exists changes nothing"
    ( map _.label (Array.index (Assign.applyAssignments
        (assignEnvOf [ onSlot 21 0 "board-gone" ] [ smallBoard ]) blank21).switches 0) == Just "" )

  -- Refuse rather than truncate: `sysexPresetData` pads with `Array.take 16`,
  -- so an over-budget board programs cleanly and arrives missing its last
  -- messages — a switch that silently does most of what you asked.
  assert "an over-budget board is reported rather than written short"
    ( map _.bank (Assign.overBudget
        (assignEnvOf [ onSlot 21 0 "board-huge" ] [ hugeBoard ]) [ blank21 ]) == [ 21 ] )
  assert "and the switch is left alone rather than truncated"
    ( map _.label (Array.index (Assign.applyAssignments
        (assignEnvOf [ onSlot 21 0 "board-huge" ] [ hugeBoard ]) blank21).switches 0) == Just "" )
  assert "a board that fits is not reported"
    ( Array.null (Assign.overBudget
        (assignEnvOf [ onSlot 21 0 "board-small" ] [ smallBoard ]) [ blank21 ]) )

  -- ── The globals warning must not fire on the globals ──────────────────────
  --
  -- Adopting a bank overwrites every slot a global owns, so real work there is
  -- lost for good and deserves a warning. But almost every bank carries
  -- `< Back` on G because THIS APP put it there, and warning about that accused
  -- the surface of destroying work it had authored moments earlier — on every
  -- cleared bank in the sweep. A warning that fires on the ordinary case is one
  -- people learn to click past, which costs the rare case it exists for.
  log ""
  log "  MC6 globals displacement:"

  let preset i msgs =
        { presetNum: i, shortName: "", toggleName: "", longName: ""
        , toToggle: false, toggleGroup: 0, messages: msgs }
      nativeBank ps = { bankNumber: 3, bankName: "", bankClearToggle: false, presets: ps }
      backMsgs = [ MC6Msg.bankJumpMessage 1 ActionPress ]
      otherWork = [ MC6Msg.ccMessage 3 105 127 ActionPress ]

  assert "a slot holding the global itself is not displaced work"
    (Array.null (Global.displacedByGlobals [ backGlobal ]
      (nativeBank (Array.mapWithIndex (\i _ -> preset i (if i == 6 then backMsgs else []))
        (Array.replicate 12 unit)))))
  -- The warning must still bite, or removing it was just deleting a safeguard.
  assert "a slot doing something ELSE under a global is displaced work"
    (Global.displacedByGlobals [ backGlobal ]
      (nativeBank (Array.mapWithIndex (\i _ -> preset i (if i == 6 then otherWork else []))
        (Array.replicate 12 unit))) == [ 6 ])
  assert "an empty slot under a global loses nothing"
    (Array.null (Global.displacedByGlobals [ backGlobal ]
      (nativeBank (Array.mapWithIndex (\i _ -> preset i []) (Array.replicate 12 unit)))))
  -- Work on a slot no global owns is not this warning's business.
  assert "work outside every global's slot is not displaced"
    (Array.null (Global.displacedByGlobals [ backGlobal ]
      (nativeBank (Array.mapWithIndex (\i _ -> preset i (if i == 2 then otherWork else []))
        (Array.replicate 12 unit)))))

  -- Taking a copy off the device must not take its sentinel with it. `EMPTY` is
  -- the device's word for an unset switch; storing it literally made pages that
  -- author twelve switches actually called "EMPTY", which then looked occupied
  -- to everything downstream — the sweep's marks skip a switch that has a label.
  assert "the device's EMPTY is not a switch name"
    (Survey.blankIfEmpty "EMPTY" == "" && Survey.blankIfEmpty " empty " == "")
  -- And a real name is kept EXACTLY, case and all. The comparison helper folds
  -- case on purpose, which makes it the wrong tool for anything that keeps its
  -- answer: copying a switch through it would store `Ht Loop` as `HT LOOP` and
  -- rename the page on its way in.
  assert "a real name is copied in unchanged"
    (Survey.blankIfEmpty "Ht Loop" == "Ht Loop")
  assert "while the comparison helper still folds case, which is why they differ"
    (Survey.emptiness "Ht Loop" == "HT LOOP")

  -- ── Two pages on one bank ─────────────────────────────────────────────────
  --
  -- The write takes both and the device keeps the last; the survey looked the
  -- bank up and took the first. So the page that was sent and the page that was
  -- checked were different pages, and five cards reported "device disagrees"
  -- about writes that were in fact perfect. A whole day went looking for a
  -- fault in the MC6 (2026-08-24).
  log ""
  log "  MC6 double-claimed banks:"

  let loopsPage = (ControlBank.blankBank 2) { id = "itajara-loops", name = "Loops" }
      copiedPage = (ControlBank.blankBank 2) { id = "bank-2", name = "Patch Two" }

  assert "one page per bank is not a collision"
    (Array.null (ControlBank.doubleClaims [ loopsPage, ControlBank.blankBank 3 ]))
  assert "two pages on one bank are reported, with both names"
    (ControlBank.doubleClaims [ loopsPage, copiedPage ]
       == [ { bank: 2, pages: [ "itajara-loops", "bank-2" ] } ])
  -- The set-based guard could not see this: as a set of bank numbers the two
  -- lists are identical, which is why multiplicity had to be the thing checked.
  assert "the collision is invisible to a set of bank numbers"
    (Array.nub (map _.mc6BankNumber [ loopsPage, copiedPage ]) == [ 2 ])

  -- And the survey must decline to answer rather than pick a side. Comparing
  -- against either page is reporting a coin toss as a measurement.
  let twoClaimants =
        Survey.survey reg Board.boardRecallChannel [ loopsPage, copiedPage ] [] []
          (Map.singleton 2 "Patch Two") (Map.singleton 2 (Array.replicate 12 "EMPTY"))
  assert "a bank two pages claim reports no verdict"
    (case Array.find (\c -> c.bankNumber == 2) twoClaimants of
       Just c -> c.agrees == Nothing
       Nothing -> false)
  assert "and names both claimants instead of accusing the device"
    (case Array.find (\c -> c.bankNumber == 2) twoClaimants of
       Just c -> c.claimants == [ "itajara-loops", "bank-2" ]
       Nothing -> false)
  -- With one claimant it must still answer, or this is a way to make every
  -- disagreement disappear.
  assert "a bank one page claims still gets a verdict"
    (case Array.find (\c -> c.bankNumber == 2)
            (Survey.survey reg Board.boardRecallChannel [ loopsPage ] [] []
               (Map.singleton 2 "Loops") (Map.singleton 2 (Array.replicate 12 "EMPTY"))) of
       Just c -> c.agrees == Just true
       Nothing -> false)

  log ""
  log "The Twister as a looper surface (Data.Looper.Twister)..."

  let eight = map idle (Array.range 0 (LB.nLoops - 1))
      rig8 = rigOf eight

  -- **One grid on three surfaces.** Four across and two down, in plain order,
  -- because that is the Twister's top half and the shape an MC8 would fill.
  assert "the loops are one grid, four across and two down, in order"
    (LB.loopRows == [ [ 0, 1, 2, 3 ], [ 4, 5, 6, 7 ] ]
      && Array.length (Array.nub (join LB.loopRows)) == LB.nLoops)

  -- **The pedal fits into the grid; the grid no longer fits the pedal.** A
  -- switch selects the loop in its own physical place, and the MC6 numbers its
  -- switches from the bottom — so A, the near-left switch, is loop 5.
  assert "each MC6 switch selects the loop that sits where it sits"
    (LB.switchLoops == [ 4, 5, 6, 0, 1, 2 ]
      && LB.loopAtSwitch 0 == Just 4
      && LB.loopAtSwitch 3 == Just 0
      && LB.switchForLoop 0 == Just 3
      && LB.switchForLoop 4 == Just 0)

  -- The two the pedal cannot reach are the fourth column, not the last two
  -- indices — which is what an MC8 would fill.
  assert "loops 4 and 8 are the column the MC6 does not have"
    (LB.switchForLoop 3 == Nothing
      && LB.switchForLoop 7 == Nothing
      && Array.length (Array.filter (\l -> isNothing (LB.switchForLoop l))
           (Array.range 0 (LB.nLoops - 1))) == 2)

  -- **The letter beside a loop is the letter under your foot.** These were the
  -- same number until the surfaces were harmonised; printing it from the loop's
  -- index would send the foot to the wrong corner and would still be right
  -- about loop 5, which is the kind of half-right that survives testing.
  assert "a loop prints the letter of the switch that selects it, or its number"
    (LB.faceLoopKey (LB.face (Just LB.LoopBank)) 0 == "D"
      && LB.faceLoopKey (LB.face (Just LB.LoopBank)) 4 == "A"
      && LB.faceLoopKey (LB.face (Just LB.LoopBank)) 2 == "F"
      && LB.faceLoopKey (LB.face (Just LB.LoopBank)) 3 == "4"
      && LB.faceLoopKey (LB.face (Just LB.LoopBank)) 7 == "8")

  -- And the bank the device is programmed with says the same thing, since both
  -- come from `switchLoops`.
  assert "the loop bank's switches are compiled in grid order"
    (map (\i -> LB.dutyAt LB.LoopBank i) (Array.range 0 5)
      == map (Just <<< LB.SelectLoop) [ 4, 5, 6, 0, 1, 2 ])

  -- The MC6 reaches six of the eight and says so. These were one number until
  -- the Twister arrived, and the whole point of separating them is that they
  -- are now allowed to differ.
  assert "the pedal reaches six of the eight, and the two facts are separate"
    (LB.loopSwitches == 6 && LB.nLoops == 8 && LB.loopSwitches < LB.nLoops)

  assert "each of the eight loop encoders selects its own loop"
    (Array.all
      (\i -> LoopTw.pressedAt { bank: 0, index: i }
          == Just (Tuple (LB.OnLoop i) (LB.SelectLoop i)))
      (Array.range 0 (LB.nLoops - 1)))

  -- **The subject bug, as a class rather than an instance.** Every per-loop
  -- verb from the old CC table went out unprefixed and landed on whatever the
  -- daemon had selected; it was found on Save Take and fixed there alone. With
  -- the subject an argument, a knob on loop 6 addresses loop 6 while the focus
  -- is elsewhere, and there is no way to express the broken form.
  assert "a turn addresses the knob's own loop, whatever is in focus"
    (Machine.perform (rig8 { focus = 0 }) (LB.OnLoop 5) (LB.Place 100)
      == [ Machine.Command "5pan100" ])

  -- **Acting on a loop takes it in hand; brushing one does not.**
  --
  -- The Set page presses stop/go on the same eight encoders that select on the
  -- Loops page, so pressing Loop 8 there left the focus wherever it was and the
  -- next Clear went somewhere else — with the log naming loop 8 eight lines
  -- above the ack. The press now says which loop you mean.
  --
  -- The second half is the one that matters more: a *turn* must not. This
  -- hardware moves an encoder when you press it, so a nudge of loop 5's pan
  -- that stole the focus would be worse than the bug being fixed.
  assert "a press on a loop takes it in hand, and a turn on one does not"
    (Machine.performPress (rig8 { focus = 0 }) (LB.OnLoop 7) LB.Transport
       == [ Machine.Focus 7, Machine.Unavailable "loop 8 has nothing to play" ]
      && Machine.performPress (rig8 { focus = 0 }) (LB.OnLoop 5) (LB.Place 100)
        == [ Machine.Focus 5, Machine.Command "5pan100" ]
      -- The turn path never reaches `performPress`, and this is the property
      -- that keeps it worth not reaching.
      && Machine.perform (rig8 { focus = 0 }) (LB.OnLoop 5) (LB.Place 100)
        == [ Machine.Command "5pan100" ])

  -- Selecting already focused, so it must not be focused twice — and every
  -- duty that reaches `SelectLoop` by recursion is covered by the same check.
  assert "selecting a loop focuses it exactly once"
    (Machine.performPress (rig8 { focus = 0 }) (LB.OnLoop 3) (LB.SelectLoop 3)
      == [ Machine.Focus 3, Machine.Handled "loop 4" ])

  -- Nothing whose subject is the focused loop can move the focus, or every
  -- knob on the Shape page would be a selector.
  assert "a duty about whatever is focused never moves the focus"
    (Machine.performPress (rig8 { focus = 5 }) LB.Focused (LB.Place 100)
      == [ Machine.Command "5pan100" ])

  assert "and a duty with no loop of its own goes to the focused one"
    (Machine.perform (rig8 { focus = 5 }) LB.Focused (LB.Place 100)
      == [ Machine.Command "5pan100" ])

  -- A ladder is a rendering of a parameter for a surface that can only press.
  -- If these two ever produced different commands, a footswitch and a knob
  -- would mean different things by the same word.
  assert "a ladder step and a knob reach the socket by the same line"
    (let r = rigOf [ (idle 0) { chance = 1.0 } ]
     in Array.filter isCommand (Machine.perform r LB.Focused LB.StepChance)
          == Machine.perform r LB.Focused (LB.Chance (LB.stepChance 1.0)))

  assert "and the same is true of fade and decay"
    (let r = rigOf [ (idle 0) { fadeMs = 25.0, decayDb = -3.0 } ]
     in Array.filter isCommand (Machine.perform r LB.Focused LB.StepFade)
          == Machine.perform r LB.Focused (LB.Fade (LB.stepFade 25.0))
       && Array.filter isCommand (Machine.perform r LB.Focused LB.StepDecay)
            == Machine.perform r LB.Focused (LB.Decay (LB.stepDecay (-3.0))))

  -- **Every position of every knob must be a value the daemon accepts.** It
  -- refuses out-of-range rather than clamping — which is right, and means a
  -- knob whose ends fall outside the range has two dead positions that report
  -- a refusal instead of moving. Checked at all 128 positions because the ends
  -- are exactly where an off-by-one lands.
  -- **Bipolar, so every position is a magnitude in range or exactly zero.**
  -- Zero is the centre band and is the one value the daemon does not take yet;
  -- it is asserted as *reachable* rather than as accepted, because the point of
  -- the dead band is that stopped is a place the hand can find.
  assert "every position of the speed knob is a speed the daemon accepts, or stopped"
    (Array.all
      (\v -> case LoopTw.fromKnob LoopTw.PRate v of
          LB.Rate r -> r == 0.0 || (r >= 0.125 && r <= 4.0) || (r <= -0.125 && r >= -4.0)
          _ -> false)
      (Array.range 0 127))

  -- Backwards is the left half and forwards the right, which is the whole
  -- reason Reverse stopped needing a cell of its own.
  assert "the speed knob runs backwards below the centre and forwards above it"
    (Array.all
      (\v -> case LoopTw.fromKnob LoopTw.PRate v of
          LB.Rate r -> r < 0.0
          _ -> false)
      (Array.range 0 60)
      && Array.all
        (\v -> case LoopTw.fromKnob LoopTw.PRate v of
            LB.Rate r -> r > 0.0
            _ -> false)
        (Array.range 68 127))

  assert "and of the decay, fade, chance and pan knobs"
    (Array.all
      (\v ->
        (case LoopTw.fromKnob LoopTw.PDecay v of
           LB.Decay db -> db <= 0.0 && db >= -60.0
           _ -> false)
        && (case LoopTw.fromKnob LoopTw.PFade v of
              LB.Fade ms -> ms >= 0.0 && ms <= 500.0
              _ -> false)
        && (case LoopTw.fromKnob LoopTw.PChance v of
              LB.Chance p -> p >= 0.0 && p <= 1.0
              _ -> false)
        && (case LoopTw.fromKnob LoopTw.PPlace v of
              LB.Place n -> n >= 0 && n <= 127
              _ -> false))
      (Array.range 0 127))

  -- **Stopped at the centre detent**, full speed either end, and pan's centre
  -- where it has always been. Unity is not a position on this knob any more —
  -- it is the press — which is the trade that bought the direction.
  assert "the speed knob stops in the middle and reaches full speed both ways"
    (LoopTw.fromKnob LoopTw.PRate 64 == LB.Rate 0.0
      && LoopTw.fromKnob LoopTw.PRate 0 == LB.Rate (-4.0)
      && LoopTw.fromKnob LoopTw.PRate 127 == LB.Rate 4.0
      && LoopTw.fromKnob LoopTw.PPlace 64 == LB.Place 64)

  -- The press is the only way back to unity, so it had better be unity.
  assert "pressing the speed knob asks for unity, forwards"
    (LoopTw.pressedAt { bank: 2, index: 0 } == Just (Tuple LB.Focused (LB.Rate 1.0)))

  -- **The ring is told, never remembered.** This is the property that separates
  -- an encoder from the MC6's scroll counters: what the device shows is the
  -- engine's own value, so whoever moved it — the console, a footswitch, some
  -- other client — the knob agrees within a frame.
  -- The speed ring lands mid-step rather than at the very end of the travel,
  -- because the knob is quantised now and a step has a middle. The property
  -- that matters is the round trip: whatever the engine says, the position the
  -- ring is put at asks for that same speed back.
  assert "the ring shows the engine's value, at both ends and the middle"
    (LoopTw.toKnob LoopTw.PRate ((idle 0) { speed = 0.0 }) == 64
      && LoopTw.fromKnob LoopTw.PRate (LoopTw.toKnob LoopTw.PRate ((idle 0) { speed = 4.0 }))
           == LB.Rate 4.0
      && LoopTw.toKnob LoopTw.PPlace ((idle 0) { pan = 100 }) == 100
      && LoopTw.toKnob LoopTw.PChance ((idle 0) { chance = 1.0 }) == 127)

  -- **Every step is an octave or a fifth from the one beside it**, which is the
  -- whole reason the knob steps at all: a continuous speed is a continuous
  -- transposition, so every position between the useful ones is a loop out of
  -- tune with the rest of the rig. Checked as a ratio rather than against a
  -- written-out list, so a rung added to the ladder has to earn its place.
  assert "the speed ladder moves in fifths and fourths and nothing else"
    (Array.all
      (\i -> case Array.index LoopTw.rateLadder i, Array.index LoopTw.rateLadder (i + 1) of
          Just a, Just b ->
            let r = b / a
            in intAbs (round (r * 1000.0) - 1500) <= 4
                 || intAbs (round (r * 1000.0) - 1333) <= 4
          _, _ -> false)
      (Array.range 0 (Array.length LoopTw.rateLadder - 2)))

  -- Unity is on the ladder, and so are the two the MC6's speed bank sends, so
  -- the knob and the switches cannot land a loop in different keys.
  assert "the ladder holds unity and the values the speed bank steps to"
    (Array.all (\r -> Array.elem r LoopTw.rateLadder) [ 0.25, 0.5, 1.0, 1.5, 2.0 ])

  -- Every position is a rung, and every rung is reachable. A step nothing can
  -- select is a step that is not there.
  assert "every rung of the ladder can be reached by turning"
    (Array.all
      (\r -> Array.elem (LB.Rate r) (map (LoopTw.fromKnob LoopTw.PRate) (Array.range 0 127)))
      LoopTw.rateSteps)

  -- **The bug this composition was written to end.** `speed` is a magnitude and
  -- the direction is `reverse`, so a ring read off `speed` alone drew a loop
  -- running backwards at half speed exactly like one running forwards at half
  -- speed. It did that for as long as the knob existed and nothing could see
  -- it, because the knob could only ever ask for one sign.
  assert "the ring tells the two directions apart"
    (LoopTw.toKnob LoopTw.PRate ((idle 0) { speed = 2.0, reverse = false })
       /= LoopTw.toKnob LoopTw.PRate ((idle 0) { speed = 2.0, reverse = true })
      && LoopTw.toKnob LoopTw.PRate ((idle 0) { speed = 2.0, reverse = true }) < 64
      && LoopTw.toKnob LoopTw.PRate ((idle 0) { speed = 2.0, reverse = false }) > 64)

  -- A knob you cannot put back is a knob you will not turn.
  assert "every knob has a press that puts it home"
    (Array.all
      (\i -> let c = LoopTw.controlAt { bank: 2, index: i }
             in case c.turn of
                  Nothing -> true
                  Just _ -> isJust c.press)
      (Array.range 0 15))

  -- **Four pages now, and the fifth is nothing.** The two spare blocks were
  -- being kept for the per-layer surface; the four-page cut spent them on the
  -- set, on shape and on set-up, and page five is off the end of the pager's
  -- travel rather than reserved.
  assert "there is no fifth page"
    (Array.all
      (\i -> LoopTw.pressedAt { bank: 4, index: i } == Nothing
          && LoopTw.turnedAt { bank: 4, index: i } 64 == Nothing)
      (Array.range 0 15))

  -- Every page has its pager in the same corner and nothing else claims one.
  assert "each of the four pages is described for the card"
    (Array.length LoopTw.pages == LoopTw.pages'
      && Array.all (\pg -> Array.length pg.cells == 16) LoopTw.pages)

  -- **The round trip is the reason the taper is allowed to bend.**
  --
  -- The level and decay knobs were straight lines from full to the floor, on
  -- the stated ground that a two-segment law is two chances to get the inverse
  -- wrong. That was the wrong trade: a straight line put the whole useful range
  -- in the top few degrees, and "it drops VERY quickly as you turn" is what
  -- that feels like. So they bend, and this is the answer to the objection —
  -- every position of both knobs goes out as a value and comes back as a
  -- position within one step of where it started.
  assert "every position of the level knob survives the round trip"
    (Array.all
      (\v -> case LoopTw.fromKnob LoopTw.PLevel v of
          LB.Level db -> intAbs (LoopTw.toKnob LoopTw.PLevel ((idle 0) { volDb = db }) - v) <= 1
          _ -> false)
      (Array.range 0 127))

  assert "and every position of the decay knob"
    (Array.all
      (\v -> case LoopTw.fromKnob LoopTw.PDecay v of
          LB.Decay db -> intAbs (LoopTw.toKnob LoopTw.PDecay ((idle 0) { decayDb = db }) - v) <= 1
          _ -> false)
      (Array.range 0 127))

  -- The bend is where a fader bends: half the travel on the first 12 dB.
  -- **Half a turn is half the amplitude.** The knee moved from the middle of
  -- the travel to a quarter of it, twice reported by ear: linear in decibels
  -- dropped too fast, and 12 dB at half travel still did. Mixing needs more of
  -- the knob than fading does, because fading is a gesture and mixing is a
  -- decision.
  assert "the level fader spends its top three quarters on the first 9 dB"
    (LoopTw.fromKnob LoopTw.PLevel 127 == LB.Level 0.0
      && LoopTw.fromKnob LoopTw.PLevel 64 == LB.Level (-6.0)
      && LoopTw.fromKnob LoopTw.PLevel 32 == LB.Level (-9.0)
      && LoopTw.fromKnob LoopTw.PLevel 0 == LB.Level (-60.0))

  -- **Decay counts down from hold**, the same way round as the level, and stops
  -- at the ladder's own floor rather than the daemon's. Every value the MC6's
  -- ladder can step to is reachable on the knob, which is the test that the two
  -- surfaces are describing one parameter.
  assert "decay holds at the top and reaches the ladder's floor at the bottom"
    (LoopTw.fromKnob LoopTw.PDecay 127 == LB.Decay 0.0
      && LoopTw.fromKnob LoopTw.PDecay 64 == LB.Decay (-3.0)
      && LoopTw.fromKnob LoopTw.PDecay 0 == LB.Decay (-12.0))

  assert "and a fresh loop's decay knob sits at the top, not the bottom"
    (LoopTw.toKnob LoopTw.PDecay (idle 0) == 127)

  -- **Centre sticks**, because the Midifighter's own detent is a setting in its
  -- configuration utility and this app keeps no device configuration. Pan you
  -- meant to centre is centred rather than one step off it.
  assert "pan and speed snap to the middle, and nothing else does"
    (LoopTw.fromKnob LoopTw.PPlace 63 == LB.Place 64
      && LoopTw.fromKnob LoopTw.PPlace 66 == LB.Place 64
      && LoopTw.fromKnob LoopTw.PPlace 60 == LB.Place 60
      && LoopTw.fromKnob LoopTw.PRate 63 == LB.Rate 0.0
      && LoopTw.fromKnob LoopTw.PChance 63 /= LoopTw.fromKnob LoopTw.PChance 64)

  -- **The ring is the value, and on this device that is not a display
  -- choice.** The ring and the encoder's own position are one number, so a
  -- playhead drawn round a loop's ring was a playhead written into the value
  -- its next touch would send — and a loop selected for recording went silent,
  -- because the press nudged it and it said whatever the playhead had left
  -- there. Pinned so nothing puts a second thing on a ring that carries a
  -- value.
  assert "a loop encoder's ring reads its level back, and follows the engine"
    (map _.ring (Array.take 1 (LoopTw.leds (rigOf [ (idle 0) { volDb = 0.0, phase = 0.7 } ]) 0))
      == [ 127 ]
      && map _.ring (Array.take 1 (LoopTw.leds (rigOf [ (idle 0) { volDb = -60.0, phase = 0.7 } ]) 0))
        == [ 0 ])

  -- **The page is the app's, not the device's.** Reading an encoder against
  -- the bank in its own CC left the only way between pages on a side button
  -- nobody here can program or verify — and stuck on page 2 with no way home is
  -- what that cost. So the same physical encoder means different things on
  -- different pages, and which page is a fact this app holds.
  assert "the same encoder is a different control on each page"
    (LoopTw.pressedAt { bank: 0, index: 0 } == Just (Tuple (LB.OnLoop 0) (LB.SelectLoop 0))
      && LoopTw.pressedAt { bank: 1, index: 0 } == Just (Tuple (LB.OnLoop 0) LB.Transport)
      && LoopTw.pressedAt { bank: 2, index: 0 } == Just (Tuple LB.Focused (LB.Rate 1.0))
      && LoopTw.pressedAt { bank: 3, index: 0 } == Just (Tuple LB.Focused LB.OneShot))

  -- **The set is the transpose of Shape, and its loops sit where the Loops
  -- page's do.** Same eight positions, same eight loops, different verb — which
  -- is the whole reason the spatial map only has to be learned once.
  assert "the set names its loops in the same eight positions as the loops page"
    (Array.all
      (\i -> (LoopTw.controlAt { bank: 0, index: i }).subject
               == (LoopTw.controlAt { bank: 1, index: i }).subject)
      (Array.range 0 7)
      && LoopTw.turnedAt { bank: 1, index: 3 } 100 == Just (Tuple (LB.OnLoop 3) (LB.Place 100)))

  -- Both pages light sixteen encoders and neither knows which CC block they
  -- land in — that is the caller's problem, because only the caller holds both
  -- the page the app is on and the block the device is showing.
  assert "a page lights sixteen encoders and names no bank"
    (Array.length (LoopTw.leds (rigOf eight) 0) == 16
      && Array.length (LoopTw.leds (rigOf eight) 1) == 16
      && map _.index (LoopTw.leds (rigOf eight) 0) == Array.range 0 15)

  -- **Level, not chance, under a loop's press** — and the reason is the
  -- hardware: you cannot press one of these encoders without rotating it a
  -- little on the way down, so whatever sits under the press is what a press
  -- will nudge. A nudged level you hear at once and correct without thinking; a
  -- nudged chance you do not hear until the pass it eats.
  assert "a loop encoder's turn sets that loop's level"
    (LoopTw.turnedAt { bank: 0, index: 5 } 127
      == Just (Tuple (LB.OnLoop 5) (LB.Level 0.0))
      && LoopTw.turnedAt { bank: 0, index: 5 } 0
        == Just (Tuple (LB.OnLoop 5) (LB.Level (-60.0))))

  assert "and the level knob spans silence to unity, both reachable"
    (Array.all
      (\v -> case LoopTw.fromKnob LoopTw.PLevel v of
          LB.Level db -> db <= 0.0 && db >= -60.0
          _ -> false)
      (Array.range 0 127)
      && LoopTw.fromKnob LoopTw.PLevel 127 == LB.Level 0.0
      && LoopTw.fromKnob LoopTw.PLevel 0 == LB.Level (-60.0))

  -- -30 dB sits below the knee, so it is well down the travel and nowhere near
  -- the middle. That is the whole point of the bend: the middle belongs to
  -- -6 dB, where mixing happens.
  assert "and the ring reads the level back off the snapshot"
    (LoopTw.toKnob LoopTw.PLevel ((idle 0) { volDb = 0.0 }) == 127
      && LoopTw.toKnob LoopTw.PLevel ((idle 0) { volDb = -60.0 }) == 0
      && LoopTw.toKnob LoopTw.PLevel ((idle 0) { volDb = -6.0 }) == 64
      && LoopTw.toKnob LoopTw.PLevel ((idle 0) { volDb = -9.0 }) == 32
      && LoopTw.toKnob LoopTw.PLevel ((idle 0) { volDb = -30.0 }) < 32)

  -- **A proven inverse, not a second spelling.** Reading a wire string back is
  -- exactly what `Data.Looper.Verb` exists to prevent, so `addressed` earns its
  -- place the way `fromKnob`/`toKnob` do: walked over every loop and a spread
  -- of verbs, and a drift between the two is a failing test rather than a wrong
  -- label on a log line.
  assert "the loop a command is addressed to can be read back off it"
    (Array.all
      (\i -> Array.all
        (\v -> LoopVerb.addressed (LoopVerb.at i v)
                 == Just { loop: i, verb: LoopVerb.render v })
        [ LoopVerb.Record, LoopVerb.Bars 4, LoopVerb.Spread 3, LoopVerb.Place' 2
        , LoopVerb.Rate (-0.5), LoopVerb.Level (-12.0), LoopVerb.Revox true ])
      (Array.range 0 7))

  -- A rig-wide verb carries no loop, and must not be read as carrying loop
  -- zero — which is what a prefix parser that defaulted would do.
  assert "a command with no loop on it reports none"
    (LoopVerb.addressed (LoopVerb.render (LoopVerb.LaunchQ 4)) == Nothing
      && LoopVerb.addressed (LoopVerb.render (LoopVerb.Click true)) == Nothing)

  -- The log names loops the way every other surface does. This said `→ 1len4`
  -- for a four-bar Loop 2, which is right and reads as loop one.
  assert "the log names the loop in the numbering the screen uses"
    (Machine.describe (Machine.Command (LoopVerb.at 1 (LoopVerb.Bars 4)))
       == "loop 2 · len4")

  assert "the level verb spells as the daemon expects"
    (LoopVerb.render (LoopVerb.Level 0.0) == "vol0.0"
      && LoopVerb.render (LoopVerb.Level (-12.5)) == "vol-12.5")

  -- **A flag on a single control has to flip.** `Grid 1` always set the grid
  -- *on*, which is fine on the MC6 bank where Free sits beside it with a switch
  -- of its own, and wrong the moment one encoder has to do both — pressing it
  -- twice did nothing the second time. Found by printing the layout below, not
  -- by reading the table.
  assert "the grid encoder turns the grid off as well as on"
    (Machine.perform (rigOf [ (idle 0) { quant = false } ]) LB.Focused LB.GridToggle
      == [ Machine.Command "0g1" ]
      && Machine.perform (rigOf [ (idle 0) { quant = true } ]) LB.Focused LB.GridToggle
        == [ Machine.Command "0g0" ])

  -- And the MC6's two switches are renderings of the same value, so they cannot
  -- come to mean something different from the encoder.
  assert "the bank's Grid and Free delegate to the same value duty"
    (let r = rigOf [ idle 0 ]
     in Array.filter isCommand (Machine.perform r LB.Focused (LB.Grid 4))
          == Machine.perform r LB.Focused (LB.OnGrid true)
       && Machine.perform r LB.Focused LB.Free
            == Machine.perform r LB.Focused (LB.OnGrid false))

  -- **The card cannot drift, because it is generated.** These pin that: a cell
  -- with a control behind it must describe itself, and a cell with nothing
  -- behind it must be blank rather than half-filled.
  -- The pager counts as a control while having no duty at all — it asks nothing
  -- of the looper, which is the whole reason it is a flag rather than a `Duty`.
  --
  -- **A turn counts too**, since the launch quantise: that encoder has a knob
  -- and no press, which this used to read as an empty cell wearing a name. A
  -- control is anything a hand can do something to, and what it must describe
  -- is whichever of the two it has.
  assert "every control on the card describes itself, and every blank stays blank"
    (Array.all
      (\pg -> Array.all
        (\c -> let k = { bank: pg.bank, index: c.index }
                   ctl = LoopTw.controlAt k
                   has = LoopTw.pressedAt k /= Nothing || ctl.turn /= Nothing || ctl.pager
               in if has then c.name /= "" && (isJust c.press || isJust c.turn)
                  else c.name == "")
        pg.cells)
      LoopTw.pages)

  -- **The whole chain for the Revox encoder**, decoder through to the wire,
  -- because the two ends were each right on their own while the thing did not
  -- work: threading a tape and pressing for the mode has to end in `rvx1` on
  -- the focused loop and nothing else.
  assert "pressing the Revox encoder sends rvx1 for the focused loop"
    (let tape = (idle 0) { layers = 1, loopSecs = 8.0, state = "playing" }
         rig = (rigOf [ idle 0, tape ]) { focus = 1 }
     in case LoopTw.pressedAt { bank: 3, index: 8 } of
          Just (Tuple subj duty) -> Machine.perform rig subj duty == [ Machine.Command "1rvx1" ]
          Nothing -> false)

  assert "and again turns it off rather than on"
    (let tape = (idle 0) { layers = 1, loopSecs = 8.0, revox = true }
         rig = rigOf [ tape ]
     in case LoopTw.pressedAt { bank: 3, index: 8 } of
          Just (Tuple subj duty) -> Machine.perform rig subj duty == [ Machine.Command "0rvx0" ]
          Nothing -> false)

  -- Turning the same encoder threads, and does not disturb the mode.
  assert "turning the Revox encoder threads the focused loop"
    (let rig = (rigOf [ idle 0, idle 1 ]) { focus = 1 }
     in case LoopTw.turnedAt { bank: 3, index: 8 } 34 of
          Just (Tuple subj duty) -> Machine.perform rig subj duty == [ Machine.Command "1blank16.0" ]
          Nothing -> false)

  -- **Tape loses the top before it loses the level.** Losing only the level is
  -- what makes a feedback loop sound digital — the last repeat is the first
  -- one, quieter, with every edge still on it.
  assert "the tape tone is hertz on the wire, and 20 kHz means off"
    (LoopVerb.render (LoopVerb.Tone 6500.0) == "tone6500.0"
      && Machine.perform (rigOf [ idle 0 ]) LB.Focused (LB.Tone 6500.0)
        == [ Machine.Command "0tone6500.0" ]
      && LB.dutyName (LB.Tone 20000.0) == "Every pass as bright")

  -- **A tape is threaded, not recorded.** Every other way a loop gets a length
  -- is by recording one; this is the only way to have a length and nothing in
  -- it, which is what Revox needs to start from.
  assert "the tape knob threads whole seconds and reads the loop's length back"
    (LoopTw.fromKnob LoopTw.PTape 127 == LB.Blank 60.0
      && LoopTw.fromKnob LoopTw.PTape 64 == LB.Blank 30.0
      && LoopTw.toKnob LoopTw.PTape ((idle 0) { loopSecs = 30.0 }) == 64)

  -- Zero is the absence of a command, not a command for no tape. Sending
  -- `blank0` would have the daemon refuse a length nobody asked for.
  assert "the bottom of the tape knob asks for nothing"
    (LoopTw.fromKnob LoopTw.PTape 0 == LB.Blank 0.0
      && Machine.perform (rigOf [ idle 0 ]) LB.Focused (LB.Blank 0.0) == []
      && Machine.perform (rigOf [ idle 0 ]) LB.Focused (LB.Blank 8.0)
        == [ Machine.Command "0blank8.0" ])

  -- One control, because they are one idea: a tape is a loop of a chosen
  -- length that you play onto, and choosing the length is how you start.
  assert "the Revox encoder carries the mode and the tape together"
    (let c = LoopTw.controlAt { bank: 3, index: 8 }
     in c.press == Just LB.RevoxToggle && c.turn == Just LoopTw.PTape)

  -- **Arm took the cell Revox left**, so the write-head row is the MC6's own
  -- loop page switch for switch: record, overdub, stop/go, arm. It was two
  -- page turns and a mode flip before this, on the one gesture where a few
  -- hundred milliseconds is a take.
  assert "arm is one press on the loops page, beside the rest of the write head"
    (LoopTw.pressedAt { bank: 0, index: 11 } == Just (Tuple LB.Focused LB.ArmLoop))

  -- **The tape's two parameters had no hardware control at all** until Set up:
  -- sliders on a web page, for the one mode in the rig that has no undo.
  assert "the tape's feedback and tone are knobs now, and read the engine back"
    (LoopTw.fromKnob LoopTw.PFeedback 127 == LB.Feedback 0.0
      && LoopTw.fromKnob LoopTw.PFeedback 0 == LB.Feedback (-24.0)
      && LoopTw.fromKnob LoopTw.PTone 127 == LB.Tone 20000.0
      && LoopTw.toKnob LoopTw.PFeedback ((idle 0) { fbDb = 0.0 }) == 127
      && LoopTw.toKnob LoopTw.PTone ((idle 0) { toneHz = 20000.0 }) == 127)

  -- **Multiply could not be asked for by any hand.** It was on the CC table and
  -- so reachable from a web button, and on no MC6 bank and no encoder — a verb
  -- the vocabulary had and no surface could send.
  -- On Shape rather than Set up: it is not a length *setting*, it is a length
  -- *performance* — press to open, play across the cycles, press to close, with
  -- the write head open throughout. Its declarative twin is the bars knob on
  -- the Loops page.
  assert "multiply has a control on the shape page, at the head of its shape row"
    (LoopTw.pressedAt { bank: 2, index: 8 } == Just (Tuple LB.Focused LB.MultiplyLoop))

  -- **Length and how often came apart.** `SpreadLoop` set the period *and* grew
  -- the loop by the same factor, so a four-bar loop whose phrase sounds every
  -- bar was not reachable at all. Three knobs, three numbers, one each — and
  -- now on two pages, because they turned out to be about different moments:
  -- the length is what a take needs before it starts, the other two are what
  -- you do to it afterwards.
  assert "bars, every and slot are three separate knobs"
    (LoopTw.turnedAt { bank: 0, index: 9 } 127 == Just (Tuple LB.Focused (LB.SetBars 32))
      && LoopTw.turnedAt { bank: 2, index: 5 } 0 == Just (Tuple LB.Focused (LB.Every 1))
      && LoopTw.turnedAt { bank: 2, index: 6 } 0 == Just (Tuple LB.Focused (LB.PlaceAt 1)))

  -- **`slot` is meaningless without `every`**, so they have to be reachable
  -- without a page turn between them. This pins the adjacency rather than the
  -- indices, which is the fact that matters: whichever cells they end up in,
  -- they end up next to each other.
  assert "every and slot are neighbours"
    (case (LoopTw.controlAt { bank: 2, index: 5 }).turn
        , (LoopTw.controlAt { bank: 2, index: 6 }).turn of
       Just LoopTw.PEvery, Just LoopTw.POn -> true
       _, _ -> false)

  -- **The grid and the length are one encoder on the Loops page**, which is
  -- what let Overdub's cell go: press to wait for the bar, turn for how many
  -- bars, and a first take needs no other page.
  assert "the grid encoder carries the quantise and the bar count together"
    (let c = LoopTw.controlAt { bank: 0, index: 9 }
     in c.press == Just LB.GridToggle && c.turn == Just LoopTw.PBars
          && c.ring == LoopTw.Value LoopTw.PBars)

  -- **The input is a per-loop choice, and it goes out addressed.**
  --
  -- `ClaimPast` is why it is per loop rather than a rig-wide switch: the ring
  -- exists so you need not decide in advance, and a global selector would put
  -- that decision straight back in front of you.
  assert "the input knob picks a source for the loop in hand"
    (LoopTw.fromKnob LoopTw.PSource 0 == LB.SetSource 1
      && LoopTw.fromKnob LoopTw.PSource 127 == LB.SetSource LoopTw.maxSources
      && Machine.perform (rig8 { focus = 2 }) LB.Focused (LB.SetSource 3)
        == [ Machine.Command "2src3" ])

  -- Every source is reachable and reads itself back, which is what lets the
  -- ring be told rather than remembered.
  assert "every source can be reached and survives the round trip"
    (Array.all
      (\n -> LoopTw.fromKnob LoopTw.PSource (LoopTw.toKnob LoopTw.PSource ((idle 0) { src = n }))
               == LB.SetSource n)
      (Array.range 1 LoopTw.maxSources))

  -- Set, never flip — the rule every other mode here follows, and the reason
  -- is that a client which flips drifts the first time a command is dropped.
  assert "the mono fold is set from the engine's own word"
    (Machine.perform (rigOf [ (idle 0) { mono = false } ]) LB.Focused LB.MonoToggle
       == [ Machine.Command "0mono1" ]
      && Machine.perform (rigOf [ (idle 0) { mono = true } ]) LB.Focused LB.MonoToggle
        == [ Machine.Command "0mono0" ])

  -- **The tempo comes from a loop and lands on the session.**
  --
  -- Addressed to a loop like everything else — the loop is where the two
  -- numbers are — and the whole chain has to end in `bpm` on the focused one,
  -- because a bare `bpm` would take the tempo from whatever the daemon happens
  -- to have selected, which is the field nothing in this app writes.
  assert "the tempo press asks the focused loop for it, and says which"
    (let full = (idle 0) { layers = 1, loopSecs = 8.0, cycles = 4, state = "playing" }
         rig = (rigOf [ idle 0, full ]) { focus = 1 }
     in case LoopTw.pressedAt { bank: 1, index: 11 } of
          Just (Tuple subj duty) ->
            duty == LB.TakeTempo
              && Machine.perform rig subj duty == [ Machine.Command "1bpm" ]
          Nothing -> false)

  -- **A knob that absorbed another control says so in its name.** The cell read
  -- `bars` and the grid was a line further down under `press`, which reads as
  -- the grid having been deleted rather than moved. Derived from the two halves
  -- and gated on `home`, so a knob whose press is only its way back — speed,
  -- layers, decay — keeps its plain name.
  assert "a knob carrying a mode is named after both, and one that is not is not"
    (named 0 9 == "bars/Grid"
      && named 3 8 == "tape/Revox"
      && named 2 0 == "speed"
      -- **The exception that proves the rule was right.** Every other `knob`
      -- press puts its parameter back where it rests, so the card prints the
      -- parameter alone. This one is an act — the stack has no resting value —
      -- and calling it home hid the most-wanted verb on the surface behind the
      -- word "layers".
      && named 0 13 == "layers/Undo"
      && named 3 11 == "fade")

  -- **Overdub was Record with a refusal bolted on.** `onOverdub` and `onRecord`
  -- send the same `r` in every case that reaches the wire; the only difference
  -- is that Overdub declines an empty loop. Worth a switch on the MC6, where a
  -- foot cannot see what it is writing to. Not worth an encoder here, and this
  -- pins the equivalence rather than the absence — if the two ever stop meaning
  -- the same thing on a loop with material in it, the cell has to come back.
  assert "record on a loop with layers in it is what overdub was"
    (let full = (idle 0) { layers = 2, loopSecs = 4.0, state = "playing" }
         rig = rigOf [ full ]
     in Machine.perform rig LB.Focused LB.RecordLoop
          == Machine.perform rig LB.Focused LB.OverdubLoop
       && Array.all (\i -> (LoopTw.controlAt { bank: 0, index: i }).press
                             /= Just LB.OverdubLoop)
            (Array.range 0 15))

  -- **The erasure is the same corner on both pages a hand learns as one row.**
  -- Clear sat at 13 and Clear All at 14, which is the kind of near-miss that
  -- only shows up when the hand is somewhere else and the eyes are elsewhere
  -- again.
  assert "clear and clear all are the same encoder, furthest from the pager"
    (LoopTw.pressedAt { bank: 0, index: 12 } == Just (Tuple LB.Focused LB.ClearLoop)
      && LoopTw.pressedAt { bank: 1, index: 12 } == Just (Tuple LB.Focused LB.ClearAll))

  -- Every step of a stepped knob is reachable and reads itself back, which is
  -- the property that lets the ring be told rather than remembered.
  assert "every bar count can be reached and survives the round trip"
    (Array.all
      (\n -> LoopTw.fromKnob LoopTw.PBars (LoopTw.toKnob LoopTw.PBars ((idle 0) { cycles = n }))
               == LB.SetBars n)
      (Array.range 1 32))

  -- **A bar is what the metre says**, so the ladder spells one as -1 rather
  -- than as four: four beats is a bar in 4/4 and three quarters of one in 3/4,
  -- and a setting that is right in one time signature and quietly wrong in
  -- every other is worse than no setting.
  assert "the launch ladder carries none, the bar, and beats in between"
    (Array.elem 0 LoopTw.launchLadder
      && Array.elem (-1) LoopTw.launchLadder
      && LoopTw.fromKnob LoopTw.PLaunch 0 == LB.Launch 0
      && LoopTw.fromKnob LoopTw.PLaunch 127 == LB.Launch (-1))

  -- The one knob on the surface that is not about a loop, which is why its ring
  -- is a `RigValue` and is filled in by `leds` rather than by `toKnob`.
  assert "the launch knob is rig-wide and reads the rig, not a loop"
    (let ring q = map _.ring
           (Array.filter (\l -> l.index == 8)
             (LoopTw.leds ((rigOf [ idle 0 ]) { launchQ = q }) 1))
     in ring 0 /= ring (-1) && ring 4 /= ring 0)

  -- **Held is not stopped.** A loop at speed zero is still in the phase-locked
  -- set and is not muted; `Transport` silences one and keeps its place. They
  -- look alike from outside, so the colour says which.
  assert "a loop held at zero has its own colour, and it is not the muted one"
    (let playing = (idle 0) { state = "playing", layers = 1, speed = 1.0 }
         heldOne = playing { speed = 0.0 }
         mutedOne = playing { muted = true }
         hueAt st = map _.hue (Array.take 1 (LoopTw.leds (rigOf [ st ]) 0))
     in hueAt heldOne /= hueAt playing
          && hueAt heldOne /= hueAt mutedOne)

  -- **Revox is a mode you opt into, and it takes undo with it.** Refused by
  -- name rather than silently doing nothing: a knob that stops working is a
  -- broken knob until something says otherwise.
  assert "the layer scrub refuses on a tape, and says why"
    (Machine.perform (rigOf [ (idle 0) { revox = true, layers = 3 } ])
       LB.Focused (LB.Layers 1)
      == [ Machine.Unavailable "loop 1 is a tape — undo went with the layers" ])

  assert "and still scrubs when the loop is layers"
    (Machine.perform (rigOf [ (idle 0) { revox = false, layers = 3 } ])
       LB.Focused (LB.Layers 2)
      == [ Machine.Command "0u", Machine.Handled "loop 1: 2 layers" ])

  -- Set, never flip — the same rule as the click, and for the same reason.
  assert "the Revox toggle sets from what the daemon reported"
    (Machine.perform (rigOf [ (idle 0) { revox = false } ]) LB.Focused LB.RevoxToggle
      == [ Machine.Command "0rvx1" ]
      && Machine.perform (rigOf [ (idle 0) { revox = true } ]) LB.Focused LB.RevoxToggle
        == [ Machine.Command "0rvx0" ])

  -- `rvx`, not `rev`: reverse got there first, and a prefix collision on the
  -- wire is a command that silently means something else.
  assert "Revox and Reverse cannot be confused on the wire"
    (LoopVerb.render (LoopVerb.Revox true) == "rvx1"
      && LoopVerb.render (LoopVerb.Reversed true) == "rev1"
      && LoopVerb.render (LoopVerb.Feedback (-6.0)) == "fb-6.0")

  -- **The pager is the same corner on every page**, because it is one control
  -- rather than one per page, and a hand finds a corner without looking.
  assert "the pager sits bottom-right on every page and nowhere else"
    (Array.all
      (\pg -> (LoopTw.controlAt { bank: pg.bank, index: 15 }).pager
        && Array.all (\i -> not (LoopTw.controlAt { bank: pg.bank, index: i }).pager)
             (Array.range 0 14))
      LoopTw.pages)

  -- **A position with ends, not a step from a moving reference.**
  --
  -- Three designs, and the two dead ones are worth keeping written down because
  -- both looked right on paper:
  --
  -- * position over the whole travel — two pages meant sweeping half the
  --   encoder, and a third page made each band narrower rather than the gesture
  --   smaller;
  -- * a step from a parked position — parked at the page's own end there was no
  --   travel left to turn into, so forward-wrap was unreachable on hardware and
  --   the test asserting it passed by handing `pageTurn` a value of 130, which
  --   nothing can send. Parked in the middle instead, the ring had to be
  --   rewritten after every change, which made one direction cost a full notch
  --   and the other cost a single unit.
  --
  -- The pager reads where it stands, in fixed 32-unit bands, and the app writes
  -- to it only when the app moved the page by itself.
  assert "a quarter turn is one page, and each page is the same angle"
    (LoopTw.pageFor 0 == 0
      && LoopTw.pageFor (LoopTw.pageStep - 1) == 0
      && LoopTw.pageFor LoopTw.pageStep == 1
      && LoopTw.pageStep == 32)

  -- **Clamped, not wrapped.** A knob with a physical end should stop, and it
  -- makes the gesture reversible: turning back exactly as far returns exactly
  -- as many pages, which a wrap does not.
  assert "turning past the last page does nothing"
    (LoopTw.pageFor 127 == LoopTw.pages' - 1
      && LoopTw.pageFor (LoopTw.pages' * LoopTw.pageStep) == LoopTw.pages' - 1
      && LoopTw.pageFor 0 == 0)

  -- Every page has to be reachable inside the encoder's actual travel, or a
  -- page added is a page you cannot turn to.
  assert "every page fits in the travel"
    (LoopTw.pagerRing (LoopTw.pages' - 1) <= 127
      && LoopTw.pagerRing (LoopTw.pages' - 1) == (LoopTw.pages' - 1) * LoopTw.pageStep)

  -- And where the app parks it must read back as the page it parked it for, or
  -- the card and the knob disagree the moment anything else moves the page.
  assert "the app parks the pager where it reads as that page"
    (Array.all (\p -> LoopTw.pageFor (LoopTw.pagerRing p) == p)
       (Array.range 0 (LoopTw.pages' - 1)))

  -- On taking focus everything is dimmed to zero, which is page one — so a
  -- reload leaves the knob and the app agreeing without either being told.
  assert "zero is the first page"
    (LoopTw.pagerRing 0 == 0 && LoopTw.pageFor 0 == 0)

  -- The page is the pager's colour as well as its position, so no two pages may
  -- look the same.
  assert "each page gives the pager its own colour"
    (Array.length (Array.nubEq (map LoopTw.pageTone (Array.range 0 (LoopTw.pages' - 1))))
       == LoopTw.pages')

  -- **Undo and Redo are one axis.** The scrub sends the difference between
  -- where the knob is and what the daemon reports, so it cannot drift — and a
  -- layer removed by a footswitch moves the knob rather than confusing it.
  assert "the layer knob scrubs the undo stack by the difference"
    (Machine.perform (rigOf [ (idle 0) { layers = 4 } ]) LB.Focused (LB.Layers 2)
      == [ Machine.Command "0u", Machine.Command "0u"
         , Machine.Handled "loop 1: 2 layers" ]
      && Machine.perform (rigOf [ (idle 0) { layers = 2 } ]) LB.Focused (LB.Layers 4)
        == [ Machine.Command "0y", Machine.Command "0y"
           , Machine.Handled "loop 1: 4 layers" ]
      && Machine.perform (rigOf [ (idle 0) { layers = 3 } ]) LB.Focused (LB.Layers 3)
        == [])

  -- Eight layers across 128 steps is sixteen steps a layer, and the press guard
  -- only has to cover two — so the nudge is harmless by arithmetic rather than
  -- by luck.
  -- Thirty-two since the ceiling went from eight layers to four, which only
  -- makes the original argument stronger: the device moves an encoder when you
  -- press it, and a step you can cross by pressing is a step that changes a
  -- loop you meant only to take in hand.
  assert "a layer is thirty-two steps wide, well clear of a press nudge"
    (LoopTw.fromKnob LoopTw.PLayers 127 == LB.Layers 4
      && LoopTw.fromKnob LoopTw.PLayers 0 == LB.Layers 0
      && LoopTw.fromKnob LoopTw.PLayers 2 == LB.Layers 0
      && LoopTw.toKnob LoopTw.PLayers ((idle 0) { layers = 2 }) == 64)

  -- The rig's threshold is not a per-loop value and has no knob for that
  -- reason; it still needs a verb, because the page sets it.
  -- **A scene is a pedal page that has been told whose it is.**
  --
  -- The claim the whole widening rests on: reading a pedal's own mapping as a
  -- scene changes nothing about the controls, only binds each to its owner. If
  -- this ever stops holding, the two kinds of page have quietly become two
  -- kinds of thing again and every surface downstream has to start asking.
  -- `handleEncoder` has never read the state it is handed — which is the fact
  -- the extraction rests on — so an empty one is the honest argument here.
  let noValues = { channel: 1, values: Map.empty, info: Map.empty }
  case Registry.findPedal (PedalId "onward") >>= _.twister of
    Nothing -> assert "Onward has a Twister mapping to read as a scene" false
    Just tw -> do
      let sc = Scene.ofPedal (PedalId "onward") "Onward" tw
      assert "reading a pedal page as a scene keeps its shape"
        (Array.length sc.encoders == Array.length tw.encoders
          && Array.length sc.buttons == Array.length tw.buttons
          && sc.hue == tw.hue)

      -- Dark stays dark. A cell with no control must not acquire one by being
      -- bound: `Nothing` is a knob that does nothing, and binding it to a pedal
      -- would make it a knob that does nothing *to that pedal*, which is a
      -- different and wrong statement.
      assert "binding does not light a dark cell"
        (map isJust sc.encoders == map isJust tw.encoders
          && map isJust sc.buttons == map isJust tw.buttons)

      -- One owner, because that is what a pedal's own page means.
      assert "every cell of a pedal's page names that pedal"
        (Scene.pedalsIn sc == [ PedalId "onward" ])

      -- **Same cell, same decision** — and it is the first half that is worth
      -- testing. `handleEncoder` calls `encoderAction` now, so their agreement
      -- is not in doubt; what this pins is that a scene's index reaches the
      -- *same control* the pedal page's does. `ofPedal` maps over two arrays,
      -- and an off-by-one or a reorder in there would be invisible everywhere
      -- else: every knob would still work, and each would work on the wrong
      -- parameter. Every index, so the options branch cannot slip through.
      assert "the bound control decides exactly as the pedal page did"
        (case Registry.findPedal (PedalId "onward") of
           Nothing -> false
           Just def -> Array.all
             (\i ->
               let viaPage = ETw.handleEncoder i 100 def noValues
                   viaCell = Scene.encoderAt sc i >>= \b -> ETw.encoderAction b.control 100
               in map _.cc viaPage == map _.cc viaCell
                    && map _.value viaPage == map _.value viaCell)
             (Array.range 0 15))

  -- **Every borrowing leads somewhere.** A pick that does not resolve becomes a
  -- dark cell rather than an error, which is right at runtime and useless as a
  -- guard — a mistyped pedal id or an index past the end of a page would show
  -- up as a knob that quietly does nothing, on a controller with no labels, in
  -- the middle of a set. So the resolution is checked here instead.
  let lookupTw pid = Registry.findPedal pid >>= _.twister
      ambientScene = Scene.resolve lookupTw Scenes.ambient
  assert "every pick in the live scene resolves"
    (Array.length (Array.catMaybes ambientScene.encoders)
       == Array.length (Array.catMaybes Scenes.ambient.encoders)
      && Array.length (Array.catMaybes ambientScene.buttons)
       == Array.length (Array.catMaybes Scenes.ambient.buttons))

  -- The three the feet are on, and only those. A scene that quietly grew a
  -- fourth pedal would be a page where some knobs act on a pedal the bank's
  -- switches cannot reach.
  -- The group DESIGN-BANKS calls ambient/evolving, and only it. A scene that
  -- quietly grew a fifth pedal would be a page where some knobs act on a pedal
  -- the bank's switches cannot reach.
  assert "the ambient scene touches exactly its group"
    (Array.sort (Scene.pedalsIn ambientScene)
      == Array.sort
           [ PedalId "mood", PedalId "onward"
           , PedalId "lostandfound", PedalId "habit" ])

  -- Column four is the switch column on every pedal page, so it is the switch
  -- column here. The bottom row breaks that on purpose and is checked with it:
  -- three seconds, in the same left-to-right order as the rows above.
  -- Column four is the switch column on every pedal page, so it is the switch
  -- column here: a row per pedal, three knobs and a switch, learned once.
  assert "the scene is four rows of three knobs and a switch"
    (Array.all (\i -> isJust (Scene.buttonAt ambientScene i)) [ 3, 7, 11, 15 ]
      && Array.all (\i -> isNothing (Scene.buttonAt ambientScene i)) [ 0, 1, 2, 4, 8, 12 ]
      && Array.all (\i -> isJust (Scene.encoderAt ambientScene i))
           [ 0, 1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14 ]
      && Array.all (\i -> isNothing (Scene.encoderAt ambientScene i)) [ 3, 7, 11, 15 ])

  -- A scene is CCs to pedals, and Itajara is not a pedal. Reaching the daemon
  -- from here would be a second route beside `Machine.perform`, which is the
  -- thing `Pedals.Itajara.twister = Nothing` exists to prevent.
  assert "no scene can reach the looper"
    (Array.all
      (\d -> not (Array.elem Looper.itajaraId
                    (Scene.pedalsIn (Scene.resolve lookupTw d))))
      Scenes.scenes)

  -- Only a bank that has given its switches to pedals asks for a scene. Every
  -- other bank must leave the Twister alone, or standing on the looper's own
  -- banks would take its four pages away.
  -- Only a page that has given its switches to a group asks for a scene. Every
  -- other page must leave the Twister alone, or standing on the looper's own
  -- banks would take its four pages away.
  -- **The id is the join, so the id is what can drift.** `Scenes` answers to
  -- "control-ambient" and the page declares it; nothing else connects the two.
  -- If they part, the scene simply never appears — a page that looks entirely
  -- right and leaves the Twister on whatever it happened to be showing, which
  -- is the quietest failure this arrangement has available.
  assert "the ambient page and the ambient scene agree on the id"
    (isJust (Scenes.sceneForControlBank ControlBank.ambientControlBank.id))

  -- Control pages live in the pedal half of the table. Below it `Reserved`
  -- refuses them as ControlTooLow, and that refusal only fires on a write —
  -- which is the worst moment to discover a bank number.
  assert "the ambient page sits in the pedal half, beside not on the other one"
    (ControlBank.ambientControlBank.mc6BankNumber >= Reserved.pedalRangeFrom
      && ControlBank.ambientControlBank.mc6BankNumber
           /= ControlBank.exampleControlBank.mc6BankNumber)

  -- Six on the expanders, because three dual-engage pedals at two channels
  -- each is exactly what G-L holds. If a pedal gained or lost a channel this
  -- page would stop being the group's page and nothing else would say so.
  assert "the ambient page fills its expanders"
    (Array.length (Array.filter (\sw -> sw.label /= "")
      (Array.drop 6 ControlBank.ambientControlBank.switches)) == 6)

  -- **The card names a borrowed cell by asking the pedal it was borrowed from.**
  -- Three things have to line up for this to read right — the pick, the pedal's
  -- own Twister mapping, and its CC labels — and they are three separate tables
  -- in three files. If any of them slips, the card shows a blank or the wrong
  -- word on a controller with nothing written on it, which is the failure the
  -- card exists to prevent.
  let testRegistry = CRegistry.mkRegistry Registry.pedals []
        { pedalOutput: { match: "" }, twisterInput: { match: "" }
        , twisterOutput: { match: "" }, mc6Input: { match: "" } }
      ambientCard = Card.cardFor testRegistry (Just ambientScene) Nothing 0
  assert "the card names the ambient scene from the pedals it borrows from"
    (ambientCard.title == "Ambient"
      && map _.name ambientCard.cells ==
           -- "Micro-Looper" and not the engage table's "ML": the card takes
           -- its words from the pedal's own layout, which is what the pedal
           -- prints and what its detail page says. The MC6 switch is labelled
           -- "MD ML" because eight characters is all it has.
           [ "Clock", "Length", "Mix", "Micro-Looper"
           , "Error", "Texture", "Size", "Glitch"
           , "Spill", "Glue", "Blend", "Left"
           , "Modify", "Scan", "Spread", "Mode"
           ])

  -- Four inks, one per row, and a row is one pedal. The colour is the only
  -- thing on the device that says whose knob is under your hand.
  assert "the card gives each row its pedal's ink"
    (let rowInk r = Array.nub
           (Array.mapMaybe _.ink (Array.take 4 (Array.drop (r * 4) ambientCard.cells)))
     in Array.all (\r -> Array.length (rowInk r) == 1) [ 0, 1, 2, 3 ]
          && Array.length (Array.nub (Array.mapMaybe _.ink ambientCard.cells)) == 4)

  -- With no scene and no pedal in focus it is the looper's own page, named
  -- from the same table the print sheet and the modal render.
  assert "with nothing overriding it the card is the looper's page"
    (let c = Card.cardFor testRegistry Nothing Nothing 0
     in c.title == "Loops" && Array.length c.cells == 16
          && map _.name (Array.take 2 c.cells) == [ "Loop 1", "Loop 2" ])

  assert "only the ambient page calls up a scene"
    (isJust (Scenes.sceneForControlBank "control-ambient")
      && isNothing (Scenes.sceneForControlBank "control-default")
      && isNothing (Scenes.sceneForControlBank ""))

  assert "the arm threshold goes unprefixed, like the click and the monitor"
    (Machine.perform (rigOf [ idle 0 ]) LB.Focused (LB.ArmLevel (-24.0))
      == [ Machine.Command "arm-24.0" ])

  assert "the card names all eight loops, in order"
    (map _.name (Array.take LB.nLoops (fromMaybe [] (map _.cells (Array.head LoopTw.pages))))
      == map (\n -> "Loop " <> show n) (Array.range 1 LB.nLoops))

  -- The range on the card is computed from the constant the knob actually uses.
  -- A card claiming 250 ms while `fadeTop` said 200 would be worse than none.
  assert "the printed ranges come from the scales themselves"
    (LoopTw.paramRange LoopTw.PFade == "0 to 200 ms"
      && LoopTw.paramRange LoopTw.PDecay == "hold at the top, down to −12 dB a pass")

  -- Enumerated from `LoopPhase`, so a seventh state would appear on the card
  -- rather than being quietly absent from the only place anyone would look.
  assert "the colour key covers every phase a loop with material can be in"
    (Array.length LoopTw.phaseKey == Array.length LooperSock.allPhases)

  log ""
  log "  The Twister, as the Looper page prints it:"
  for_ LoopTw.pages \pg -> do
    log ""
    log ("    Page " <> show (pg.bank + 1) <> " — " <> pg.name)
    for_ (Array.range 0 3) \row ->
      log ("      " <> joinWith "  "
        (map (\c -> padTo 16 (if c.name == "" then "·" else c.name))
          (Array.slice (row * 4) (row * 4 + 4) pg.cells)))
  log ""
  log ("    loop colours: " <> joinWith ", "
    (map (\k -> k.phase <> " " <> k.tone) LoopTw.phaseKey))

  -- **Printed as markdown, because `docs/RECIPES.md` is a copy of this.** The
  -- recipes are data in `Data.Looper.Recipes`; the modal renders them and this
  -- prints them, so the file on disk has one author and cannot quietly disagree
  -- with the app. Regenerate by pasting this section over the body of the doc.
  log ""
  log "  Recipes, as markdown for docs/RECIPES.md:"
  log ""
  log ("    > " <> Recipes.preamble)
  for_ Recipes.recipes \r -> do
    log ""
    log ("    ## " <> r.name)
    log ""
    log ("    " <> r.why)
    log ""
    for_ r.steps \st' -> do
      log ("    - " <> (if st'.at == "" then "" else "**" <> st'.at <> "** ")
             <> st'.act)
      for_ st'.expect \e -> log ("      - *" <> e <> "*")
    for_ r.note \n -> do
      log ""
      log ("    > " <> n)

  -- **The printed sheet is the same tables again**, which is the only reason it
  -- is allowed to exist: a third rendering of a layout is a third thing to keep
  -- true unless it is generated, and then it is free. These check that it says
  -- everything the surfaces say, rather than checking any particular markup —
  -- how it looks on paper is not a thing a test can know.
  assert "the sheet names every control and every recipe"
    (let doc = Sheet.sheet
     in Array.all (\pg -> Array.all
            (\c -> c.name == "" || String.contains (String.Pattern (Sheet.escape c.name)) doc)
            pg.cells)
          LoopTw.pages
        && Array.all (\r -> String.contains (String.Pattern (Sheet.escape r.name)) doc)
             Recipes.recipes
        && Array.all (\r -> Array.all
              (\st' -> String.contains (String.Pattern (Sheet.escape st'.act)) doc)
              r.steps)
             Recipes.recipes)

  -- Pages are numbered from one everywhere a person reads them, and the sheet
  -- is nothing but a thing a person reads. A `bank` printed raw would be the
  -- loop-numbering bug again, on paper this time.
  assert "the sheet counts its pages from one"
    (String.contains (String.Pattern "Page 4 — Set up") Sheet.sheet
      && not (String.contains (String.Pattern "Page 0") Sheet.sheet))

  -- The colours are a claim about the device and the sheet has to make the same
  -- claim, or matching paper to a lit encoder is guesswork. One rule per tone,
  -- emitted from `swatch` rather than typed into the stylesheet.
  assert "every tone has ink on the sheet"
    (Array.all
      (\t -> String.contains
               (String.Pattern (".tone-" <> LoopTw.toneName t <> "{background:"
                                  <> LoopTw.swatch t <> "}"))
               Sheet.sheet)
      [ LoopTw.Red, LoopTw.Orange, LoopTw.Yellow, LoopTw.Green
      , LoopTw.Teal, LoopTw.Blue, LoopTw.Violet ])

  -- Nothing in the tables is hostile, but a name that closed a tag early would
  -- silently lose everything after it — the worst failure a printed reference
  -- has, because paper cannot say it went wrong.
  assert "the sheet escapes what could close a tag"
    (Sheet.escape "a<b>&\"c\"" == "a&lt;b&gt;&amp;&quot;c&quot;"
      && Sheet.escape "level — silent to full" == "level — silent to full")

  -- **Eight identical paragraphs is not a reference, it is a wall.** The loop
  -- encoders describe themselves word for word alike — that is the property the
  -- surface is built on — so the detail grid folds a run of them into one.
  assert "the sheet prints the eight loops once, as a range"
    (String.contains (String.Pattern "Loop 1 – Loop 8") Sheet.sheet)

  -- Every step that can report says what right looks like, which is what makes
  -- the list a test script rather than only a manual. A recipe whose steps are
  -- all silent is one nobody can tell has gone wrong.
  assert "every recipe has steps, and most of them say what to expect"
    (Array.all
      (\r -> not (Array.null r.steps)
          && Array.length (Array.filter (\st' -> isJust st'.expect) r.steps) >= 1)
      Recipes.recipes)

  -- Printed, not just asserted about. The map is the thing a person needs when
  -- deciding where to put the next bank, and a test that only says "no
  -- collisions" makes them go and read five fields to find out what there was
  -- no collision between.
  log ""
  log "  MC6 bank map (wire numbers; the editor shows each one higher):"
  for_ (Array.sortWith _.bank defaultClaims) \c ->
    log $ "    " <> pad (show c.bank) <> "  " <> Reserved.claimantLabel c.claimant

  log ""
  log "Done."
  where
  -- The card's own word for a cell, by position. Read from `pages` rather than
  -- from `controlAt`, because the name is a fact about the card.
  named bank index =
    maybe "" (\pg -> maybe "" _.name (Array.find (\c -> c.index == index) pg.cells))
      (Array.find (\pg -> pg.bank == bank) LoopTw.pages)

  unsafeMV :: Int -> _
  unsafeMV n = case makeMidiValue n of
    Just mv -> mv
    Nothing -> unsafeMV 0 -- unreachable for valid test values
