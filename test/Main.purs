module Test.Main where

import Prelude

import Data.Argonaut.Core (stringify)
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
import Data.MC6.Board as Board
import Data.MC6.ControlBank as ControlBank
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..), MC6MsgType(..), MC6TogglePosition(..))
import Data.MC6.Model as Model
import Data.MC6.Settings as Settings
import Test.MC6Capture as Capture
import Data.Looper as Looper
import Component.Looper.Slots as Slots
import Data.Looper.Banks as LB
import Data.MC6.Diagnostics as Diagnostics
import Data.Looper.Machine as Machine
import Data.String as String
import Data.MC6.Read as Read
import Data.MC6.SysEx as SysEx
import Data.MC6.Dump as Dump
import Data.MC6.Global as Global
import Data.MC6.Survey as Survey
import Data.MC6.Verb as Verb
import Data.Tuple (Tuple(..))
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

  -- Itajara still has to bring a layout, or the Looper page renders nothing.
  assert "Itajara has a layout"
    (Array.any (\p -> p.meta.id == PedalId "itajara" && isJust p.layout) Registry.pedals)

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
  assert "unknown banks are Unknown, not empty"
    (Array.all (\c -> c.provenance == Survey.Unknown)
      (Array.filter (\c -> c.bankNumber /= 20) cards))

  -- The default bank lives at 20 and its nine switches pad out to twelve.
  case Array.find (\c -> c.bankNumber == 20) cards of
    Nothing -> assert "default control bank surveyed at bank 20" false
    Just c -> do
      assert "default control bank surveyed at bank 20" (c.provenance == Survey.Authored)
      assert "its name survives the survey" (c.name == "Default Controls")
      assert "padding to 12 is Blank, not Raw"
        (Array.length (Array.filter (_ == Verb.Blank) c.slots) == 4)

  -- Navigation edges: the default bank's return switch carries no jump until
  -- the bank is compiled, so an uncompiled survey has no edges to draw.
  assert "no phantom navigation edges" (Array.null (Survey.navigationEdges cards))

  let navCard =
        { bankNumber: 1, name: "Boards", provenance: Survey.Authored
        , slots: [ Verb.Navigation (Verb.ToBank 20), Verb.Blank ]
        , observedNames: [], agrees: Nothing }
  assert "a bank jump becomes a graph edge"
    (Survey.navigationEdges [ navCard ] == [ Tuple 1 20 ])
  assert "unknown banks contribute no edges"
    (Array.null (Survey.navigationEdges
      [ { bankNumber: 5, name: "", provenance: Survey.Unknown
        , slots: [ Verb.Navigation (Verb.ToBank 3) ]
        , observedNames: [], agrees: Nothing } ]))

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
        , slots, observedNames: [], agrees: Nothing }
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
        , slots: [], observedNames: [], agrees: Nothing } ]))

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
  assert "a switch with all three gestures sends all three, one per action"
    (map ccsOf (loopBankSwitch 0)
      == Just [ Tuple ActionRelease 127
              , Tuple ActionDoubleTapRelease 64
              , Tuple ActionLongPress 1
              ])

  -- **The fallback.** The device suppresses Release on a double whether or not
  -- anything is bound to it, so a switch with no second meaning would answer a
  -- fumbled double with silence. It gets the tap's own value instead, and a
  -- double tap on Click toggles the click once.
  let clickSwitch = loopBankSwitch 11
  assert "a switch with no second meaning answers a double tap as a tap"
    (map ccsOf clickSwitch
      == Just [ Tuple ActionRelease 127
              , Tuple ActionDoubleTapRelease 127
              ])

  -- The jump has to ride on the same action as the CC that reports it, or the
  -- app is told about a press from a bank the board has already left.
  assert "a navigating switch jumps on the same actions it reports on"
    (map jumpsOf (loopBankSwitch 6)
      == Just [ ActionRelease, ActionDoubleTapRelease ])

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
  assert "a hold that navigates nowhere programs no jump"
    (LB.sendsTo LB.LoopBank 0 LB.Hold == Just (LB.ToSlot LB.ConfigBank)
      && LB.sendsTo LB.LoopBank 0 LB.Tap == Nothing
      && LB.sendsTo LB.ConfigBank 0 LB.Tap == Just (LB.ToSlot LB.QuantiseBank)
      && LB.sendsTo LB.ConfigBank 0 LB.Hold == Nothing)

  log ""
  log "What a gesture means (Data.Looper.Machine)..."

  let idle n = { index: n, state: "idle", layers: 0, loopFrames: 0, loopSecs: 0.0
               , pos: 0, phase: 0.0, armed: false, recording: false, quant: false
               , muted: false, reverse: false, pan: 64, speed: 1.0, pendulum: false
               , oneShot: false, levelArm: false, firing: false
               , chance: 1.0, skipping: false, fadeMs: 0.0, decayDb: 0.0
               , pendingAt: -1, shapes: [] }
      withState n s ls = (idle n) { state = s, layers = ls }
      rigOf ls = { loops: ls, focus: 0 }

  assert "tapping an empty loop records it"
    (Machine.act (rigOf [ idle 0 ]) (LB.switchGesture LB.LoopBank 0 LB.Tap)
      == [ Machine.Focus 0, Machine.Command "0r" ])

  -- The one that was wrong in use. Undo removes a layer and deliberately keeps
  -- the loop's length, so undoing the last one leaves layers 0, a length, and a
  -- state still reading "playing". Testing emptiness as `state == "idle" &&
  -- layers == 0` made that a playing loop: tapping offered stop, and a loop
  -- undone to nothing could never be recorded into from the board again.
  assert "a loop undone to nothing records again, length and state notwithstanding"
    (Machine.act (rigOf [ (withState 0 "playing" 0) { loopFrames = 155215 } ])
       (LB.switchGesture LB.LoopBank 0 LB.Tap)
      == [ Machine.Focus 0, Machine.Command "0r" ])

  assert "and double-tapping it does not close a loop a fifth of a second long"
    (Machine.act (rigOf [ (withState 0 "playing" 0) { loopFrames = 155215 } ])
       (LB.switchGesture LB.LoopBank 0 LB.Double)
      == [ Machine.Focus 0, Machine.Handled "already recording" ])

  -- Closing is a command and, when the config bank is wired, a bank change too.
  -- It is off for now because a courtesy that lands on a page of unwired
  -- switches strands the player after every loop they record.
  assert "tapping a recording loop closes it"
    (Machine.act (rigOf [ withState 0 "recordingFirst" 0 ]) (LB.switchGesture LB.LoopBank 0 LB.Tap)
      == [ Machine.Focus 0, Machine.Command "0r" ])

  -- Transport, once the engine grew one. Explicit h0/h1 rather than a flipping
  -- h, because a stopped loop is invisible and a dropped toggle would leave the
  -- app and the engine disagreeing with nothing on screen to show it.
  assert "tapping a playing loop stops it"
    (Machine.act (rigOf [ withState 0 "playing" 1 ]) (LB.switchGesture LB.LoopBank 0 LB.Tap)
      == [ Machine.Focus 0, Machine.Command "0h0" ])

  assert "and tapping a stopped one brings it back"
    (Machine.act (rigOf [ (withState 0 "playing" 1) { muted = true } ])
       (LB.switchGesture LB.LoopBank 0 LB.Tap)
      == [ Machine.Focus 0, Machine.Command "0h1" ])

  -- Overdubbing onto something you cannot hear is a way to record a mistake
  -- twice, so the loop comes back first.
  assert "double tapping a stopped loop unmutes before overdubbing"
    (Machine.act (rigOf [ (withState 0 "playing" 1) { muted = true } ])
       (LB.switchGesture LB.LoopBank 0 LB.Double)
      == [ Machine.Focus 0, Machine.Command "0h1", Machine.Command "0r" ])

  assert "stop all reaches every loop"
    (Machine.act (rigOf []) (LB.switchGesture LB.LoopBank 7 LB.Tap)
      == map (\i -> Machine.Command (show i <> "h0")) (Array.range 0 5))

  -- Whatever the engine calls it, no layers means record.
  assert "any state with no layers records"
    (Machine.act (rigOf [ withState 0 "weird" 0 ]) (LB.switchGesture LB.LoopBank 0 LB.Tap)
      == [ Machine.Focus 0, Machine.Command "0r" ])

  assert "but a double tap on a playing loop does overdub, which the engine has"
    (Machine.act (rigOf [ withState 0 "playing" 1 ]) (LB.switchGesture LB.LoopBank 0 LB.Double)
      == [ Machine.Focus 0, Machine.Command "0r" ])

  assert "a hold only moves the focus; the MC6 changes bank by itself"
    (Machine.act (rigOf [ withState 0 "playing" 1 ]) (LB.switchGesture LB.LoopBank 2 LB.Hold)
      == [ Machine.Focus 2, Machine.Handled "configuring loop 3" ])

  assert "undo and clear act on the focused loop"
    (Machine.act { loops: [ idle 0, idle 1, idle 2 ], focus: 2 } (LB.switchGesture LB.LoopBank 8 LB.Tap)
      == [ Machine.Command "2u" ]
      && Machine.act { loops: [ idle 0 ], focus: 1 } (LB.switchGesture LB.LoopBank 9 LB.Tap)
        == [ Machine.Command "1c" ])

  -- The config family acts on the focused loop, which is what a hold sets. One
  -- config bank serving six loops only works because of that.
  assert "reverse and clear act on the focused loop, not the pressed switch"
    (Machine.act { loops: [ idle 0, idle 1, idle 2 ], focus: 2 } (LB.switchGesture LB.ConfigBank 4 LB.Tap)
      == [ Machine.Command "2rev1" ]
      && Machine.act { loops: [], focus: 1 } (LB.switchGesture LB.ConfigBank 9 LB.Tap)
        == [ Machine.Command "1c" ])

  assert "the pan bank places the focused loop across the field"
    (map (\i -> Machine.act { loops: [], focus: 0 } (LB.switchGesture LB.PanBank i LB.Tap))
       [ 0, 2, 4 ]
      == [ [ Machine.Command "0pan0" ]
         , [ Machine.Command "0pan64" ]
         , [ Machine.Command "0pan127" ] ])

  -- Free and Grid are real; the bar counts have nothing to select, because the
  -- engine's grid is the anchor loop's cycle and not a bar.
  assert "quantise sets the grid flag and is honest about bar counts"
    (Machine.act { loops: [], focus: 3 } (LB.switchGesture LB.QuantiseBank 0 LB.Tap)
      == [ Machine.Command "3g0" ]
      && Machine.act { loops: [], focus: 3 } (LB.switchGesture LB.QuantiseBank 1 LB.Tap)
        == [ Machine.Command "3g1"
           , Machine.Handled "on the grid — bar counts need the frame-to-bar join" ])

  assert "and stop-all reaches every loop, from any bank"
    (Machine.act (rigOf []) (LB.switchGesture LB.QuantiseBank 7 LB.Tap)
      == map (\i -> Machine.Command (show i <> "h0")) (Array.range 0 5))

  -- Direction is the sign of speed, not a second control, so the bottom row is
  -- one press that says both things rather than two in the right order.
  -- The legend on the Looper page is what a player reads to find out what the
  -- six unmarked footswitches do, and for a while it was a hand-written copy of
  -- the loop bank's six shown whatever bank the board was on. So with the board
  -- on config it said J was Clear while J was End Stop — which reads exactly
  -- like a switch wired to the wrong place, and is worse than saying nothing.
  assert "the aux legend is the bank's own table"
    (LB.auxLegend LB.LoopBank
      == [ { key: "G", what: "< Board" }, { key: "H", what: "Stop All" }
         , { key: "I", what: "Undo" }, { key: "J", what: "Clear" }
         , { key: "K", what: "Capture" }, { key: "L", what: "Click" } ])

  -- **The rule about feet.** G to L have no markings, so they are remembered as
  -- positions; a switch that clears a loop on one page and sets an end-state on
  -- the next cannot be learned at all. Everything but the way out is identical
  -- on every bank, and the way out differs only in where "out" is.
  assert "the toolbar means the same thing on every bank"
    (Array.all
      (\slot -> map _.what (Array.drop 1 (LB.auxLegend slot))
        == [ "Stop All", "Undo", "Clear", "Capture", "Click" ])
      LB.allSlots)

  -- The second gesture, where a switch carries one. Same six everywhere for
  -- the same reason the first six are: an unmarked switch is a position, and
  -- a position that means different things on different pages cannot be
  -- learned at all.
  assert "and so does the second gesture, where there is one"
    (Array.all
      (\slot -> map (map LB.dutyLabel <<< _.double)
        (Array.catMaybes (map (LB.dutiesAt slot) (Array.range 7 11)))
        == [ Just "Start All", Just "Redo", Just "Clear All", Just "Save", Nothing ])
      LB.allSlots)

  -- Claiming the past is the live gesture and the one thing no pedal can do;
  -- saving a WAV is never time-critical and was holding the fast slot while
  -- the feature the ring exists for had no switch at all.
  assert "capture has the tap and saving has the double, not the other way round"
    (map LB.dutyLabel (Array.catMaybes [ LB.dutyAt LB.LoopBank 10 ]) == [ "Capture" ]
      && Machine.act (rigOf []) (LB.switchGesture LB.PanBank 10 LB.Tap)
        == [ Machine.Command "0t" ]
      && Machine.act (rigOf []) (LB.switchGesture LB.PanBank 10 LB.Double)
        == [ Machine.Command "w" ])

  assert "and only the way out differs, because only its destination does"
    (map (\slot -> map _.what (Array.take 1 (LB.auxLegend slot))) LB.allSlots
      == [ [ "< Board" ], [ "< Loops" ], [ "< Loops" ]
         , [ "< Loops" ], [ "< Loops" ], [ "< Loops" ] ])

  -- The code has to say it too, or two tables agree until one of them does not.
  assert "and the meaning table answers the toolbar without consulting the bank"
    (Array.all
      (\slot -> Machine.act { loops: [], focus: 2 } (LB.switchGesture slot 9 LB.Tap)
        == [ Machine.Command "2c" ])
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
           acts = Machine.act { loops: [], focus: 0 } (LB.switchGesture r.slot r.i LB.Tap)
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
    (map (\i -> Machine.act { loops: [], focus: 2 } (LB.switchGesture LB.SpeedBank i LB.Tap))
       [ 0, 2, 4 ]
      == [ [ Machine.Command "2sp0.25" ]
         , [ Machine.Command "2sp1.0" ]
         , [ Machine.Command "2sp2.0" ] ])

  -- **Set, never flip.** The toggles read the engine's own answer out of the
  -- snapshot and send the explicit form, so a dropped command cannot leave the
  -- app and the engine disagreeing for ever about which way a loop is facing.
  assert "and pendulum is a config switch of its own, sent as a value"
    (Machine.act { loops: [], focus: 4 } (LB.switchGesture LB.ConfigBank 5 LB.Tap)
      == [ Machine.Command "4pend1" ])

  -- **The mode changes what the switch means, and the switch keeps its name.**
  --
  -- A one-shot is silent between passes by definition, so there is no playing
  -- and stopped for a tap to toggle between — the only thing it can mean is
  -- fire. Which is exactly why the mode rides in the snapshot: what the foot
  -- does depends on a fact only the engine holds, and no amount of remembering
  -- on this side would be as good as being told.
  assert "a tap on a one-shot fires it, where a tap on any other loop stops it"
    (Machine.act (rigOf [ (withState 0 "playing" 1) { oneShot = true } ])
       (LB.switchGesture LB.LoopBank 0 LB.Tap)
      == [ Machine.Focus 0, Machine.Command "0f" ]
      && Machine.act (rigOf [ withState 0 "playing" 1 ])
           (LB.switchGesture LB.LoopBank 0 LB.Tap)
        == [ Machine.Focus 0, Machine.Command "0h0" ])

  -- A level-armed loop waits for a sound that may never come, holding the one
  -- converter the rig has. A press has to be able to take that back, or one
  -- loop can lock out the other five with nothing on screen to blame.
  -- **Stranding a recording is the worst failure this surface has**: one
  -- converter, so a loop left writing locks out all five others, silently, from
  -- a bank you are no longer standing on. It happened twice in one session.
  -- Closing is what the gesture meant either way — a deliberate hold is asking
  -- to configure a loop that has no length yet, and a tap held a little too long
  -- meant to close it.
  assert "holding a loop that is still recording closes it on the way to config"
    (Machine.act (rigOf [ withState 0 "recordingFirst" 0 ]) (LB.switchGesture LB.LoopBank 0 LB.Hold)
      == [ Machine.Focus 0, Machine.Command "0r"
         , Machine.Handled "closed loop 1 on the way to its config" ]
      && Machine.act (rigOf [ (idle 0) { armed = true } ]) (LB.switchGesture LB.LoopBank 0 LB.Hold)
        == [ Machine.Focus 0, Machine.Command "0r"
           , Machine.Handled "stopped loop 1 listening" ]
      && Machine.act (rigOf [ withState 0 "playing" 2 ]) (LB.switchGesture LB.LoopBank 0 LB.Hold)
        == [ Machine.Focus 0, Machine.Handled "configuring loop 1" ])

  assert "and a press takes back an arm that is still waiting"
    (Machine.act (rigOf [ (idle 0) { armed = true } ]) (LB.switchGesture LB.LoopBank 0 LB.Tap)
      == [ Machine.Focus 0, Machine.Command "0r" ])

  -- Modes, where Chance was. Two toggles rather than five values, because
  -- one-shot and level-arm are not exclusive — which is the thing a bank of
  -- five choices cannot say.
  assert "the modes bank sets its toggles from what the engine last reported"
    (Machine.act { loops: [ (idle 0) { levelArm = true } ], focus: 0 }
       (LB.switchGesture LB.ModesBank 0 LB.Tap) == [ Machine.Command "0one1" ]
      && Machine.act { loops: [ (idle 0) { levelArm = true } ], focus: 0 }
           (LB.switchGesture LB.ModesBank 1 LB.Tap) == [ Machine.Command "0lev0" ])

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
    (Machine.act { loops: [ (idle 0) { fadeMs = 25.0 } ], focus: 0 }
       (LB.switchGesture LB.ModesBank 3 LB.Tap)
      == [ Machine.Command "0xf50.0", Machine.Handled "loop 1 wraps 50 ms" ])

  assert "and a press sends the rung it stepped to"
    (Machine.act { loops: [ (idle 0) { chance = 0.5 } ], focus: 0 }
       (LB.switchGesture LB.ModesBank 2 LB.Tap)
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
  -- board on config, A is Quantise, so labelling the first loop "A" there
  -- points a foot at the wrong thing.
  assert "a loop is lettered only when the board is on the bank that reaches it"
    (map (LB.faceLoopKey (LB.face (Just LB.LoopBank))) [ 0, 3 ] == [ "A", "D" ]
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
    (Machine.act (rigOf []) (LB.switchGesture LB.LoopBank 0 LB.Tap)
      == [ Machine.Focus 0, Machine.Unavailable "loop 1 is not in the snapshot" ])

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

  assert "six banks, on consecutive numbers from the base"
    (map _.mc6BankNumber family == Array.range 22 27)

  assert "each bank has its full twelve switches"
    (Array.all (\cb -> Array.length cb.switches == ControlBank.switchCount) family)

  -- Names are truncated in silence by `shortNameTLV`/`longNameTLV`, so an
  -- over-long label does not fail, it just arrives on the device meaning
  -- something slightly different from what this table says.
  assert "no label is longer than the device shows"
    (Array.all (\e -> String.length e.sw.label <= 8) familySwitches)

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
      (\m -> m.msgType /= MsgBankJump || Array.elem m.data1 (Array.range 22 27 <> [ 1 ]))
      (Array.concatMap (\e -> e.sw.messages) familySwitches))

  let loopBankSwitches =
        Array.filter (\e -> e.cb.mc6BankNumber == 22 && e.i < LB.loopSwitches) familySwitches

  -- A jump on press emits everything after it from the bank you have already
  -- arrived at. That is how a press on one bank and its release on another came
  -- to be seen, and a hold nobody made came to be fired. The gesture is one
  -- message now, so the ordering is belt as well as braces — kept because the
  -- reason it was needed has not stopped being true of the device.
  assert "a tap's bank jump fires on the release, after the CC that reports it"
    (let
       cfg = LB.banks { base: 22, boardBank: 0 }
       switchA = do
         cb <- Array.find (\b -> b.name == "Loop Cfg") cfg
         Array.index cb.switches 0
       acts = maybe [] (map _.action <<< _.messages) switchA
     -- Quantise on a tap: a CC on each of the two gestures the switch answers,
     -- then the jump on each of the same two.
     in acts == [ ActionRelease, ActionDoubleTapRelease
                , ActionRelease, ActionDoubleTapRelease
                ])

  assert "each of the six loop switches holds to the config bank"
    (Array.length loopBankSwitches == LB.loopSwitches
      && Array.all
        (\e -> Array.any
          (\m -> m.msgType == MsgBankJump && m.action == ActionLongPress && m.data1 == 23)
          e.sw.messages)
        loopBankSwitches)

  -- **Nothing at all on ActionPress.** The device defers every gesture until it
  -- knows which one it is, so a message on the press either never fires (a
  -- double suppresses it) or fires before the gesture has happened (a hold). It
  -- is the one action this family must not use.
  assert "and nothing anywhere in the family fires on the bare press"
    (Array.all
      (\e -> Array.all (\m -> m.action /= ActionPress) e.sw.messages)
      familySwitches)

  -- One CC per gesture, all on the switch's own number: the number says where
  -- the press came from and the value says which gesture it was.
  assert "a loop switch reports all three gestures on its own CC"
    (Array.all
      (\e -> Array.all
        (\g -> Array.any
          (\m -> m.msgType == MsgCC && m.channel == LB.switchChannel
                   && m.data1 == LB.switchCC LB.LoopBank e.i
                   && m.data2 == LB.gestureValue g
                   && m.action == LB.gestureAction g)
          e.sw.messages)
        LB.allGestures)
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
       everyMessage =
         ((LB.banks { base: 22, boardBank: 0 } <> [ Diagnostics.gestureProbeBank 28 0 ])
            >>= _.switches) >>= _.messages
       inSeven n = n >= 0 && n <= 127
     in Array.all
          (\m -> inSeven m.data1 && inSeven m.data2 && inSeven m.data3
                   && inSeven m.data4 && inSeven m.channel)
          everyMessage)

  -- Switches 9-11 are a second FS3X that may not be plugged in. A way out that
  -- lands there is a bank you can walk into and not leave, and you would find
  -- that out with a foot rather than a compiler.
  --
  -- Any action counts. This used to require `ActionPress` and so failed the
  -- moment tap jumps moved to the release, which was the test noticing a change
  -- rather than a fault — the claim is that a way out is *reachable*, and a hold
  -- is as much a way out as a tap.
  assert "every bank's way out is reachable without a second FS3X"
    (Array.all
      (\cb -> Array.any
        (\e -> e.i <= 8
            && Array.any (\m -> m.msgType == MsgBankJump) e.sw.messages)
        (Array.filter (\e -> e.cb.mc6BankNumber == cb.mc6BankNumber) familySwitches))
      family)

  log ""
  log "Done."
  where
  unsafeMV :: Int -> _
  unsafeMV n = case makeMidiValue n of
    Just mv -> mv
    Nothing -> unsafeMV 0 -- unreachable for valid test values
