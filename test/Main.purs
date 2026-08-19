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
import Data.Looper as Looper
import Data.Looper.Banks as LB
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
  assert "an undecoded function code is named rather than dropped"
    (Read.decodeReply [ 0xF0, 0x00, 0x21, 0x24, 0x03, 0x03, 0x03, 0x20
                      , 0, 0, 0, 0, 0, 0, 0, 18, 0, 0xF7 ]
      == Just (Read.OtherReply 0x03 0x20))

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

  assert "bank names hold the 16 the device showed us"
    (isJust (Model.bankName "Ableton Controls")
      && isNothing (Model.bankName "Ableton Controls!"))

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

  assert "every switch's CC decodes back to itself"
    (Array.all
      (\s -> LB.decodeSwitch LB.switchChannel (LB.switchCC s.slot s.i) 127
               == Just { slot: s.slot, switch: s.i, down: true })
      everySwitch)

  assert "and a release decodes as one"
    (Array.all
      (\s -> map _.down (LB.decodeSwitch LB.switchChannel (LB.switchCC s.slot s.i) 0)
               == Just false)
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

  assert "each of the six loop switches holds to the config bank"
    (Array.length loopBankSwitches == LB.loopSwitches
      && Array.all
        (\e -> Array.any
          (\m -> m.msgType == MsgBankJump && m.action == ActionLongPress && m.data1 == 23)
          e.sw.messages)
        loopBankSwitches)

  assert "and taps nowhere, because the app decides what a tap means"
    (Array.all
      (\e -> Array.all (\m -> m.msgType /= MsgBankJump || m.action /= ActionPress) e.sw.messages)
      loopBankSwitches)

  -- The pair the app times a hold with. If the release ever stopped being
  -- written, a hold and a tap would be the same message.
  assert "a loop switch sends 127 down and 0 up on its own CC"
    (Array.all
      (\e -> Array.any (\m -> m.msgType == MsgCC && m.channel == LB.switchChannel
                              && m.data1 == LB.switchCC LB.LoopBank e.i
                              && m.data2 == 127 && m.action == ActionPress) e.sw.messages
          && Array.any (\m -> m.msgType == MsgCC && m.channel == LB.switchChannel
                              && m.data1 == LB.switchCC LB.LoopBank e.i
                              && m.data2 == 0 && m.action == ActionRelease) e.sw.messages)
      loopBankSwitches)

  -- The engine has six loops; the bank offers six places to put a foot. These
  -- are two copies of one number, and the daemon cannot be asked from here.
  assert "the loop bank offers as many loops as the engine has"
    (LB.loopSwitches == 6)

  -- Switches 9-11 are a second FS3X that may not be plugged in. A way out that
  -- lands there is a bank you can walk into and not leave, and you would find
  -- that out with a foot rather than a compiler.
  assert "every bank's way out is reachable without a second FS3X"
    (Array.all
      (\cb -> Array.any
        (\e -> e.i <= 8
            && Array.any (\m -> m.msgType == MsgBankJump && m.action == ActionPress)
                 e.sw.messages)
        (Array.filter (\e -> e.cb.mc6BankNumber == cb.mc6BankNumber) familySwitches))
      family)

  log ""
  log "Done."
  where
  unsafeMV :: Int -> _
  unsafeMV n = case makeMidiValue n of
    Just mv -> mv
    Nothing -> unsafeMV 0 -- unreachable for valid test values
