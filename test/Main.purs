module Test.Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Midi (makeCC, makeMidiValue, makeProgramNumber)
import Data.Pedal (PedalId(..))
import Data.Pedal.Engage (EngageConfig(..), EngageState(..), bypassCCs)
import Effect (Effect)
import Effect.Console (log)
import Config.Registry as CRegistry
import Engine (initEngineFromPedals, pedalsOnChannel)
import Engine.Storage (engineToJson, parseEngine, parseCardOrder, parsePresets, parseBoardPresets, parseEngageState)
import Data.MC6.Board as Board
import Data.MC6.ControlBank as ControlBank
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..))
import Data.MC6.Read as Read
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

  let cards = Survey.survey reg 1 [ ControlBank.exampleControlBank ] [] Map.empty Map.empty

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
      readCards = Survey.survey reg 1 [ ControlBank.exampleControlBank ] [] readNames readSwitches
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
    (Read.decodeReply [ 0xF0, 0x00, 0x21, 0x24, 0x03, 0x03, 0x06, 0x01
                      , 0, 0, 0, 0, 0, 0, 0, 18, 0, 0xF7 ]
      == Just (Read.OtherReply 0x06 0x01))

  -- No request-frame tests: there are no request frames. Sweeping the
  -- function-code space found nothing that asks for bank data, because the
  -- device volunteers a full dump on connect instead. All this module does is
  -- decode, and the decoder is tested above against the device's own bytes.

  log ""
  log "Done."
  where
  unsafeMV :: Int -> _
  unsafeMV n = case makeMidiValue n of
    Just mv -> mv
    Nothing -> unsafeMV 0 -- unreachable for valid test values
