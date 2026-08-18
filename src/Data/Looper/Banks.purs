-- | The MC6 banks for the six-loop looper, and the namespace they speak in.
-- |
-- | This is the build-time artifact of `itajara-in-atlantis` §"The MC6 is a
-- | keyboard": six banks, generated here and uploaded once. Nothing rewrites
-- | them live, because relabelling a switch costs a preset upload of well over
-- | a second and the state of a loop changes faster than that. What each loop
-- | is doing is shown on the computer; the MC6 supplies twelve labelled places
-- | to put a foot.
-- |
-- | ## Every press says where it came from
-- |
-- | A switch does not send "record" or "loop 3". It sends **which switch on
-- | which bank**, and the app decides what that means. The whole meaning table
-- | is then in one place that can be changed without touching the hardware, and
-- | — more importantly — the app never has to remember which bank the board is
-- | showing. It is told, on every press.
-- |
-- | That is worth the CC space. If a press only said "switch 3", the app would
-- | have to infer the bank from its own memory of the last bank change, and the
-- | first missed message would put the two out of step permanently, with every
-- | subsequent press doing something confidently wrong. Here a stale bank
-- | arrives as a CC that does not fit the app's expectation, which is a thing
-- | you can *notice*.
-- |
-- | The arithmetic is deliberately readable in a MIDI monitor:
-- |
-- | ```
-- | cc = 16 * (bank slot + 1) + switch index
-- | ```
-- |
-- | so bank slots start at 16, 32, 48, 64, 80, 96 and a switch is the offset
-- | within the block — CC 51 is slot 2, switch D, at a glance and without a
-- | table. Four CCs per block go unused, which buys that legibility cheaply.
-- | CC 0-15 are left alone because CC 0 and 32 are bank select and the low
-- | numbers are where every other device's defaults live.
-- |
-- | ## Channel 16 is the app's own
-- |
-- | The pedals hold channels 2-8, 10-12, 14 and 15; channel 1 is board recall
-- | and channel 13 is Itajara's pedal surface. Channel 16 is free, and taking a
-- | whole channel for "the MC6 talking to the app about its own switches" keeps
-- | this namespace from ever colliding with a message meant for a pedal. It is
-- | a control plane, not a pedal.
-- |
-- | Note the split from `Data.Looper.looperBank`, which is a different thing
-- | wearing a similar name: that bank drives the *single-loop* transport
-- | through Itajara's pedal CCs on channel 13, and stays as long as the old
-- | Looper page does. This is the six-loop machine, and it addresses the app.
-- |
-- | ## Press, release, and the hold
-- |
-- | Each switch sends its CC at 127 on press and 0 on release. The app times
-- | the gap, because the double-tap window has to be a function of the grid
-- | rather than a constant (§"Gesture timing"), and the MC6 cannot know the
-- | grid.
-- |
-- | A **hold** is the one gesture the MC6 resolves by itself: a long press on a
-- | loop switch jumps to the config bank unconditionally, no state required.
-- | The app must reach the same conclusion, and it does so by arming a timer on
-- | the *press* rather than by measuring at the release — so the two agree at
-- | the same instant, and it does not matter whether the device suppresses the
-- | release message after a long press or sends it anyway. The release only
-- | cancels a timer that has not fired.
module Data.Looper.Banks
  ( switchChannel
  , BankSlot(..)
  , allSlots
  , slotIndex
  , slotFromIndex
  , slotName
  , loopSwitches
  , switchCC
  , SwitchPress
  , decodeSwitch
  , labelOf
  , banks
  ) where

import Prelude

import Data.Array as Array
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch, switchCount)
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..), MC6Message)
import Data.Maybe (Maybe(..))

-- | The channel the MC6 uses to address the app about its own switches.
switchChannel :: Int
switchChannel = 16

-- | How far apart two banks' CC blocks sit. Sixteen rather than twelve so the
-- | block boundary falls on a round number and the switch index is the low
-- | nibble of the CC.
stride :: Int
stride = 16

-- | How many of the loop bank's switches address a loop.
-- |
-- | Must equal `N_LOOPS` in the daemon. Six because that is what the MC6 has
-- | underfoot without an FS3X, and a loop you can only reach through an
-- | accessory is a loop you will not use.
loopSwitches :: Int
loopSwitches = 6

-- | The banks, as roles rather than numbers. Which MC6 bank each lands on is a
-- | deployment question (`banks` takes a base); which bank a press came from is
-- | not, and that is what travels in the CC.
data BankSlot
  = LoopBank
  | ConfigBank
  | QuantiseBank
  | SpeedBank
  | ChanceBank
  | PanBank

derive instance Eq BankSlot
derive instance Ord BankSlot

allSlots :: Array BankSlot
allSlots = [ LoopBank, ConfigBank, QuantiseBank, SpeedBank, ChanceBank, PanBank ]

slotIndex :: BankSlot -> Int
slotIndex = case _ of
  LoopBank -> 0
  ConfigBank -> 1
  QuantiseBank -> 2
  SpeedBank -> 3
  ChanceBank -> 4
  PanBank -> 5

slotFromIndex :: Int -> Maybe BankSlot
slotFromIndex = case _ of
  0 -> Just LoopBank
  1 -> Just ConfigBank
  2 -> Just QuantiseBank
  3 -> Just SpeedBank
  4 -> Just ChanceBank
  5 -> Just PanBank
  _ -> Nothing

-- | The bank's name on the device's screen. Eight characters, like a label.
slotName :: BankSlot -> String
slotName = case _ of
  LoopBank -> "Loops"
  ConfigBank -> "Loop Cfg"
  QuantiseBank -> "Quantise"
  SpeedBank -> "Speed"
  ChanceBank -> "Chance"
  PanBank -> "Pan"

slotId :: BankSlot -> String
slotId = case _ of
  LoopBank -> "loops"
  ConfigBank -> "config"
  QuantiseBank -> "quantise"
  SpeedBank -> "speed"
  ChanceBank -> "chance"
  PanBank -> "pan"

-- | Which CC a given switch sends.
switchCC :: BankSlot -> Int -> Int
switchCC slot i = stride * (slotIndex slot + 1) + i

-- | A switch going down or coming up, as the app reads it off the wire.
type SwitchPress =
  { slot :: BankSlot
  , switch :: Int
  , down :: Boolean
  }

-- | Read a CC as a switch press, or refuse it.
-- |
-- | Total and cheap, so the router can offer it every incoming CC and let the
-- | `Nothing` mean "not ours" — which is what keeps this namespace from having
-- | to be checked for at the call site as well as here.
decodeSwitch :: Int -> Int -> Int -> Maybe SwitchPress
decodeSwitch channel ccNum value =
  if channel /= switchChannel || ccNum < stride then Nothing
  else do
    slot <- slotFromIndex (ccNum `div` stride - 1)
    let sw = ccNum `mod` stride
    -- The four CCs above each block are the gap that makes the arithmetic
    -- legible; nothing sends them, so nothing should accept them either.
    if sw >= switchCount then Nothing
      else Just { slot, switch: sw, down: value > 63 }

-- | What a switch is labelled, for logging a press the app was not expecting.
-- |
-- | Reads the same table the device was programmed from, so a surprise can be
-- | reported as "Loop 3, hold to set up" rather than as a CC number — and if
-- | the two ever disagree, the disagreement is between the device and this
-- | table rather than between two copies of the table.
labelOf :: BankSlot -> Int -> Maybe String
labelOf slot i = map _.longName (Array.index (layout slot) i)

-- | Where a switch sends the board. `ToBoard` is the way out of the looper
-- | entirely; everything else stays in the family.
data Jump = ToSlot BankSlot | ToBoard

type SwitchSpec =
  { label :: String
  , longName :: String
  -- | A bank jump on press. Navigation, and the device does it alone.
  , tap :: Maybe Jump
  -- | A bank jump on long press. Only the loop switches carry one.
  , hold :: Maybe Jump
  }

say :: String -> String -> SwitchSpec
say label longName = { label, longName, tap: Nothing, hold: Nothing }

goto :: Jump -> String -> String -> SwitchSpec
goto j label longName = (say label longName) { tap = Just j }

blank :: SwitchSpec
blank = say "" ""

-- | The twelve switches of each bank.
-- |
-- | Switches 0-5 are the MC6's own A-F; 6-8 are the first FS3X; 9-11 a second
-- | one, which may not be plugged in — so **nothing that must be reachable
-- | lives past 8**, and the way back is what that rule is for. It sits at 5 on
-- | every bank whose loops are elsewhere, and at 6 on the loop bank, whose six
-- | loops take the unit's own switches and leave no room lower down.
-- |
-- | The duplicate "< Loops" at 11 on the sub-banks is a convenience for a board
-- | that does have the second FS3X, never the only way out of anywhere.
layout :: BankSlot -> Array SwitchSpec
layout = case _ of

  -- The way out comes first of the six that are not loops, because it is the
  -- only one that must work with a single FS3X: the six loops fill the unit
  -- itself, so switch G is the first place a way home can go. The rest run in
  -- rough order of how much you would miss them.
  LoopBank ->
    Array.mapWithIndex loopSwitch (Array.replicate loopSwitches unit)
      <>
        [ goto ToBoard "< Board" "Back to the board bank"
        , say "Stop All" "Stop every loop"
        , say "Undo" "Undo the last layer"
        , say "Clear" "Clear the chosen loop"
        , say "Take" "Save the take to disk"
        , say "Click" "Click on or off"
        ]

  -- One config bank serves all six loops: it acts on whichever loop was last
  -- touched, which the app knows because the press that got here said so.
  --
  -- The four that lead somewhere sit on A-D, together, because they are the
  -- ones with a value to choose; the rest are switches you press and are done
  -- with.
  ConfigBank ->
    [ goto (ToSlot QuantiseBank) "Quantise" "Set the launch grid"
    , goto (ToSlot SpeedBank) "Speed" "Set playback speed"
    , goto (ToSlot ChanceBank) "Chance" "Set chance and every"
    , goto (ToSlot PanBank) "Pan" "Set stereo placement"
    , say "Reverse" "Play the loop backwards"
    , goto (ToSlot LoopBank) "< Loops" "Back to the loops"
    , say "Pendulum" "Forward, then back"
    , say "Moment" "Sound only while held"
    , say "End Play" "On leaving, keep playing"
    , say "End Stop" "On leaving, stop"
    , say "Take" "Save the take to disk"
    , say "Clear" "Clear this loop"
    ]

  -- Free is the default and sits first, because ambient wants it and because a
  -- loop that quantises when you did not ask is a loop that starts late for a
  -- reason you cannot see.
  --
  -- The all-at-once forms on the second row are the two common cases stated in
  -- one press: everything free, or everything on the same bar.
  QuantiseBank ->
    [ say "Free" "Free length and launch"
    , say "1 Bar" "Round to one bar"
    , say "2 Bars" "Round to two bars"
    , say "4 Bars" "Round to four bars"
    , say "8 Bars" "Round to eight bars"
    , goto (ToSlot ConfigBank) "< Config" "Back to loop config"
    , say "All Free" "Every loop free"
    , say "All 1Bar" "Every loop on one bar"
    , say "All 2Bar" "Every loop on two bars"
    , say "All 4Bar" "Every loop on four bars"
    , blank
    , goto (ToSlot LoopBank) "< Loops" "Back to the loops"
    ]

  -- Speed and direction on one bank, because in SuperDirt's vocabulary they are
  -- one parameter — a negative `speed` is reverse — and splitting them here
  -- would invent a distinction the rest of the rig does not make.
  SpeedBank ->
    [ say "x 1/4" "Quarter speed"
    , say "x 1/2" "Half speed"
    , say "x 1" "Normal speed"
    , say "x 1 1/2" "One and a half speed"
    , say "x 2" "Double speed"
    , goto (ToSlot ConfigBank) "< Config" "Back to loop config"
    , say "Rev 1/4" "Quarter speed, reverse"
    , say "Rev 1/2" "Half speed, reverse"
    , say "Rev 1" "Normal speed, reverse"
    , say "Rev 1.5" "One and a half, reverse"
    , say "Rev 2" "Double speed, reverse"
    , goto (ToSlot LoopBank) "< Loops" "Back to the loops"
    ]

  -- `degrade` on the top row, `every` on the bottom — SuperDirt's two names for
  -- "not every time", kept apart because one is per-cycle chance and the other
  -- is a count, and a single control spanning both would say neither.
  ChanceBank ->
    [ say "Always" "Sound every cycle"
    , say "3 in 4" "Three cycles in four"
    , say "1 in 2" "Sound half the cycles"
    , say "1 in 4" "Sound one cycle in four"
    , say "1 in 8" "One cycle in eight"
    , goto (ToSlot ConfigBank) "< Config" "Back to loop config"
    , say "Every 2" "Sound on every 2nd cycle"
    , say "Every 3" "Sound on every 3rd cycle"
    , say "Every 4" "Sound on every 4th cycle"
    , say "Every 8" "Sound on every 8th cycle"
    , say "Every 1" "Clear the every count"
    , goto (ToSlot LoopBank) "< Loops" "Back to the loops"
    ]

  PanBank ->
    [ say "Left" "Hard left"
    , say "L 66" "Two thirds left"
    , say "L 33" "One third left"
    , say "Centre" "Centre"
    , say "R 33" "One third right"
    , goto (ToSlot ConfigBank) "< Config" "Back to loop config"
    , say "R 66" "Two thirds right"
    , say "Right" "Hard right"
    , say "Wide" "Full stereo width"
    , say "Mono" "Collapse to mono"
    , blank
    , goto (ToSlot LoopBank) "< Loops" "Back to the loops"
    ]

-- | One of the six loop switches.
-- |
-- | The only difference between them is the switch index, which is exactly the
-- | point: a loop is *where you put your foot*, not a mode you enter.
loopSwitch :: Int -> Unit -> SwitchSpec
loopSwitch i _ =
  { label: "Loop " <> show (i + 1)
  , longName: "Loop " <> show (i + 1) <> ", hold to set up"
  , tap: Nothing
  , hold: Just (ToSlot ConfigBank)
  }


-- | Compile the family onto consecutive MC6 banks from `base`.
-- |
-- | Six consecutive banks, taken as a block, so that "which bank is the speed
-- | bank" is arithmetic rather than six separate settings that can be set
-- | inconsistently.
banks :: { base :: Int, boardBank :: Int } -> Array ControlBank
banks cfg = map toBank allSlots
  where
  toBank :: BankSlot -> ControlBank
  toBank slot =
    { id: "itajara-" <> slotId slot
    , name: slotName slot
    , description: describe slot
    , mc6BankNumber: cfg.base + slotIndex slot
    -- Legacy, and read only by `Global.migrateReturns` on stored banks. Set to
    -- where the way back actually is, so it is not a lie if anything ever does
    -- look at it.
    , returnSwitchIndex: case slot of
        LoopBank -> 11
        _ -> 5
    , switches: Array.mapWithIndex (compile slot) (padTo (layout slot))
    }

  describe :: BankSlot -> String
  describe slot =
    "Itajara " <> slotName slot <> " — switch CCs "
      <> show (switchCC slot 0) <> "-" <> show (switchCC slot (switchCount - 1))
      <> " on channel " <> show switchChannel

  padTo :: Array SwitchSpec -> Array SwitchSpec
  padTo specs = Array.take switchCount (specs <> Array.replicate switchCount blank)

  compile :: BankSlot -> Int -> SwitchSpec -> ControlBankSwitch
  compile slot i spec =
    { label: spec.label
    , longName: spec.longName
    -- Never the MC6's native toggle. A latching switch keeps state on the
    -- device, and the device is the one thing here that cannot be told it is
    -- wrong — every piece of state lives in the app, which can see the engine.
    , toToggle: false
    -- Blank switches are written blank rather than left alone, so uploading
    -- over whatever the bank held before leaves no stragglers doing something
    -- from a previous life.
    , messages: if spec.label == "" then [] else pressPair slot i <> jumps spec
    }

  pressPair :: BankSlot -> Int -> Array MC6Message
  pressPair slot i =
    [ MC6Msg.ccMessage switchChannel (switchCC slot i) 127 ActionPress
    , MC6Msg.ccMessage switchChannel (switchCC slot i) 0 ActionRelease
    ]

  jumps :: SwitchSpec -> Array MC6Message
  jumps spec =
    jumpFor ActionPress spec.tap <> jumpFor ActionLongPress spec.hold

  jumpFor :: MC6Action -> Maybe Jump -> Array MC6Message
  jumpFor action = case _ of
    Nothing -> []
    Just j -> [ MC6Msg.bankJumpMessage (target j) action ]

  target :: Jump -> Int
  target = case _ of
    ToSlot s -> cfg.base + slotIndex s
    ToBoard -> cfg.boardBank
