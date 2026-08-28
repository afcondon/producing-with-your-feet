-- | The MC6 banks for the six-loop looper, and the namespace they speak in.
-- |
-- | This is the build-time artifact of `itajara-in-atlantis` §"The MC6 is a
-- | keyboard": one bank per `BankSlot`, generated here and uploaded once — seven
-- | of them, which is as many as the CC arithmetic below has room for. Nothing
-- | rewrites
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
-- | so bank slots start at 16, 32, 48, 64, 80, 96, 112 and a switch is the offset
-- | within the block — CC 51 is slot 2, switch D, at a glance and without a
-- | table. Four CCs per block go unused, which buys that legibility cheaply.
-- | CC 0-15 are left alone because CC 0 and 32 are bank select and the low
-- | numbers are where every other device's defaults live.
-- |
-- | ## Channel 9 is the app's own
-- |
-- | Taking a whole channel for "the MC6 talking to the app about its own
-- | switches" keeps this namespace from ever colliding with a message meant for
-- | a pedal. It is a control plane, not a pedal.
-- |
-- | **Nine, because it is the only one left, and that was checked against the
-- | device rather than against a comment.** This first shipped on channel 16 on
-- | the strength of a note in `Data.Looper` saying 9, 13 and 16 were free.
-- | Thirteen had since been taken by Itajara, and sixteen is **LoopyPro** — the
-- | device's own channel table says so, and its `sendToPort` is 2034 where every
-- | other channel is 2047, three ports masked off exactly as a channel routed to
-- | the iPad and nowhere else would be.
-- |
-- | The device's table, read from the August connect dump, is the authority:
-- | 1 MC6, 2 (Brothers), 3 MOOD, 4 Clean, 5 Hedra, 7 Flint, 8 Lex, 10 Iridium,
-- | 11 Riverside, 12 Mercury7, 14 Brig, 15 Habit, 16 LoopyPro — plus 6
-- | Lost + Found and 13 Itajara, which the app knows about and the device does
-- | not name. That leaves nine, alone.
-- |
-- | Note that the March backup has Habit and LoopyPro the other way round. They
-- | were swapped at some point between, which is precisely why a channel is not
-- | something to take on the word of a comment.
-- |
-- | Note the split from `Data.Looper.looperBank`, which is a different thing
-- | wearing a similar name: that bank drives the *single-loop* transport
-- | through Itajara's pedal CCs on channel 13, and stays as long as the old
-- | Looper page does. This is the six-loop machine, and it addresses the app.
-- |
-- | ## The device says which gesture, and the value says which
-- |
-- | **Tap, double tap and long press are recognised on the MC6, not here.**
-- | Measured on the device 2026-08-21 with the gesture probe in
-- | `Data.MC6.Diagnostics`, one CC per action, read off a MIDI listener:
-- |
-- | ```
-- | double tap   DoubleTap alone — no Release at all, three times out of three
-- | double tap   DoubleTapRelease alone — Release suppressed too
-- | long press   Press at press-down, then LongPress while held, and no Release
-- | press-only   Press at press-down, Release when the foot lifts, both instant
-- | window       under 414 ms: two presses that far apart read as two singles
-- | ```
-- |
-- | Which gives a **clean, mutually exclusive triple**: `Release` fires on a
-- | single tap and on nothing else, because a double takes `DoubleTapRelease`
-- | and a hold takes `LongPress`. Nothing has to be suppressed by hand and
-- | nothing overlaps.
-- |
-- | **The deferral is on the release, never on the press.** `Press` fires the
-- | instant the foot lands, on every switch, whatever else is bound to it. It is
-- | `Release` that has to be *decided* — is this a single tap's release, the
-- | first half of a double, or the end of a hold? — and so it is `Release` that
-- | waits, and that gets suppressed when the answer turns out to be something
-- | else. This is Morningstar's own advice to program the release on any switch
-- | that also carries a long press, arrived at from underneath. See
-- | `soleGesture` for the rule it implies and for what it buys.
-- |
-- | So the app stopped doing it. There was an `Data.Looper.Gestures`, a `Mealy`
-- | transducer timing the gap between a CC at 127 and the same CC at 0, and it
-- | was well made and it is gone. Its stated justification — the double-tap
-- | window should be a function of the grid, which the device cannot know — was
-- | real and never exercised: the window was a constant. What it cost was three
-- | failure modes the hardware does not have, all of which were met in one day:
-- | the **orphan release** (a bank jump on press means the release arrives from
-- | the bank you have already reached, so the pair never completes), the
-- | **phantom hold** (that orphan's timer firing a gesture nobody made), and the
-- | app and the board **disagreeing about a threshold** — 600 ms here against
-- | 700 there, so a press in the gap closed nothing and stranded a recording.
-- |
-- | ### The value carries the gesture
-- |
-- | The CC number still says which switch on which bank. The **value** says
-- | which of the three gestures it was:
-- |
-- | ```
-- | 127  tap        ActionRelease
-- |  64  double     ActionDoubleTapRelease
-- |   1  hold       ActionLongPress
-- | ```
-- |
-- | Three widely spaced values rather than three CC blocks, because the
-- | namespace is already spent — six banks of sixteen is ninety-six CCs, and
-- | tripling that does not fit. A monitor still reads it at a glance: CC 51
-- | value 64 is a double tap on slot 2, switch D.
-- |
-- | Decoding is exact rather than banded, which makes a board programmed by an
-- | older version *say so*: its release sends 0, which is on no gesture and gets
-- | logged instead of silently meaning something.
-- |
-- | ### A switch with no second meaning still answers a double
-- |
-- | This applies only to a switch that carries a *hold* but no double — one that
-- | is therefore on the release side. The device suppresses `Release` on a
-- | double tap whether or not anything is bound to it, so left alone a fumbled
-- | double would do **nothing at all**, which is a worse answer than doing it
-- | once.
-- |
-- | So `bindings` gives such a switch the *tap's* value on
-- | `ActionDoubleTapRelease`, and its jump with it. Two taps too close together
-- | come out as one tap, which is what the player meant. The table is untouched:
-- | `Duties.double` stays `Nothing`, the screen still says the switch has no
-- | second meaning, and the fallback lives in the one function that programs the
-- | device.
-- |
-- | A switch carrying **only** a tap needs none of this, because it is on
-- | `ActionPress` and two presses are simply two presses.
module Data.Looper.Banks
  ( switchChannel
  , BankSlot(..)
  , allSlots
  , slotIndex
  , slotFromIndex
  , slotName
  , loopSwitches
  , nLoops
  , switchCC
  , Gesture(..)
  , allGestures
  , gestureName
  , gestureAction
  , gestureValue
  , gestureFromValue
  , SwitchGesture
  , switchGesture
  , decodeSwitch
  , labelOf
  , mc6OwnSwitches
  , switchLetter
  , auxLegend
  , Duty(..)
  , Subject(..)
  , Duties
  , dutiesAt
  , dutyFor
  , soleGesture
  , firesAtPressDown
  , dutyAt
  , dutyLabel
  , dutyName
  , Rung
  , ladderLine
  , levelWord
  , chanceLadder
  , stepChance
  , chanceWord
  , fadeLadder
  , stepFade
  , fadeWord
  , decayLadder
  , stepDecay
  , decayWord
  , Face
  , face
  , faceSlot
  , faceLoopKey
  , boardRows
  , loopRows
  , switchLoops
  , loopAtSwitch
  , switchForLoop
  , Jump(..)
  , sendsTo
  , banks
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Number as Number
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch, switchCount)
import Data.String (joinWith)
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..), MC6Message)
import Data.Maybe (Maybe(..), fromMaybe, maybe)

-- | The channel the MC6 uses to address the app about its own switches.
switchChannel :: Int
switchChannel = 9

-- | How far apart two banks' CC blocks sit. Sixteen rather than twelve so the
-- | block boundary falls on a round number and the switch index is the low
-- | nibble of the CC.
stride :: Int
stride = 16

-- | How many of the eight loops the **MC6** can reach.
-- |
-- | **This used to say "must equal `N_LOOPS` in the daemon", and it deliberately
-- | no longer does.** Six was the MC6's number — what it has underfoot without
-- | an FS3X — and `N_LOOPS` was set to match it. With the web page as the
-- | reference surface and the Twister as a second controller (`DESIGN-TWISTER`
-- | §1, §5) that reasoning inverts: the loop count comes from the instrument,
-- | and the foot reaches what it can.
-- |
-- | So this is a fact about the *device*, and `nLoops` is the fact about the
-- | *instrument*. Restoring the equality would give the app two ghost loops it
-- | could see and never address.
loopSwitches :: Int
loopSwitches = 6

-- | How many loops Itajara has. **Must equal `N_LOOPS` in the daemon** — that
-- | invariant moved here from `loopSwitches` rather than being dropped.
-- |
-- | Eight to match the Twister's 4×4: the top two rows of its first bank are
-- | the loops, one encoder each. Seven and eight are reachable from the page
-- | and the Twister and not from the MC6, which is not a deficiency — they are
-- | the ones you *set up* rather than stomp.
-- |
-- | The wire is unaffected: `dispatch` picks the loop from a single leading
-- | digit, so 0–7 still fits.
nLoops :: Int
nLoops = 8

-- | The banks, as roles rather than numbers. Which MC6 bank each lands on is a
-- | deployment question (`banks` takes a base); which bank a press came from is
-- | not, and that is what travels in the CC.
data BankSlot
  = LoopBank
  -- | One loop's verbs, whichever loop you came from. See `own LoopPage`.
  | LoopPage
  | ConfigBank
  | QuantiseBank
  | SpeedBank
  | ModesBank
  | PanBank

derive instance Eq BankSlot
derive instance Ord BankSlot

allSlots :: Array BankSlot
allSlots =
  [ LoopBank, LoopPage, ConfigBank, QuantiseBank, SpeedBank, ModesBank, PanBank ]

-- | **Seven is the last one that fits.** The CC block is `16 * (index + 1)`, so
-- | Pan sits at 112 and its twelfth switch is 123 — four short of the 127 a
-- | seven-bit value stops at. An eighth bank would put switches above 128,
-- | where they stop being data and start being status bytes, and the frame
-- | carrying them would truncate. The byte-range test in `test/Main` is what
-- | would catch it, and it would catch it as a wall rather than as a warning.
slotIndex :: BankSlot -> Int
slotIndex = case _ of
  LoopBank -> 0
  LoopPage -> 1
  ConfigBank -> 2
  QuantiseBank -> 3
  SpeedBank -> 4
  ModesBank -> 5
  PanBank -> 6

slotFromIndex :: Int -> Maybe BankSlot
slotFromIndex = case _ of
  0 -> Just LoopBank
  1 -> Just LoopPage
  2 -> Just ConfigBank
  3 -> Just QuantiseBank
  4 -> Just SpeedBank
  5 -> Just ModesBank
  6 -> Just PanBank
  _ -> Nothing

-- | The bank's name on the device's screen. Eight characters, like a label.
slotName :: BankSlot -> String
slotName = case _ of
  LoopBank -> "Loops"
  -- Not "Loop 3". The device cannot be relabelled fast enough to track which
  -- loop is in hand — an upload is well over a second — so the pedal names the
  -- *page* and the computer names the loop. The standing division of labour.
  LoopPage -> "The Loop"
  ConfigBank -> "Loop Cfg"
  QuantiseBank -> "Quantise"
  SpeedBank -> "Speed"
  ModesBank -> "Modes"
  PanBank -> "Pan"

slotId :: BankSlot -> String
slotId = case _ of
  LoopBank -> "loops"
  LoopPage -> "loop"
  ConfigBank -> "config"
  QuantiseBank -> "quantise"
  SpeedBank -> "speed"
  ModesBank -> "modes"
  PanBank -> "pan"

-- | Which CC a given switch sends.
switchCC :: BankSlot -> Int -> Int
switchCC slot i = stride * (slotIndex slot + 1) + i

-- | The three gestures, told apart by the device rather than by the app.
-- |
-- | A closed set, and closed for a hardware reason: these are the three the MC6
-- | resolves without overlap. It has more actions than this — `LongDoubleTap`,
-- | the scroll pair, the release halves of each — and every one of them is
-- | another thing to remember while standing on an unmarked switch. Three is
-- | already more than most switches use.
data Gesture = Tap | Double | Hold

derive instance Eq Gesture
derive instance Ord Gesture

allGestures :: Array Gesture
allGestures = [ Tap, Double, Hold ]

gestureName :: Gesture -> String
gestureName = case _ of
  Tap -> "tap"
  Double -> "double tap"
  Hold -> "long press"

-- | Which of the device's actions carries each gesture.
-- |
-- | **The measured triple**, and the reason this module can stop timing
-- | anything. `Release` fires on a single tap and on nothing else; a double
-- | takes `DoubleTapRelease` and suppresses the release; a long press takes
-- | `LongPress` and sends no release at all.
-- |
-- | The release halves rather than `Press` and `DoubleTap`, because the device
-- | is already deferring — it cannot know a tap is a tap until the window
-- | expires — so the earlier action buys nothing and, on a hold, would fire
-- | before the gesture had happened.
gestureAction :: Gesture -> MC6Action
gestureAction = case _ of
  Tap -> ActionRelease
  Double -> ActionDoubleTapRelease
  Hold -> ActionLongPress

-- | The CC value that says which gesture it was. See the module header.
gestureValue :: Gesture -> Int
gestureValue = case _ of
  Tap -> 127
  Double -> 64
  Hold -> 1

-- | Exact, so that a board programmed by an older version is *noticed* rather
-- | than half-understood: its release sends 0, which is on no gesture.
gestureFromValue :: Int -> Maybe Gesture
gestureFromValue = case _ of
  127 -> Just Tap
  64 -> Just Double
  1 -> Just Hold
  _ -> Nothing

-- | A recognised gesture on a known switch, as the app reads it off the wire.
-- |
-- | One message, complete. There is no half of this: the device does not tell
-- | the app a switch went down, so there is nothing to hold onto between
-- | messages and nothing that can be left holding it.
type SwitchGesture =
  { slot :: BankSlot
  , switch :: Int
  , gesture :: Gesture
  }

switchGesture :: BankSlot -> Int -> Gesture -> SwitchGesture
switchGesture slot switch gesture = { slot, switch, gesture }

-- | Read a CC as a gesture on a switch, or refuse it.
-- |
-- | Total and cheap, so the router can offer it every incoming CC and let the
-- | `Nothing` mean "not ours" — which is what keeps this namespace from having
-- | to be checked for at the call site as well as here.
decodeSwitch :: Int -> Int -> Int -> Maybe SwitchGesture
decodeSwitch channel ccNum value =
  if channel /= switchChannel || ccNum < stride then Nothing
  else do
    slot <- slotFromIndex (ccNum `div` stride - 1)
    let sw = ccNum `mod` stride
    -- The four CCs above each block are the gap that makes the arithmetic
    -- legible; nothing sends them, so nothing should accept them either.
    if sw >= switchCount then Nothing
      else do
        gesture <- gestureFromValue value
        Just { slot, switch: sw, gesture }

-- | What a switch is labelled, for logging a press the app was not expecting.
-- |
-- | Reads the same table the device was programmed from, so a surprise can be
-- | reported as "Loop 3, hold to set up" rather than as a CC number — and if
-- | the two ever disagree, the disagreement is between the device and this
-- | table rather than between two copies of the table.
labelOf :: BankSlot -> Int -> Maybe String
labelOf slot i = map dutyName (dutyAt slot i)

-- | How many switches the MC6 has of its own, and so where the aux ones start.
-- |
-- | Numerically the same as `loopSwitches`, and not the same fact: one is a
-- | property of the unit, the other a choice about how many loops to run. They
-- | are written separately so that changing one does not silently change the
-- | other.
mc6OwnSwitches :: Int
mc6OwnSwitches = 6

-- | The letter printed on the board for a switch index.
switchLetter :: Int -> Maybe String
switchLetter = Array.index
  [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L" ]

-- | The switches past the MC6's own six, as letter and label, for a given bank.
-- |
-- | **The display's only honest source for these.** The MC6's LCD names its own
-- | six switches and stops there; G to L are FS3X footswitches with no display
-- | and no markings, so the app has to say what they do — and it has to say it
-- | *for the bank the board is actually showing*.
-- |
-- | That last part was learned the hard way. The legend was a hand-written copy
-- | of the loop bank's six, shown unconditionally, so with the board on the
-- | config bank the screen said J was Clear while J was End Stop. Pressing it
-- | reported something about leaving-state, which reads exactly like a switch
-- | wired to the wrong place — and cost an hour looking for a reversed mapping
-- | that was never reversed.
-- |
-- | Derived from `layout` rather than restated, because a legend that can
-- | disagree with the table the device was programmed from is a legend that
-- | eventually will.
auxLegend :: BankSlot -> Array { key :: String, what :: String }
auxLegend = map (\e -> { key: e.key, what: e.what }) <<< auxLegendAt

auxLegendAt :: BankSlot -> Array { index :: Int, key :: String, what :: String }
auxLegendAt slot = Array.catMaybes (map entry (Array.range mc6OwnSwitches (switchCount - 1)))
  where
  entry i = do
    key <- switchLetter i
    d <- dutyAt slot i
    if d == Nothing_ then Nothing else Just { index: i, key, what: dutyLabel d }

-- | The MC6's own six switches, in the rows they physically occupy.
-- |
-- | **The device numbers from the bottom**: A B C is the near row, under your
-- | toes, and D E F the far one. A view that lays out six things in board order
-- | must use this rather than index order, and it lives here because it is a
-- | fact about the hardware, not about any one screen — the loop grid got it
-- | wrong on its own, and the next screen to draw six switches would have got
-- | it wrong again, differently.
boardRows :: Array (Array Int)
boardRows = [ [ 3, 4, 5 ], [ 0, 1, 2 ] ]

-- | **The loops, four across and two down, on every surface at once.**
-- |
-- | This is the harmonisation, decided 2026-08-25, and it is a reversal worth
-- | stating plainly: the loop order used to be the *MC6's*, because the MC6 was
-- | once the only way to reach a loop and its switches number from the bottom.
-- | So loop 1 was switch A, on the near row, and the screen drew A B C below
-- | D E F to match.
-- |
-- | With eight loops and a 4×4 controller that tail is wagging the dog. The
-- | Twister's top-left encoder is loop 1 because that is what a grid of eight
-- | things obviously means, the page draws the same two rows of four, and the
-- | **pedal is the surface that has to fit in** — it covers the left three
-- | columns of both rows and simply lacks the fourth.
-- |
-- | Which is the shape an MC8 would fill exactly: four across, two down, loops
-- | 4 and 8 in the column the MC6 does not have. The layout is already waiting
-- | for it.
loopRows :: Array (Array Int)
loopRows = [ [ 0, 1, 2, 3 ], [ 4, 5, 6, 7 ] ]

-- | Which loop each of the MC6's six switches selects, by switch index.
-- |
-- | **The device numbers from the bottom** — A B C is the near row, under your
-- | toes, and D E F the far one — so the far row holds the *top* row of
-- | `loopRows` and the near row holds the bottom. Read it as the grid with its
-- | right-hand column removed and its rows swapped, because that is exactly
-- | what it is.
-- |
-- | ```
-- |   the page and the Twister      the MC6
-- |     1  2  3  4                    D  E  F      (far row)
-- |     5  6  7  8                    A  B  C      (near row)
-- | ```
-- |
-- | One table, both directions, so a switch and the letter printed beside a
-- | loop on screen cannot come to disagree. **Changing this needs the MC6
-- | re-uploaded**: the labels are compiled from it.
switchLoops :: Array Int
switchLoops = [ 4, 5, 6, 0, 1, 2 ]

loopAtSwitch :: Int -> Maybe Int
loopAtSwitch = Array.index switchLoops

-- | Which switch selects a loop, or `Nothing` for the two the pedal cannot
-- | reach.
switchForLoop :: Int -> Maybe Int
switchForLoop l = Array.findIndex (_ == l) switchLoops

-- | One switch, as a view is allowed to see it.
-- |
-- | Opaque, and that is the whole point: **a view cannot make one up.** Three
-- | times in one day a screen restated something this module already knew — the
-- | six aux labels, the A-to-F letters, the physical row order — and each time
-- | the copy was right when written and wrong later. Types cannot stop somebody
-- | typing a word into a div, but they can stop a second *table* existing, and
-- | a second table is what actually rots.
-- | **The `Switch` view type was removed 2026-08-27**, with `switchKey`,
-- | `switchLabel`, `switchDouble`, `switchHold`, `faceAux` and `faceName`.
-- |
-- | They existed for one caller: the row under the loops that named G to L,
-- | because those are FS3X switches with no markings and nothing else could
-- | say. The board panel says it now, from `dutiesAt` and `dutyLabel` — the
-- | same facts by a shorter road, and pressable, which the legend never was.
-- |
-- | Left as a note rather than silently: `auxLegend` above is the surviving
-- | answer to the same question and is what four tests hold to.

-- | What the board is showing, as everything a view may say about it.
-- |
-- | `Nothing` is a real state and not a missing value: leaving for the board
-- | bank leaves the looper family entirely, and a face that kept naming the
-- | loop bank's switches there would be describing a board nobody is standing
-- | on.
newtype Face = Face (Maybe BankSlot)

face :: Maybe BankSlot -> Face
face = Face

-- | Which bank the face is, when it is one of ours.
-- |
-- | The one way out of the newtype, and deliberately so. A view that wants to
-- | draw the whole board needs the slot; a view that only wants to label a loop
-- | asks `faceLoopKey` and never opens it at all.
faceSlot :: Face -> Maybe BankSlot
faceSlot (Face m) = m

-- | What to print on a loop's slot: the switch that reaches it, or its number
-- | when the board is somewhere that cannot reach it.
-- |
-- | The letters are only true on the loop bank. With the board on config, A is
-- | Quantise — so labelling the first loop "A" there points a foot at the wrong
-- | thing, which is the same fault the legend had.
-- | What to print on a loop: the letter of the switch that selects it when the
-- | board is showing the loop bank, its own number otherwise.
-- |
-- | **Through `switchForLoop`, not through the loop's index.** Those were the
-- | same number until the surfaces were harmonised and are not any more: loop 1
-- | is switch D. Reading the letter straight off the index would print "A"
-- | beside loop 1 and send the foot to the wrong corner of the pedal — and the
-- | two would still agree perfectly on loop 5, which is the kind of half-right
-- | that survives testing.
-- |
-- | Loops with no switch have no letter; their number is the honest answer.
faceLoopKey :: Face -> Int -> String
faceLoopKey (Face m) i = case m of
  Just LoopBank -> fromMaybe (show (i + 1)) (switchForLoop i >>= switchLetter)
  _ -> show (i + 1)

data Jump = ToSlot BankSlot | ToBoard

derive instance Eq Jump

-- | What a switch is *for*.
-- |
-- | **The one table.** Three things have to agree about every switch: the eight
-- | characters the MC6 prints on its screen, the words the app shows for the
-- | six that have no screen, and the command the press actually sends. They
-- | used to be two tables keyed by a switch index — the layout said switch 9
-- | was "Clear", `Data.Looper.Machine` said switch 9 sent `c`, and nothing
-- | linked them but the number. A layout edit that moved Clear would have left
-- | a switch labelled one thing and doing another, and nothing would have
-- | failed to compile.
-- |
-- | So a switch carries a value rather than a string, and the label, the long
-- | name and the meaning are all *functions of that value*. Relabelling and
-- | rewiring stop being separate acts. It is the same move as `Emit` in
-- | `Data.MC6.Model`: closed alternatives are an ADT, never a string.
data Duty
  -- | One of the six loops. The index is the loop, not the switch, because on
  -- | the loop bank they coincide and everywhere else they must not.
  -- |
  -- | **A place, not a verb.** This used to be seven verbs in a trenchcoat: a
  -- | tap meant record, or close, or overdub, or cancel an arm, or fire, or
  -- | stop, or start, depending on what the daemon last reported — and nothing
  -- | underfoot said which. Now it selects the loop and opens its page, where
  -- | each of those has its own switch with its own name on the screen.
  -- |
  -- | The cost is a second press to start a take on a loop you are not already
  -- | standing on. It buys back more than it costs: the switch carries one
  -- | gesture, so it is on `ActionPress` and reports the instant your foot
  -- | lands, where before every loop press waited out the double-tap window.
  = SelectLoop Int
  -- | Start writing, stop writing, or take back a wait — whichever the loop is
  -- | ready for.
  -- |
  -- | **Still context-dependent, and honestly so.** `r` is one command in the
  -- | daemon and it means "toggle the write head": it opens a first recording,
  -- | closes one, opens and closes an overdub, and cancels a loop that is
  -- | listening. Splitting that across four switches would be splitting a thing
  -- | the engine does not split.
  | RecordLoop
  -- | Add a pass to what is already there, bringing a stopped loop back first.
  -- |
  -- | Overdubbing something you cannot hear is a way to record a mistake twice,
  -- | which is why this unmutes rather than refusing.
  | OverdubLoop
  -- | Stop it, or start it again — and fire it, if it is a one-shot.
  -- |
  -- | The one-shot case is not an overload sneaking back in. A one-shot is
  -- | silent between passes *by definition*, so it has no playing and stopped to
  -- | move between; firing is the only thing this switch could mean there.
  | Transport
  -- | Wait for a sound instead of starting on the press.
  -- |
  -- | `lev1` and then `r`, which is the mode plus the gesture in one press —
  -- | because "start when I play" is something you decide in the moment, not
  -- | something you go to a config bank to arrange. The mode stays visible in
  -- | Modes and on screen; this is the shortcut, not a second source of truth.
  | ArmLoop
  -- | Deeper into the family. Labelled with the destination's own name.
  | Enter BankSlot
  -- | Up, or out. Labelled with where it goes, so "< Config" and "< Board"
  -- | cannot drift from the jump the device was programmed with.
  | Back Jump
  | StopAll
  | Undo
  | ClearLoop
  | SaveTake
  | ClickToggle
  | Reverse
  | Pendulum
  -- | One pass per trigger, rather than turning for ever.
  -- |
  -- | **The one gesture that moves a loop's zero.** Everywhere else in this rig
  -- | a loop's position is fixed at the moment it was recorded — that is what
  -- | phase-locking means, and it is why stopping a loop and starting it again
  -- | puts it back where it would have been rather than where it began. A
  -- | one-shot has to start from the top, so firing it moves `origin`, and a
  -- | loop that can be fired has given up its place in the phase-locked set.
  -- |
  -- | Which is precisely why it is a mode and not a gesture: losing your grid
  -- | should be something you switch on, never something a footswitch does to
  -- | you on a bank you did not mean to be standing on.
  | OneShot
  -- | Wait for a sound instead of starting on the press.
  -- |
  -- | Free, because the pre-roll ring is already running: the recording begins
  -- | fifty milliseconds *before* the threshold was crossed, so the attack that
  -- | crossed it is in the take rather than clipped off the front of it. The
  -- | same trick as claiming the past and as un-doing gesture latency — the
  -- | third thing the ring has paid for.
  | LevelArm
  -- | Step this loop's chance of sounding down the ladder, wrapping at the end.
  -- |
  -- | **A value on one switch, which is what the config family kept wanting.**
  -- | Chance had a bank of five to itself before it worked at all. It is a value
  -- | chosen from a few, like speed and pan, so five switches is the shape it
  -- | *looks* like it wants — but a Chance bank reached from the Modes bank
  -- | reached from Loop Cfg is four deep, and three was already one too many.
  -- |
  -- | So it steps instead, and the step is computed here from what the engine
  -- | last reported rather than counted on the device. That is the difference
  -- | between this and the MC6's own scroll counters: the device would keep its
  -- | own position, and a device that keeps state is the one thing here that
  -- | cannot be told it is wrong. The app is looking at the engine thirty times
  -- | a second.
  -- |
  -- | The pedal says "Chance" and the screen says which rung — the standing
  -- | division of labour, and the reason the board can be programmed once.
  | StepChance
  -- | Step how much of the wrap is crossfaded with what followed it.
  -- |
  -- | The other half of *store everything, flatten late*: the frames recorded
  -- | after the loop closed were kept rather than trimmed, and this is what they
  -- | were kept for. A first recording is cut, so the frame after the last one
  -- | is not the frame that followed it when it was played — the join is a step
  -- | in the waveform. Arriving at the head through the continuation makes it
  -- | continuous, because the two are one performance either side of one
  -- | instant.
  -- |
  -- | Applied at playback, so it costs nothing to change and nothing to undo.
  | StepFade
  -- | Step how much a pass costs the material already there.
  -- |
  -- | **The parameter that separates Frippertronics from song looping.** Two
  -- | Revoxes with the second one feeding back below unity is this number, and
  -- | so is what a tape echo does to its repeats. Without it every layer plays
  -- | at full for ever and the only shape a loop can have is the one it was
  -- | given.
  -- |
  -- | Per layer, counted from each one's own birth, so new material enters at
  -- | full while everything underneath recedes — which a single feedback gain
  -- | cannot do, because it destroys as it goes and has no idea how old
  -- | anything is. Here it is a resolution at playback: a loop faded to nothing
  -- | is still all there, and turning decay off brings it back.
  | StepDecay
  -- | Claim the recent past. **The one thing a pedal cannot do**, and the
  -- | reason for a sixty-second ring: you played something good and did not
  -- | hit record, so hit it afterwards. It had no footswitch at all until
  -- | now, while the switch labelled "Take" saved a WAV — two different takes,
  -- | and the wrong one had the fast slot.
  | ClaimPast
  -- | Put back the last undone layer. Free now that undo keeps what it
  -- | removes.
  | Redo
  -- | Bring every loop back, the counterpart of stopping them all.
  | StartAll
  | ClearAll
  | Free
  -- | Quantised launch. The bar count is carried and does not yet do anything —
  -- | the engine's grid is the anchor loop's cycle, not a bar — so this is a
  -- | promise the meaning table has to keep honestly.
  | Grid Int
  | Rate Number
  | Place Int

  -- ## The verbs the CC table had and this one did not
  --
  -- Until 2026-08-25 there were two vocabularies: this one, reached by foot,
  -- and the CC table in `Data.Looper` reached by the page. `Multiply` lived
  -- only in the second — the MC6 loop family has never had a switch for it —
  -- so the *reference* surface could ask for something the machine had no word
  -- for. Folding the page onto the machine (`DESIGN-TWISTER` §4) means this
  -- type gains everything the CC table could express.

  -- | Extend by whole cycles while it runs, and close on the next press. Asks
  -- | "how many bars of this?" where `SpreadLoop` asks "how often?".
  | MultiplyLoop
  -- | The layer keeps its length and the loop grows around it, so the pass
  -- | sounds one cycle in `n`. Structural, instant and reversible; it records
  -- | nothing.
  | SpreadLoop Int
  -- | Move a spread layer one slot later in its cycle.
  | RotateLoop
  -- | Sound every cycle again — the way back from `SpreadLoop`, and the reason
  -- | spreading is safe to try mid-take.
  | DenseLoop
  -- | Let go of the length that `Undo` deliberately kept. The third of the
  -- | three erasures, and the one that is a between-takes decision.
  | ForgetLength
  -- | **Take the session tempo from this loop.**
  -- |
  -- | The other half of `SetBars`. That duty has three jobs and the ack says
  -- | which one you got — size an empty loop, declare the bar count of a
  -- | clockless anchor, resize something with material in it — and *declaring*
  -- | was reachable only with no clock, because with one there was nothing to
  -- | tell. There is now: link-spike answers `/link/set-tempo`.
  -- |
  -- | **The only duty in this vocabulary whose effect leaves the rig.**
  -- | Everything else here addresses a loop or the daemon; this one reaches
  -- | Ableton, purerl-tidal and the modular's tempo-relative rates, because
  -- | Link is a session rather than a clock we read. That is a reason for it to
  -- | be a deliberate press rather than a knob, not a reason to leave the click
  -- | fighting what you played.
  | TakeTempo
  -- | Input monitoring. Global in the engine, like the click.
  | MonitorToggle

  -- ## Value duties
  --
  -- The same parameters as `StepChance`, `StepFade` and `StepDecay`, carrying a
  -- value rather than a direction. **The step duties are defined in terms of
  -- these** (`Data.Looper.Machine`): a ladder is a *rendering* of a parameter
  -- for a surface that can only press, and the value is the parameter. One
  -- place where chance becomes a command means a footswitch and a knob cannot
  -- disagree — the argument `Data.Looper.Verb` makes about spellings, one level
  -- up.
  --
  -- `Rate` and `Place` above are the same family; they got here first, put
  -- there by the Speed and Pan banks.

  -- | The two global flags, as values rather than flips.
  -- |
  -- | `ClickToggle` and `MonitorToggle` above are defined in terms of these, the
  -- | same way the `Step*` family is defined in terms of `Chance` and friends. A
  -- | footswitch programmed as an MC6 *native toggle* sends 127 and 0 on
  -- | alternate presses and must **set** from that value — flipping there would
  -- | flip twice per press — while a momentary switch has no value to carry and
  -- | must ask what the current one is. Both, one meaning.
  | Click Boolean
  | Monitor Boolean

  -- | Whether this loop waits for the grid, as a value — and the flip that a
  -- | surface with no value to send needs.
  -- |
  -- | **`Grid n` and `Free` are the MC6's rendering of these**, and delegate to
  -- | them the way `StepChance` delegates to `Chance`. They had to be: a control
  -- | whose press always sets *on* cannot be pressed twice, which is fine on a
  -- | bank where `Free` sits beside it with a switch of its own and wrong
  -- | anywhere else. The Twister found it — one encoder for a flag means the
  -- | encoder has to flip.
  | OnGrid Boolean
  | GridToggle

  -- | Make this loop a tape, or stop being one. The flip, for a control with
  -- | no value to send; `Revox` is the form with.
  | RevoxToggle
  | Revox Boolean
  -- | Thread an empty tape of this many seconds.
  | Blank Number
  -- | What a Revox pass leaves of what was under it, in decibels.
  | Feedback Number
  -- | How much top a Revox pass keeps, in hertz.
  | Tone Number
  -- | How many layers should be live — the undo stack as a **position** rather
  -- | than as two buttons.
  -- |
  -- | Undo and Redo are one axis and had two controls, which on a device whose
  -- | encoders report an absolute position is a waste of the thing it is good
  -- | at. `perform` compares this with what the daemon reports and sends the
  -- | difference as `u`s or `y`s, so the knob is a scrub through the stack and
  -- | the ring shows how deep you are.
  -- |
  -- | Nudge-proof by arithmetic rather than by luck: eight layers across 128
  -- | steps is sixteen steps a layer, and the press guard only has to cover two.
  | Layers Int
  -- | The rig's level-arm threshold, in decibels. Not per loop.
  | ArmLevel Number
  -- | This loop's level, in decibels. Zero is unity, -60 is silence.
  | Level Number
  -- | How often a pass sounds, as a probability. `1.0` is always.
  | Chance Number
  -- | How much of the wrap is crossfaded, in milliseconds. Zero is a hard join.
  | Fade Number
  -- | How much a pass costs what is already there, in decibels. Zero holds.
  | Decay Number

  -- | **How many bars this loop is**, and the only place metre reaches a loop.
  -- |
  -- | One duty doing three jobs, decided by what the loop already is and said
  -- | out loud in the ack every time:
  -- |
  -- | * **empty** — sizes it, and the next recording closes itself there
  -- |   instead of waiting for a second press;
  -- | * **the first loop with no clock** — *declares* what you played. `4` on a
  -- |   four-bar phrase makes the bar a quarter of it and touches no audio,
  -- |   which is the only way a clockless session gets a loop shorter than its
  -- |   first take;
  -- | * **anything else with material in it** — resizes, and the layers keep
  -- |   their own lengths inside the new one.
  -- |
  -- | The three are one control because they are one question — *how many bars
  -- | is this* — asked of a loop in three states. Splitting them would make the
  -- | player decide which verb they meant, which is a decision about the engine
  -- | rather than about the music.
  | SetBars Int
  -- | How often the newest layer sounds, in cycles of its own length. `1` is
  -- | every time round, which is what `Dense` asks for by another name.
  | Every Int
  -- | Which of those cycles it lands on. One-based, and wraps.
  | PlaceAt Int
  -- | What a launch waits for, in beats. **Rig-wide**, like the click and the
  -- | arm threshold — `-1` is a bar and is the default, `0` is none.
  -- |
  -- | Beats rather than fractions of a bar so it means the same thing in 3/4 as
  -- | in 4/4: a quantum of three does not turn "one beat" into a third of a
  -- | bar, it stays a beat.
  | Launch Int
  -- | Named, unimplemented, and still occupying its switch. Carries what it
  -- | would be called and what it is waiting for, so a press answers with the
  -- | reason rather than with silence.
  | NotYet String String
  | Nothing_

derive instance Eq Duty

-- | Which loop a duty is about.
-- |
-- | **An argument to `Data.Looper.Machine.perform`, not a field of `Duty`**, and
-- | that is the whole point of it: with the subject explicit there is no way to
-- | send a per-loop verb without having said which loop, because the compiler
-- | asks. The bug it kills is a class rather than an instance — `SaveTake`
-- | wrote loop 1 whatever the board was focused on for as long as the CC table
-- | rendered bare, and every other per-loop verb on that table had the same
-- | fault waiting.
-- |
-- | The MC6 always passes `Focused`: six switches cannot name eight loops in a
-- | parameter gesture. The Twister passes `OnLoop i` for its per-loop encoders,
-- | because there every loop has its own knob and turning one must not steal
-- | focus from another.
data Subject
  = Focused
  | OnLoop Int

derive instance Eq Subject

-- | The eight characters the MC6 prints. Refused rather than truncated by
-- | `Data.MC6.Model.shortName` downstream, so a label that will not fit is a
-- | build-time problem and not a mystery on the device.
dutyLabel :: Duty -> String
dutyLabel = case _ of
  SelectLoop i -> "Loop " <> show (i + 1)
  RecordLoop -> "Record"
  OverdubLoop -> "Overdub"
  Transport -> "Stop/Go"
  ArmLoop -> "Arm"
  Enter slot -> slotName slot
  Back (ToSlot slot) -> "< " <> shortSlot slot
  Back ToBoard -> "< Board"
  StopAll -> "Stop All"
  StartAll -> "Start All"
  Undo -> "Undo"
  Redo -> "Redo"
  ClearLoop -> "Clear"
  ClearAll -> "Clear All"
  ClaimPast -> "Capture"
  SaveTake -> "Save"
  ClickToggle -> "Click"
  Reverse -> "Reverse"
  Pendulum -> "Pendulum"
  OneShot -> "One Shot"
  LevelArm -> "Listen"
  StepChance -> "Chance"
  StepFade -> "Fade"
  StepDecay -> "Decay"
  Free -> "Free"
  Grid n -> show n <> (if n == 1 then " Bar" else " Bars")
  Rate r -> "x " <> rateWord r
  Place p -> placeWord p
  MultiplyLoop -> "Multiply"
  SetBars _ -> "Bars"
  Every _ -> "Every"
  PlaceAt _ -> "On"
  Launch _ -> "Launch"
  SpreadLoop _ -> "Spread"
  RotateLoop -> "Shift"
  DenseLoop -> "Dense"
  ForgetLength -> "Length"
  TakeTempo -> "Tempo"
  MonitorToggle -> "Monitor"
  -- The value goes in `dutyName`, not here. Eight characters cannot hold
  -- "Chance 3 in 4", and these three never reach an MC6 switch anyway — they
  -- are what the knobs and the page send.
  Click _ -> "Click"
  Monitor _ -> "Monitor"
  OnGrid _ -> "Grid"
  GridToggle -> "Grid"
  RevoxToggle -> "Revox"
  Revox _ -> "Revox"
  Blank _ -> "Tape"
  Feedback _ -> "Feedback"
  Tone _ -> "Tone"
  Layers _ -> "Layers"
  ArmLevel _ -> "Listen at"
  Level _ -> "Level"
  Chance _ -> "Chance"
  Fade _ -> "Fade"
  Decay _ -> "Decay"
  NotYet l _ -> l
  Nothing_ -> ""

-- | Twenty-four characters, for the device's long name and for reporting a
-- | press the app did not expect as words rather than as a CC number.
dutyName :: Duty -> String
dutyName = case _ of
  SelectLoop i -> "Loop " <> show (i + 1)
  RecordLoop -> "Record, or close what is"
  OverdubLoop -> "One more pass over it"
  Transport -> "Stop it, or set it going"
  ArmLoop -> "Start on the next note"
  Enter ConfigBank -> "Set up this loop"
  Enter slot -> "Set " <> slotName slot
  Back (ToSlot slot) -> "Back to " <> slotName slot
  Back ToBoard -> "Leave the looper"
  StopAll -> "Stop every loop"
  StartAll -> "Start every loop"
  Undo -> "Undo the last layer"
  Redo -> "Put the layer back"
  ClearLoop -> "Clear the chosen loop"
  ClearAll -> "Clear every loop"
  ClaimPast -> "Claim what just happened"
  SaveTake -> "Save the take to disk"
  ClickToggle -> "Click on or off"
  Reverse -> "Play the loop backwards"
  Pendulum -> "Forward, then back"
  OneShot -> "One pass, then silence"
  LevelArm -> "Start when you play"
  StepChance -> ladderLine chanceLadder
  StepFade -> ladderLine fadeLadder
  StepDecay -> ladderLine decayLadder
  Free -> "Free length and launch"
  Grid n -> "Round to " <> show n <> (if n == 1 then " bar" else " bars")
  Rate r -> rateWord r <> " speed"
  Place p -> placeWord p <> " in the field"
  MultiplyLoop -> "Extend by whole cycles"
  SetBars n -> show n <> (if n == 1 then " bar long" else " bars long")
  Every n -> if n == 1 then "Sounds every time round"
             else "Sounds once every " <> show n
  PlaceAt n -> "On slot " <> show n
  Launch n -> case n of
    -1 -> "Launch on the bar"
    0 -> "Launch straight away"
    b -> "Launch on " <> show b <> (if b == 1 then " beat" else " beats")
  SpreadLoop n -> "Sound one cycle in " <> show n
  RotateLoop -> "Move it one slot later"
  DenseLoop -> "Sound every cycle again"
  ForgetLength -> "Let go of the length"
  TakeTempo -> "Take the tempo from this loop"
  MonitorToggle -> "Input monitoring"
  Click on -> "Click " <> onOff on
  Monitor on -> "Monitoring " <> onOff on
  OnGrid on -> if on then "Waits for the grid" else "Free length and launch"
  GridToggle -> "Wait for the grid, or not"
  RevoxToggle -> "Tape mode, or layers"
  Revox on -> if on then "A tape: undo is gone" else "Record in layers again"
  Blank secs -> "Thread " <> show (Int.round secs) <> " s of tape"
  Feedback db -> "A pass leaves " <> levelWord db
  Tone hz -> if hz >= 20000.0 then "Every pass as bright"
             else "Keeps " <> show (Int.round (hz / 100.0) * 100) <> " Hz"
  Layers n -> "Keep " <> show n <> (if n == 1 then " layer" else " layers")
  ArmLevel db -> "Starts at " <> show (Int.round db) <> " dBFS"
  Level db -> "Plays at " <> levelWord db
  Chance p -> "Sounds " <> chanceWord p
  Fade ms -> "Wraps " <> fadeWord ms
  Decay db -> "Decays " <> decayWord db
  NotYet l _ -> l
  Nothing_ -> ""

-- | A level in words. The daemon's own vocabulary — "full" and "silent" rather
-- | than "0.0 dB" and "-60.0 dB", because those are things a meter says and not
-- | things a person does.
levelWord :: Number -> String
levelWord db
  | db >= 0.0 = "full"
  | db <= -60.0 = "silent"
  | otherwise = show (Int.round db) <> " dB"

-- | For the duties whose whole content is a boolean.
onOff :: Boolean -> String
onOff on = if on then "on" else "off"

-- | A value a switch can step to, and what to call it.
-- |
-- | **The word lives beside the value**, rather than in a second function keyed
-- | by it. Three things read a ladder: the step a press takes, what the screen
-- | says, and — through the step — what the engine is told. Keeping them in one
-- | table is the same move as `Duty` itself.
-- | `word` is what the screen says; `tick` is the same thing squeezed small
-- | enough that the whole ladder fits in the twenty-four characters the pedal
-- | flashes on a press.
type Rung = { value :: Number, word :: String, tick :: String }

-- | The whole ladder on one line, for the pedal's long name.
-- |
-- | **A stepper cannot say where it is, so it should at least say where it can
-- | go.** The MC6 has one static line per switch and no way to update it from
-- | the device, so a stepper's long name was a description — "How often it
-- | plays" — which is the one thing the player already knows from the label
-- | underfoot. Listing the rungs at least tells you what the presses will do
-- | and in what order, which is what you want when your hands are busy; where
-- | you *are* on it is the computer's job.
ladderLine :: Array Rung -> String
ladderLine = joinWith " " <<< map _.tick

-- | The next rung the switch walks to, wrapping at the end.
-- |
-- | **One rule for every ladder**, rather than a step function per parameter:
-- | *the rung after the one you are standing on, and back to the first if there
-- | is none.* A value that is on no rung — only reachable by typing at the
-- | daemon — also goes to the first, because guessing which rung a number
-- | nobody chose is nearest to is a guess the player would have to learn.
-- |
-- | The wrap matters more than it looks: a ladder you cannot get off is worse
-- | than one that takes five presses, and five presses on a switch you are
-- | already standing over is nothing.
nextRung :: Array Rung -> Number -> Number
nextRung rungs now = case Array.findIndex (\r -> onRung r.value now) rungs of
  Just i -> maybe first _.value (Array.index rungs (i + 1))
  Nothing -> first
  where
  first = maybe 0.0 _.value (Array.head rungs)

-- | This ladder's own word for a value, when it has one.
rungWord :: Array Rung -> Number -> Maybe String
rungWord rungs v = _.word <$> Array.find (\r -> onRung r.value v) rungs

-- | How close counts as being on a rung. Wide enough to survive a round trip
-- | through the wire as text, narrow enough that no two rungs could claim the
-- | same reading.
onRung :: Number -> Number -> Boolean
onRung a b = Number.abs (a - b) < 1.0e-4

-- | How often a pass sounds. Rarest last, so stepping makes it rarer.
-- |
-- | The engine takes any probability from zero to one and has no opinion about
-- | which are worth a press. That is a question about feet, and this is where
-- | feet are answered.
chanceLadder :: Array Rung
chanceLadder =
  [ { value: 1.0, word: "always", tick: "all" }
  , { value: 0.75, word: "3 in 4", tick: "3:4" }
  , { value: 0.5, word: "1 in 2", tick: "1:2" }
  , { value: 0.25, word: "1 in 4", tick: "1:4" }
  , { value: 0.125, word: "1 in 8", tick: "1:8" }
  ]

stepChance :: Number -> Number
stepChance = nextRung chanceLadder

chanceWord :: Number -> String
chanceWord p = fromMaybe (show (Int.round (p * 100.0)) <> "%") (rungWord chanceLadder p)

-- | How much of the wrap is crossfaded with what followed it, in milliseconds.
-- |
-- | **Off first, and off by default.** A fade changes the first few milliseconds
-- | of every cycle — it has to, that is what makes the join continuous — and a
-- | looper that quietly softened every downbeat would be doing something nobody
-- | asked for. Ten is under a drum transient; a hundred is a real dissolve.
fadeLadder :: Array Rung
fadeLadder =
  [ { value: 0.0, word: "hard", tick: "hard" }
  , { value: 10.0, word: "10 ms", tick: "10" }
  , { value: 25.0, word: "25 ms", tick: "25" }
  , { value: 50.0, word: "50 ms", tick: "50" }
  , { value: 100.0, word: "100 ms", tick: "100" }
  ]

stepFade :: Number -> Number
stepFade = nextRung fadeLadder

fadeWord :: Number -> String
fadeWord ms = fromMaybe (show (Int.round ms) <> " ms") (rungWord fadeLadder ms)

-- | How much a pass costs what is already there, in decibels.
-- |
-- | **Hold first, and hold by default**, because holding is what a looper has
-- | always done and every loop recorded before this existed was recorded
-- | expecting it.
-- |
-- | The rungs are chosen by how long they take rather than by round numbers: at
-- | one a pass a phrase is still there thirty passes later, which is Fripp's
-- | Soundscapes; at twelve it is gone in three, which is a slapback with
-- | delusions. Three and six are where most of the interesting ambience lives.
decayLadder :: Array Rung
decayLadder =
  [ { value: 0.0, word: "hold", tick: "hold" }
  , { value: -1.0, word: "-1 dB", tick: "1" }
  , { value: -3.0, word: "-3 dB", tick: "3" }
  , { value: -6.0, word: "-6 dB", tick: "6" }
  , { value: -12.0, word: "-12 dB", tick: "12" }
  ]

stepDecay :: Number -> Number
stepDecay = nextRung decayLadder

decayWord :: Number -> String
decayWord db = fromMaybe (show (Int.round db) <> " dB") (rungWord decayLadder db)

shortSlot :: BankSlot -> String
shortSlot = case _ of
  ConfigBank -> "Config"
  slot -> slotName slot

rateWord :: Number -> String
rateWord r
  | r == 0.25 = "1/4"
  | r == 0.5 = "1/2"
  | r == 1.5 = "1 1/2"
  | r == 2.0 = "2"
  | otherwise = "1"

placeWord :: Int -> String
placeWord p
  | p <= 10 = "Left"
  | p <= 52 = "L 50"
  | p <= 74 = "Centre"
  | p <= 116 = "R 50"
  | otherwise = "Right"

-- | What one switch does, across the three gestures it can carry.
-- |
-- | `double` and `hold` are `Maybe` and mostly `Nothing`, which is a design
-- | position rather than an omission twice over.
-- |
-- | **It is a memory burden.** An unmarked switch is remembered as a position,
-- | and three meanings on every position is three times as much to hold while
-- | standing on it. Filling the surface because it is there is how a pedal
-- | becomes something you have to think about.
-- |
-- | **And it used to be a latency.** A tap cannot be known to be a tap until
-- | the double-tap window expires, so any switch that *might* be
-- | double-tapped answers a few hundred milliseconds late. Making it a `Maybe`
-- | lets the recogniser resolve instantly for the switches that have no
-- | double — which is most of them — instead of taxing every press for a
-- | gesture only a few carry.
type Duties =
  { tap :: Duty
  , double :: Maybe Duty
  , hold :: Maybe Duty
  }

only :: Duty -> Duties
only d = { tap: d, double: Nothing, hold: Nothing }

alsoDouble :: Duty -> Duties -> Duties
alsoDouble d s = s { double = Just d }

alsoHold :: Duty -> Duties -> Duties
alsoHold d s = s { hold = Just d }

-- | Where a press sends the board, from the duty rather than from a second
-- | list. A bank jump is a *consequence* of what the switch is for.
dutyTap :: Duty -> Maybe Jump
dutyTap = case _ of
  Enter slot -> Just (ToSlot slot)
  Back j -> Just j
  -- Choosing a loop *is* opening its page — one act, so one press. The MC6
  -- makes the jump itself and the app is told which loop by the same CC, so
  -- there is no moment where the two disagree about whose page this is.
  SelectLoop _ -> Just (ToSlot LoopPage)
  _ -> Nothing

-- | Whether this switch carries one meaning and nothing else.
-- |
-- | **The condition for using `ActionPress`, and it is exact.** Measured on the
-- | device 2026-08-21: the MC6 fires `Press` at press-down, unconditionally and
-- | for every switch — a press-only switch and a switch with all four actions
-- | bound both reported the instant the foot landed. So the deferral was never
-- | on the press. It is on the **release**, which is the message that has to be
-- | *decided*: is this a single tap's release, the first half of a double, or
-- | the end of a hold?
-- |
-- | Which gives the rule with a sharp edge: **a switch may use `ActionPress` if
-- | and only if it carries exactly one gesture.** Add a second and `Press`
-- | becomes a message that fires before the device knows what you meant — the
-- | tap runs, and then the hold runs on top of it. That is Morningstar's own
-- | advice to program the release on any switch that also has a long press,
-- | arrived at from underneath.
-- |
-- | The switches that qualify get their press for nothing: no double-tap window
-- | to wait out, no lateness to hand the daemon, and no fallback needed either
-- | — two presses in quick succession are simply two presses, which is already
-- | what a stepper wants and is harmless for everything else here.
soleGesture :: Duties -> Boolean
soleGesture s = s.double == Nothing && s.hold == Nothing

-- | Whether the device reported this the moment the foot landed.
-- |
-- | Read by the app to decide what to tell the daemon about lateness, and
-- | derived from the same table the device was programmed from rather than
-- | agreed by hand — which is the whole reason the app can know it at all.
firesAtPressDown :: BankSlot -> Int -> Gesture -> Boolean
firesAtPressDown slot i g =
  g == Tap && maybe false soleGesture (dutiesAt slot i)

-- | What a switch is for under a given gesture, when it is for anything.
-- |
-- | `Nothing` is a real answer twice over: most switches carry no hold, and a
-- | double on a switch with no second meaning is a fumble the device programming
-- | already turned into a tap (see the module header) — so a `Double` reaching
-- | here unbound means the board and this table have fallen out of step, which
-- | is worth saying rather than covering for.
dutyFor :: Gesture -> Duties -> Maybe Duty
dutyFor g s = case g of
  Tap -> Just s.tap
  Double -> s.double
  Hold -> s.hold

-- | Where a press sends the board, if anywhere.
-- |
-- | **Read from the same table the device was programmed from**, which is the
-- | only way the app can follow a bank change it did not command. Most of the
-- | navigation in this family is the MC6's own: a long press on a loop switch
-- | jumps to the config bank without a word to anyone, because that jump is a
-- | message stored *on the device*. The app sees the loop switch's CC and
-- | nothing else — so if it waits to be told, it is one press behind for ever,
-- | and the legend describes a bank nobody is standing on.
-- |
-- | It does not have to wait. It wrote the jumps; it can read them back.
sendsTo :: BankSlot -> Int -> Gesture -> Maybe Jump
sendsTo slot i g = do
  s <- dutiesAt slot i
  d <- dutyFor g s
  dutyTap d

-- | What a given switch on a given bank is for. The whole surface, in one
-- | lookup that everything else goes through.
dutiesAt :: BankSlot -> Int -> Maybe Duties
dutiesAt slot = Array.index (layout slot)

dutyAt :: BankSlot -> Int -> Maybe Duty
dutyAt slot i = _.tap <$> dutiesAt slot i

-- | The twelve switches of each bank.
-- |
-- | Switches 0-5 are the MC6's own A-F, which the device labels on its screen;
-- | 6-11 are two FS3X units, which have no screen and no markings.
layout :: BankSlot -> Array Duties
layout slot =
  Array.take mc6OwnSwitches (own slot <> Array.replicate mc6OwnSwitches (only Nothing_))
    <> toolbar slot

-- | The six unmarked switches, and they are the same six everywhere.
-- |
-- | **This is a rule about feet, not about screen space.** G to L have no
-- | markings and no display; the only way to use them is to remember them, and
-- | memory of a footswitch is memory of a *position*. A switch that clears a
-- | loop on one page and sets an end-state on the next cannot be learned at all
-- | — you would have to know which bank you were on before you could know what
-- | your foot was about to do, which is exactly the thing a footswitch exists
-- | to avoid.
-- |
-- | So the family reserves them, and every bank spends its own choices on A to
-- | F, where the MC6 prints a label. That costs each sub-bank half its options,
-- | and the cost turned out to be nearly free: the reverse speeds were made
-- | redundant when direction became the sign of speed, and the rest of what was
-- | up here — the every-N counts, the leaving-states, momentary — is not
-- | implemented and had nowhere honest to sit anyway.
-- |
-- | `Back` is the one that changes destination: from the loops it leaves the
-- | looper entirely, and from anywhere else it goes home to the loops. Same
-- | role, same place, one press, from any depth.
toolbar :: BankSlot -> Array Duties
toolbar slot =
  -- Out. From the loops it leaves the looper; from anywhere else it goes home
  -- to the loops, and a hold leaves outright from any depth — so the way out
  -- is one gesture wherever you happen to be standing.
  [ case slot of
      LoopBank -> only (Back ToBoard)
      _ -> alsoHold (Back ToBoard) (only (Back (ToSlot LoopBank)))
  , alsoDouble StartAll (only StopAll)
  , alsoDouble Redo (only Undo)
  , alsoDouble ClearAll (only ClearLoop)
  -- **The swap.** Claiming the past is the live gesture and the one thing no
  -- pedal can do; saving a WAV is never time-critical and was holding the fast
  -- slot while the feature the sixty-second ring exists for had no switch at
  -- all.
  , alsoDouble SaveTake (only ClaimPast)
  , only ClickToggle
  ]

-- | The MC6's own six, which the device labels on its screen and which each
-- | bank is therefore free to spend as it likes.
own :: BankSlot -> Array Duties
own = case _ of

  -- Six loops on six switches: a loop is *where you put your foot*, not a mode
  -- you enter. Which is also why the loop bank has no room for anything else.
  --
  -- **One gesture each, and that is the point.** These carried three — tap to
  -- act, double to overdub, hold for config — and so had to be programmed on
  -- the release side, where the device waits out its double-tap window before
  -- it can say which one you meant. Every loop press in the rig was a few
  -- hundred milliseconds late, on the one switch where a few hundred
  -- milliseconds is a take.
  --
  -- Carrying one meaning, they sit on `ActionPress` and report the instant a
  -- foot lands. The verbs they used to carry are on `LoopPage`, one switch and
  -- one printed name each.
  -- **Not switch order — grid order.** Switch A selects loop 5, because A is
  -- the bottom-left switch and loop 5 is the bottom-left loop. See
  -- `switchLoops`.
  LoopBank -> map (only <<< SelectLoop) switchLoops

  -- **The verbs, for whichever loop is in hand.**
  --
  -- One bank, not six. The page is the same six switches whatever brought you
  -- here, because the app already knows which loop that was — the same
  -- arrangement the config bank has always had, and the reason the family fits
  -- on the device at all. The pedal names the verb; the computer names the loop.
  --
  -- Every switch here carries exactly one gesture, so every one of them is on
  -- `ActionPress`. That is the whole return on the redesign: Record answers
  -- when your foot lands, not when the device has finished deciding you were
  -- not about to press it again.
  --
  -- No `< Loops` on F: the way out is the toolbar's, in the same place it is on
  -- every other bank, which leaves all six of the printed switches for verbs.
  LoopPage ->
    [ only RecordLoop
    , only OverdubLoop
    , only Transport
    , only ArmLoop
    -- **E is a shortcut, and it is the only one.** Reverse also lives on the
    -- config bank, where it belongs by category — but flipping a loop backwards
    -- is something you do mid-phrase, and two presses away is the wrong distance
    -- for a thing you reach for while playing. Nothing else on the config family
    -- earns that: speed, pan and the modes are all decisions you make once.
    --
    -- Duplication across banks is cheap here in a way it would not be on G to L.
    -- These six are printed on the screen, so a player reads what is under their
    -- foot rather than remembering it, and the positional-grammar rule that
    -- governs the unmarked switches does not apply.
    , only Reverse
    , only (Enter ConfigBank)
    ]

  -- The four that lead somewhere sit first, because they are the ones with a
  -- value to choose; the two that act sit last.
  ConfigBank ->
    [ only (Enter QuantiseBank)
    , only (Enter SpeedBank)
    , only (Enter ModesBank)
    , only (Enter PanBank)
    , only (Reverse)
    , only (Pendulum)
    ]

  -- Free is the default and sits first, because ambient wants it and because a
  -- loop that quantises when you did not ask is a loop that starts late for a
  -- reason you cannot see.
  QuantiseBank -> [ only (Free)
    , only (Grid 1)
    , only (Grid 2)
    , only (Grid 4)
    , only (Grid 8)
    , only (Back (ToSlot ConfigBank))
    ]

  -- No reverse row: direction is the sign of speed, so backwards at half speed
  -- is Reverse on the config bank and then a half here. Two presses for a thing
  -- that was ten switches, and one fewer place for the two to disagree.
  SpeedBank -> [ only (Rate 0.25)
    , only (Rate 0.5)
    , only (Rate 1.0)
    , only (Rate 1.5)
    , only (Rate 2.0)
    , only (Back (ToSlot ConfigBank))
    ]

  -- **Modes, where Chance was.**
  --
  -- Chance had a bank of five to itself and could not do any of it — five
  -- switches spending the config bank's scarcest resource on a feature waiting
  -- on a random source in the audio callback. It keeps one place here, which is
  -- all an unimplemented thing has earned.
  --
  -- What replaces it is the shape the surface actually wanted. Quantise, speed
  -- and pan are each **one value chosen from a few**, and a bank of five reads
  -- like that. One-shot and level-arm are not values, they are *toggles*, and
  -- they are not exclusive — which is the conundrum that came up when the last
  -- of the config switches was being spent: you cannot step through a set of
  -- things that can all be on at once. A bank of independent switches is the
  -- honest rendering of a set of independent facts.
  --
  -- Three of the six are empty, deliberately. This is where the modes that are
  -- still being argued about will land, and a bank with room in it is better
  -- than one that has to be redesigned to admit the next one.
  ModesBank ->
    [ only OneShot
    , only LevelArm
    , only StepChance
    , only StepFade
    , only StepDecay
    , only (Back (ToSlot ConfigBank))
    ]

  -- Five places across the field rather than the eight this had, which is
  -- plenty for placing six loops against each other and is what fits where the
  -- device can print the names.
  PanBank -> [ only (Place 0)
    , only (Place 32)
    , only (Place 64)
    , only (Place 96)
    , only (Place 127)
    , only (Back (ToSlot ConfigBank))
    ]

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

  padTo :: Array Duties -> Array Duties
  padTo ds = Array.take switchCount (ds <> Array.replicate switchCount (only Nothing_))

  compile :: BankSlot -> Int -> Duties -> ControlBankSwitch
  compile slot i sw =
    -- The device's own words come out of the duty, so a switch cannot be
    -- labelled one thing on the pedal and mean another in the app. The tap is
    -- what gets printed: the pedal has room for one name, and the gesture a
    -- player reaches for without thinking is the one it should say.
    { label: dutyLabel sw.tap
    , longName: dutyName sw.tap
    -- Never the MC6's native toggle. A latching switch keeps state on the
    -- device, and the device is the one thing here that cannot be told it is
    -- wrong — every piece of state lives in the app, which can see the engine.
    , toToggle: false
    -- Blank switches are written blank rather than left alone, so uploading
    -- over whatever the bank held before leaves no stragglers doing something
    -- from a previous life.
    , messages:
        if sw.tap == Nothing_ then []
        else
          let bs = bindings slot i sw
          in map _.cc bs <> Array.mapMaybe _.jump bs
    }

  -- | Every message this switch carries, one row per gesture the device can
  -- | tell apart.
  -- |
  -- | The gesture travels in the value and the bank jump rides on the same
  -- | action, so the app is told what happened by the same press that made it
  -- | happen. There is nothing left to infer from timing, from ordering, or from
  -- | a memory of the last bank change.
  -- |
  -- | **The double-tap fallback is here and nowhere else.** A switch with no
  -- | second meaning still binds `DoubleTapRelease` — to the tap's own value and
  -- | the tap's own jump — because the device suppresses `Release` on a double
  -- | whether or not anything is listening, and a fumbled double that does
  -- | nothing is worse than one that does the thing once.
  bindings
    :: BankSlot -> Int -> Duties
    -> Array { cc :: MC6Message, jump :: Maybe MC6Message }
  bindings slot i sw =
    let
      emit action value duty =
        { cc: MC6Msg.ccMessage switchChannel (switchCC slot i) value action
        , jump: (\j -> MC6Msg.bankJumpMessage (target j) action) <$> dutyTap duty
        }

      row g = do
        duty <- case g of
          -- The fallback: no second meaning means a double is a tap said twice.
          Double -> Just (fromMaybe sw.tap sw.double)
          _ -> dutyFor g sw
        let value = case g of
              Double | sw.double == Nothing -> gestureValue Tap
              _ -> gestureValue g
        Just (emit (gestureAction g) value duty)
      -- | **Report on the press, navigate on the release.**
      -- |
      -- | A sole-gesture switch reports at press-down, which is the whole point
      -- | of it. But putting its bank jump on the same action put a CC and a
      -- | bank change in one message list again — and that configuration has
      -- | cost this project twice now. The first time it ate the *release*: the
      -- | board moved mid-press, so the second message was emitted from a bank
      -- | the app was not expecting. This time it ate the CC itself, and the
      -- | symptom was much worse for being quiet — selecting a loop did nothing
      -- | at all, so `focus` never moved off zero, and every verb on the page
      -- | went to loop 1 while the pedal said loop 2. It looked like the machine
      -- | was misrouting; nothing was reaching the machine.
      -- |
      -- | So the two are never on the same action any more. The CC goes at
      -- | press-down, the board moves when the foot lifts a few milliseconds
      -- | later, and the app is told *before* the thing it is being told about
      -- | happens. Ordering within one action was not enough; the device gets a
      -- | separate action.
      --
      -- Binding the release costs nothing: `Press` fires at press-down whatever
      -- else is bound (measured), so the report stays instant. The double is
      -- covered too, because the device suppresses `Release` on one and the
      -- board would otherwise stay put after two quick presses.
      pressSide duty =
        Array.cons
          (MC6Msg.ccMessage switchChannel (switchCC slot i) (gestureValue Tap) ActionPress)
          (case dutyTap duty of
             Nothing -> []
             Just j ->
               [ MC6Msg.bankJumpMessage (target j) ActionRelease
               , MC6Msg.bankJumpMessage (target j) ActionDoubleTapRelease
               ])
    in
      -- One meaning, so there is nothing for the device to resolve and no
      -- reason for it to wait. See `soleGesture`.
      if soleGesture sw then map (\m -> { cc: m, jump: Nothing }) (pressSide sw.tap)
      else Array.mapMaybe row allGestures

  -- | **The CCs before the jumps.**
  -- |
  -- | A bank jump that goes out first means the message after it is emitted from
  -- | the bank the board has already reached — which is how the app came to see
  -- | a press on one bank and its release on another, forget the orphan, and
  -- | then fire a hold nobody made. The gesture is one message now, so this is
  -- | belt as well as braces; the ordering is kept because the reason it was
  -- | needed has not stopped being true of the device.
  target :: Jump -> Int
  target = case _ of
    ToSlot s -> cfg.base + slotIndex s
    ToBoard -> cfg.boardBank
