-- | Who claims which MC6 bank — in one place, because more than one thing does.
-- |
-- | The device has thirty banks and this app writes to several of them for its
-- | own reasons: the board mirror, the probe, the looper transport, the seven
-- | consecutive banks of the six-loop machine, the diagnostics page. On top of
-- | those sit the user's own control banks, which carry whatever number they
-- | were given.
-- |
-- | Nothing used to compare the two lists, and they collided: the default
-- | control bank and the probe bank both claimed **20**. That does not fail —
-- | it *uploads*. The second write lands on top of the first, the device is
-- | left holding one of them, and which one depends on the order the day
-- | happened to go in. A bank is physical storage on a pedal you are standing
-- | on; discovering a collision by pressing a switch is the wrong way round.
-- |
-- | So: one table, derived from the numbers rather than restating them, plus a
-- | check that says who is fighting over what. The numbers live where they
-- | always did (`Engine.purs`); this module knows how to *enumerate* them, and
-- | that is the part that could not be got right by reading five fields in five
-- | places.
-- |
-- | See `docs/DESIGN-v2.md` on the device as compiled output: the store is the
-- | truth and the pedal is a projection of it. A projection that overwrites
-- | itself is not a projection.
-- |
-- | ## The map, and the line through the middle of it
-- |
-- | Andrew's rule, 2026-08-23: **the machinery consolidates at the low end, and
-- | fifteen upwards belongs to individual pedal controls and to pedal and bank
-- | presets.** So the table has two halves and a boundary, rather than an
-- | accumulation of numbers each chosen for a local reason — which is how the
-- | probe came to be wedged between the looper and a bank it did not know was
-- | occupied.
-- |
-- | In wire numbers (the editor shows each one higher):
-- |
-- | ```
-- |    1   board mirror              — what the board boots into
-- |  2-8   the loop machine          — one bank per BankSlot, base + slotIndex
-- |    9   looper transport          — the legacy one, driven by hand
-- |   10   probe
-- | 11-12  diagnostics — a RANGE, one page per eight pedals, so it grows
-- |        with the registry (`Diagnostics.bypassBankCount`)
-- | 13-14  spare, for this app
-- | ─────  pedalRangeFrom
-- | 15-29  pedal controls, pedal presets, bank presets — the user's own
-- | ```
-- |
-- | The boundary is checked, not just documented (`misplaced`): a machinery
-- | bank at 15 or above, or a control bank below it, is a failing test rather
-- | than a convention someone remembers.
module Data.MC6.Reserved
  ( Claimant(..)
  , BankClaim
  , Collision
  , BankNumbers
  , claimantLabel
  , appClaims
  , allClaims
  , collisions
  , describeCollisions
  , pedalRangeFrom
  , mc6BankCount
  , allBanks
  , sweep
  , isExternalClaim
  , external
  , Misplacement(..)
  , Misplaced
  , misplaced
  , describeMisplaced
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (intercalate)
import Data.Function (on)
import Data.Looper.Banks (BankSlot, allSlots, slotIndex, slotName)
import Data.Maybe (Maybe(..))

-- | The bank numbers the app is configured with. A record rather than five
-- | arguments, so adding a sixth claimant is a compile error at every call site
-- | instead of a silently missing row.
type BankNumbers =
  { board :: Int
  , probe :: Int
  , looperTransport :: Int
  , loopMachineBase :: Int
  , diagnostics :: Int
  -- | How many consecutive banks the diagnostics pages take. NOT one:
  -- | `Diagnostics.bypassBankCount` chunks the registry eight pedals to a bank,
  -- | so thirteen pedals is two banks. Passed in rather than assumed, because
  -- | this module has no business knowing how many pedals there are — and
  -- | assuming it was how bank 12 came to be both spoken for and listed spare.
  , diagnosticsCount :: Int
  }

-- | What is claiming a bank. An ADT and not a string, because the set is closed
-- | and because `LoopMachine` carries which slot it is — "22 and 25 collide" is
-- | not useful; "22 is the Loops page and 25 is Speed" is.
data Claimant
  = Board
  | Probe
  | LooperTransport
  | LoopMachine BankSlot
  | Diagnostics Int
  -- | One of the user's own pages, by id — the only claimant this module cannot
  -- | enumerate for itself.
  | Control String
  -- | A bank this app does not write but must not tread on: something the
  -- | device holds for another purpose entirely. Ableton Controls is the
  -- | standing example, and it is *why* the probe could not simply move up when
  -- | the loop machine grew — a fact that lived in a comment on one field,
  -- | where nothing could consult it.
  | External String

derive instance Eq Claimant

claimantLabel :: Claimant -> String
claimantLabel = case _ of
  Board -> "the board mirror"
  Probe -> "the probe bank"
  LooperTransport -> "the looper transport"
  LoopMachine slot -> "the loop machine's " <> slotName slot <> " page"
  Diagnostics i -> "diagnostics page " <> show (i + 1)
  Control cbId -> "control bank `" <> cbId <> "`"
  External what -> what <> " (not this app's, but on the device)"

type BankClaim = { bank :: Int, claimant :: Claimant }

type Collision = { bank :: Int, claimants :: Array Claimant }

-- | Every bank the APP claims for itself, derived from the configured numbers.
-- |
-- | The loop machine's seven are computed from the base for the same reason the
-- | base is a single setting: "which bank is Speed" should be arithmetic. A
-- | hand-written list of seven is a list that can be edited to disagree with
-- | `slotIndex`, and then the collision check would be checking the wrong banks.
appClaims :: BankNumbers -> Array BankClaim
appClaims n =
  [ { bank: n.board, claimant: Board }
  , { bank: n.probe, claimant: Probe }
  , { bank: n.looperTransport, claimant: LooperTransport }
  ]
    <> map (\i -> { bank: n.diagnostics + i, claimant: Diagnostics i })
         (Array.range 0 (max 1 n.diagnosticsCount - 1))
    <> map (\slot -> { bank: n.loopMachineBase + slotIndex slot, claimant: LoopMachine slot }) allSlots
    <> external

-- | Banks spoken for by something other than this app. Not configurable,
-- | because they are facts about the pedalboard rather than settings — and a
-- | fact nobody can consult is how the probe came to be moved onto an occupied
-- | bank in the first place.
-- | **Empty, and deliberately still here.** It held Ableton Controls at bank 29
-- | until the device was read back and the bank turned out to be 19 — so the
-- | exemption was protecting a bank that had nothing on it while the real one
-- | sat in the clearable range. It was spared only because that write happened
-- | to be refused.
-- |
-- | Andrew, 2026-08-23: everything on the MC6 is up for grabs. So nothing is
-- | exempt except the board mirror, and the list is empty rather than wrong —
-- | an exemption nobody has verified against the device is worse than none,
-- | because it reads as protection and isn't.
-- |
-- | The constructor stays because the concept will return: the plan is for this
-- | app to drive Ableton itself, and until it does, anything the board holds
-- | that we do not generate belongs here — WITH the bank number read off the
-- | device rather than remembered.
external :: Array BankClaim
external = []

-- | The app's claims and the user's, together — which is the only list worth
-- | checking. Either half is internally consistent on its own; the collision
-- | was between them.
allClaims
  :: forall r
   . BankNumbers
  -> Array { id :: String, mc6BankNumber :: Int | r }
  -> Array BankClaim
allClaims n controlBanks =
  appClaims n
    <> map (\cb -> { bank: cb.mc6BankNumber, claimant: Control cb.id }) controlBanks

-- | Every bank with more than one claimant, in bank order. Empty ⇔ nothing
-- | overwrites anything.
collisions :: Array BankClaim -> Array Collision
collisions cs =
  Array.mapMaybe pick
    $ Array.groupBy (eq `on` _.bank)
    $ Array.sortWith _.bank cs
  where
  pick grp =
    let claimants = map _.claimant (NEA.toArray grp)
    in if Array.length claimants > 1
         then Just { bank: (NEA.head grp).bank, claimants }
         else Nothing

-- | Where the machinery stops and the pedals begin. One number, because the
-- | boundary is a decision and not a fact about any one bank.
pedalRangeFrom :: Int
pedalRangeFrom = 15

-- | How many banks the device has, numbered **0 to 29 on the wire**.
-- |
-- | The zero matters and was got wrong here first time: `sweep` ranged 1 to 30,
-- | so wire bank 0 was never cleared and a bank 30 that does not exist was
-- | written to. Every other number in this app is a wire number — the editor
-- | displays each one higher — and the sweep was the one place that quietly
-- | wasn't (2026-08-23, spotted from the device behaving oddly, not from the
-- | test, which asserted the same off-by-one it was checking).
mc6BankCount :: Int
mc6BankCount = 30

-- | Every bank on the device, in wire numbering.
allBanks :: Array Int
allBanks = Array.range 0 (mc6BankCount - 1)

-- | What a whole-map write touches, given the banks that were actually
-- | generated. Three disjoint sets covering `1 .. mc6BankCount`.
-- |
-- | Pure, and separated from the writing for one reason: **`clear` is a list of
-- | banks about to be erased on hardware**, and a set-difference that is only
-- | ever exercised by clicking the button is a set-difference nobody has
-- | checked. Getting it wrong does not throw; it silently wipes a bank that was
-- | not supposed to be in the sweep.
-- |
-- | `untouched` is the board mirror and everything `External`. The board mirror
-- | because it is assigned rather than generated — blanking it discards the
-- | assignment instead of rebuilding it — and externals because nothing here
-- | can regenerate them, which makes clearing one the only step in the sweep
-- | that running the sweep again would not repair.
sweep
  :: BankNumbers
  -> Array Int
  -> { write :: Array Int, clear :: Array Int, untouched :: Array Int }
sweep n owned =
  { write: Array.sort owned
  , clear: Array.filter keepable allBanks
  , untouched: Array.sort untouched
  }
  where
  untouched = Array.cons n.board (map _.bank (Array.filter isExternalClaim (appClaims n)))
  keepable b = not (Array.elem b owned) && not (Array.elem b untouched)

isExternalClaim :: BankClaim -> Boolean
isExternalClaim c = case c.claimant of
  External _ -> true
  _ -> false

data Misplacement
  -- | Machinery that has drifted up into pedal territory.
  = MachineryTooHigh
  -- | A control bank sitting among the machinery, where the next thing added
  -- | to this app will land on top of it.
  | ControlTooLow

derive instance Eq Misplacement

type Misplaced = { bank :: Int, claimant :: Claimant, why :: Misplacement }

-- | Claims on the wrong side of `pedalRangeFrom`.
-- |
-- | `External` is exempt: those are facts about the pedalboard, not decisions
-- | this app gets to make, and a rule that fails on one of them would be
-- | telling the user their own board is wrong.
misplaced :: Array BankClaim -> Array Misplaced
misplaced = Array.mapMaybe check
  where
  check c = case c.claimant of
    External _ -> Nothing
    Control _
      | c.bank < pedalRangeFrom -> Just { bank: c.bank, claimant: c.claimant, why: ControlTooLow }
      | otherwise -> Nothing
    _
      | c.bank >= pedalRangeFrom -> Just { bank: c.bank, claimant: c.claimant, why: MachineryTooHigh }
      | otherwise -> Nothing

describeMisplaced :: Array Misplaced -> Maybe String
describeMisplaced [] = Nothing
describeMisplaced ms = Just $ intercalate "\n" (map line ms)
  where
  line m = case m.why of
    MachineryTooHigh ->
      "bank " <> show m.bank <> " holds " <> claimantLabel m.claimant
        <> ", which is machinery and belongs below " <> show pedalRangeFrom <> "."
    ControlTooLow ->
      "bank " <> show m.bank <> " holds " <> claimantLabel m.claimant
        <> ", which belongs at " <> show pedalRangeFrom <> " or above."

-- | The collisions as something to put in front of a person. `Nothing` when
-- | there are none — so a caller cannot render "0 collisions" as a warning.
describeCollisions :: Array Collision -> Maybe String
describeCollisions [] = Nothing
describeCollisions cols = Just $ intercalate "\n" (map line cols)
  where
  line c =
    "bank " <> show c.bank <> " is claimed by "
      <> intercalate " and " (map claimantLabel c.claimants)
      <> " — whichever is uploaded last is what the device will hold."
