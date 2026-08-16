-- | The whole instrument at once: thirty banks of twelve switches, and what we
-- | actually know about each.
-- |
-- | The important design point is that **"empty" and "unknown" are different**,
-- | and until the device can be read back most of it is unknown. The app
-- | authors a handful of banks and has a stale two-bank fragment in
-- | `config/controllers/mc6-banks.json`; the other twenty-odd could hold
-- | anything. A view that painted those as empty would be quietly lying about
-- | the majority of the instrument, and would make eliding them actively
-- | dangerous — you would be hiding work you simply had not looked at.
-- |
-- | So provenance is carried per bank, and the honest default is `Unknown`.
-- | Reading the device (`DESIGN-CONTROLS.md` §7) is what turns those into
-- | knowledge, and this survey is the thing that will show it happening.
module Data.MC6.Survey
  ( Provenance(..)
  , BankCard
  , bankCount
  , survey
  , knownBanks
  , navigationEdges
  , NavEdge
  , navEdges
  , universalEdges
  , reachableFrom
  , stranded
  , deadEnds
  ) where

import Prelude

import Config.Registry (PedalRegistry)
import Data.Array as Array
import Data.Foldable (any, foldr)
import Data.MC6.ControlBank (ControlBank)
import Data.MC6.Types (MC6NativeBank)
import Data.MC6.Verb (NavTarget(..), Verb(..), classify)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)

-- | How the MC6 MKII numbers its banks, taken from the device's own backup
-- | file, where `bankArray` runs 0 to 29.
bankCount :: Int
bankCount = 30

-- | Where our picture of a bank came from, best first.
data Provenance
  = Observed   -- ^ the device itself said so. The only one that is not a belief.
  | Authored   -- ^ this app wrote it, and nothing has checked since
  | Declared   -- ^ described in the controller config; may be stale
  | Unknown    -- ^ never looked. Not the same as empty.

derive instance Eq Provenance

type BankCard =
  { bankNumber :: Int
  , name :: String
  , provenance :: Provenance
  , slots :: Array Verb
  -- | The switch names the device reports, when it has been read. These are
  -- | separate from `slots` on purpose: `slots` is what we *meant* a switch to
  -- | do and comes from what we authored, while this is what the hardware says
  -- | is on it. Holding both is what makes disagreement visible at all.
  , observedNames :: Array String
  -- | `Nothing` where there is nothing to compare — the bank was never authored
  -- | here, or never read from there. Silence rather than a false clean bill.
  , agrees :: Maybe Boolean
  }

-- | Build a card for every bank the device has.
-- |
-- | Sources are tried in order of authority: what we authored beats what the
-- | config claims, and anything else is admitted as unknown rather than
-- | guessed at.
survey
  :: PedalRegistry
  -> Int                       -- ^ the app's board-recall channel
  -> Array ControlBank         -- ^ pages this app authored
  -> Array MC6NativeBank       -- ^ whatever the controller config declares
  -> Map Int String            -- ^ bank names the device reported
  -> Map Int (Array String)    -- ^ switch names the device reported, per bank
  -> Array BankCard
survey registry boardRecallChannel controlBanks nativeBanks readNames readSwitches =
  map card (Array.range 0 (bankCount - 1))
  where
  card n =
    let observedName = Map.lookup n readNames
        observed = fromMaybe [] (Map.lookup n readSwitches)
        authored = Array.find (\cb -> cb.mc6BankNumber == n) controlBanks
        declared = Array.find (\nb -> nb.bankNumber == n) nativeBanks

        slots = case authored, declared of
          Just cb, _ -> pad (map (\sw -> classify registry boardRecallChannel sw.messages) cb.switches)
          _, Just nb -> pad (map (\p -> classify registry boardRecallChannel p.messages) nb.presets)
          _, _ -> []

        -- A read beats anything we merely believe, and a bank the device named
        -- is known even when we have no idea what is on its switches.
        provenance
          | not (Array.null observed) = Observed
          | isJust observedName = Observed
          | isJust authored = Authored
          | isJust declared = Declared
          | otherwise = Unknown

        name = case observedName, authored, declared of
          Just nm, _, _ | nm /= "" -> nm
          _, Just cb, _ -> cb.name
          _, _, Just nb -> nb.bankName
          _, _, _ -> ""

        -- Compare the labels we intended against the labels the device reports.
        -- Only meaningful when both exist; anything else is Nothing rather than
        -- a clean bill we cannot justify.
        agrees = case authored, Array.null observed of
          Just cb, false ->
            Just (map _.label cb.switches == Array.take (Array.length cb.switches) observed)
          _, _ -> Nothing
    in { bankNumber: n, name, provenance, slots, observedNames: observed, agrees }

  -- Twelve slots per bank: six on the MC6 itself plus two FS3X's worth.
  pad vs = Array.take 12 (vs <> Array.replicate 12 Blank)

knownBanks :: Array BankCard -> Array BankCard
knownBanks = Array.filter (\c -> c.provenance /= Unknown)

-- | Every jump between banks, as a directed edge.
-- |
-- | This is the payoff for making navigation a verb rather than leaving it as
-- | an untyped message: bank jumps become a graph, and graphs have findable
-- | bugs. Reachability from home finds pages that are programmed and cannot be
-- | got to; a node with no outgoing edge is a page you stomp into and cannot
-- | leave, which is the one that bites mid-take.
-- |
-- | Only counts edges out of banks we actually know, since an unknown bank's
-- | navigation is unknown too — drawing it as a dead end would be a false
-- | accusation.
navigationEdges :: Array BankCard -> Array (Tuple Int Int)
navigationEdges = Array.nub <<< map (\e -> Tuple e.from e.to) <<< navEdges

-- | The same edges, keeping the switch they live on.
-- |
-- | Which switch a jump sits on is not decoration: a footswitch that means the
-- | same thing on every page is a different kind of thing from a jump peculiar
-- | to one bank. The first is the instrument's furniture and should recede; the
-- | second is the actual shape of a performance. Losing the slot index makes
-- | them indistinguishable, which is why this is the primitive and the plain
-- | edge list is derived from it.
type NavEdge = { from :: Int, to :: Int, slot :: Int }

navEdges :: Array BankCard -> Array NavEdge
navEdges cards = do
  c <- knownBanks cards
  Tuple i v <- Array.mapWithIndex Tuple c.slots
  case v of
    Navigation (ToBank n) -> pure { from: c.bankNumber, to: n, slot: i }
    _ -> []

-- | Jumps that are furniture: the same switch going to the same bank from most
-- | of the instrument.
-- |
-- | A "back to bank 1 on switch G everywhere" edge would otherwise draw thirty
-- | lines converging on one node and drown every jump that actually
-- | distinguishes a page. Returned as a set of `(slot, to)` pairs so the view
-- | can draw them faintly rather than hide them — they are real, they are just
-- | not news.
-- |
-- | The threshold is half the known banks, floored at three, so a two-bank rig
-- | never has its only two jumps declared universal.
universalEdges :: Array BankCard -> Set (Tuple Int Int)
universalEdges cards =
  let known = Array.length (knownBanks cards)
      threshold = max 3 (known / 2)
      counted = Map.fromFoldableWith (+)
        (map (\e -> Tuple (Tuple e.slot e.to) 1) (Array.nub (navEdges cards)))
  in Set.fromFoldable
       (Map.keys (Map.filter (_ >= threshold) counted))

-- | Every bank you can get to from `home` by pressing switches.
-- |
-- | Plain breadth-first search over `navigationEdges`. It is here rather than
-- | in the view because it answers a question about the instrument, not about
-- | a drawing of it.
reachableFrom :: Int -> Array BankCard -> Set Int
reachableFrom home cards = go (Set.singleton home) [ home ]
  where
  edges = navigationEdges cards
  go seen frontier = case Array.uncons frontier of
    Nothing -> seen
    Just { head, tail } ->
      let next = Array.filter (\n -> not (Set.member n seen))
                   (map snd (Array.filter (\(Tuple from _) -> from == head) edges))
      in go (foldr Set.insert seen next) (tail <> next)

-- | Banks that are programmed and cannot be walked to.
-- |
-- | The bug this finds is quiet and expensive: you build a page, you cannot
-- | reach it from where you actually stand, and you discover that mid-take.
-- |
-- | Only known banks are accused, for the usual reason — a bank nobody has
-- | read may well be reachable by a jump we cannot see.
stranded :: Int -> Array BankCard -> Array Int
stranded home cards =
  let live = reachableFrom home cards
  in Array.filter (\n -> not (Set.member n live))
       (map _.bankNumber (knownBanks cards))

-- | Known banks carrying no bank jump of their own.
-- |
-- | Softer than it sounds, and the view should say so: the MC6's own bank
-- | up/down gestures still work, so this is "no programmed way out" rather than
-- | "trapped". It is worth surfacing because a page you have to escape by
-- | remembering a hardware gesture is a page that will strand you when the
-- | lights are down.
deadEnds :: Array BankCard -> Array Int
deadEnds cards =
  map _.bankNumber (Array.filter (not <<< any isNav <<< _.slots) (knownBanks cards))
  where
  isNav = case _ of
    Navigation _ -> true
    _ -> false
