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
  ) where

import Prelude

import Config.Registry (PedalRegistry)
import Data.Array as Array
import Data.MC6.ControlBank (ControlBank)
import Data.MC6.Types (MC6NativeBank)
import Data.MC6.Verb (NavTarget(..), Verb(..), classify)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | How the MC6 MKII numbers its banks, taken from the device's own backup
-- | file, where `bankArray` runs 0 to 29.
bankCount :: Int
bankCount = 30

-- | Where our picture of a bank came from, worst-known last.
data Provenance
  = Authored   -- ^ this app wrote it, so we know exactly what is there
  | Declared   -- ^ described in the controller config; may be stale
  | Unknown    -- ^ never looked. Not the same as empty.

derive instance Eq Provenance

type BankCard =
  { bankNumber :: Int
  , name :: String
  , provenance :: Provenance
  , slots :: Array Verb
  }

-- | Build a card for every bank the device has.
-- |
-- | Sources are tried in order of authority: what we authored beats what the
-- | config claims, and anything else is admitted as unknown rather than
-- | guessed at.
survey
  :: PedalRegistry
  -> Int                      -- ^ the app's board-recall channel
  -> Array ControlBank        -- ^ pages this app authored
  -> Array MC6NativeBank      -- ^ whatever the controller config declares
  -> Array BankCard
survey registry boardRecallChannel controlBanks nativeBanks =
  map card (Array.range 0 (bankCount - 1))
  where
  card n = case Array.find (\cb -> cb.mc6BankNumber == n) controlBanks of
    Just cb ->
      { bankNumber: n
      , name: cb.name
      , provenance: Authored
      , slots: pad (map (\sw -> classify registry boardRecallChannel sw.messages) cb.switches)
      }
    Nothing -> case Array.find (\nb -> nb.bankNumber == n) nativeBanks of
      Just nb ->
        { bankNumber: n
        , name: nb.bankName
        , provenance: Declared
        , slots: pad (map (\p -> classify registry boardRecallChannel p.messages) nb.presets)
        }
      Nothing ->
        { bankNumber: n, name: "", provenance: Unknown, slots: [] }

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
navigationEdges cards = Array.nub do
  c <- knownBanks cards
  v <- c.slots
  case v of
    Navigation (ToBank n) -> pure (Tuple c.bankNumber n)
    _ -> []
