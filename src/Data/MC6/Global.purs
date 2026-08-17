-- | Switches that belong to the instrument rather than to any one page.
-- |
-- | Some footswitches are not part of a performance, they are part of the
-- | instrument: the way back, a tuner, a tap. Authoring those per bank means
-- | typing the same thing thirty times and having no way to keep the copies
-- | honest afterwards — and the MC6's editor gives you no better answer.
-- |
-- | **The device has no global switch, and cannot.** Whatever is defined here
-- | compiles to one copy per bank at sync, exactly as a board preset compiles
-- | to messages. So this is not a feature of the MC6 that we expose; it is a
-- | feature of the *source*, and the sync is the compiler. That is the same
-- | posture as the rest of DESIGN-v2: the app is what is written, the device is
-- | what is produced.
-- |
-- | ## A global cannot be left
-- |
-- | An earlier version let a page take a slot back — a per-page override — and
-- | that one escape hatch was the whole problem. A slot could then be local,
-- | global, or global-but-not-here, and standing in front of the third state
-- | there was no way to tell whether typing would change this page or all of
-- | them. Three states, and the one you were in was the one you could not see.
-- |
-- | So: **a global is on every page, or it is not a global.** There is no
-- | membership list and no exception. The escape hatch moves off the slot and
-- | onto the global itself, as `dissolve` — which turns it into an ordinary
-- | local copy on every page it occupied and forgets it. The exception is bought
-- | once, deliberately, by giving up the link.
-- |
-- | That leaves exactly two operations, and they are duals: `promote` makes a
-- | local switch global, `dissolve` makes a global local everywhere.
-- | Promote-then-dissolve is the identity, which is the sign that these are two
-- | concepts rather than one concept with a flag.
-- |
-- | `stampTo` is the other half of the answer: a one-time write of one switch
-- | onto a chosen set of pages, with no link afterwards. It is what you use when
-- | you want a switch on *most* pages, or on the five pages of one group — the
-- | cases a global deliberately refuses to express.
module Data.MC6.Global
  ( GlobalSwitch
  , globalAt
  , toSwitch
  , applyGlobals
  , promote
  , discard
  , dissolve
  , dissolveInto
  , stampTo
  , retireOverrides
  , migrateReturns
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, maximumBy)
import Data.Map as Map
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch, switchLetter)
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..), MC6Message)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)

type GlobalSwitch =
  { id :: String
  -- | Which switch it occupies, on every page. A global that moved around would
  -- | not be furniture — the whole value is that your foot knows where it is
  -- | without looking.
  , slot :: Int
  , label :: String
  , longName :: String
  , toToggle :: Boolean
  , messages :: Array MC6Message
  }

-- | The global that owns a slot, if any.
globalAt :: Array GlobalSwitch -> Int -> Maybe GlobalSwitch
globalAt globals slot = Array.find (\g -> g.slot == slot) globals

-- | A global's content as an ordinary switch — which is all it ever was.
toSwitch :: GlobalSwitch -> ControlBankSwitch
toSwitch g =
  { label: g.label, longName: g.longName, toToggle: g.toToggle, messages: g.messages }

-- | Write the globals into a bank, ready for compilation.
-- |
-- | Applied on the way to the device rather than stored into the bank, so the
-- | authored page keeps saying what its author wrote and the global parts stay
-- | in one place. Unconditional: every page takes every global, because that is
-- | what a global is.
applyGlobals :: Array GlobalSwitch -> ControlBank -> ControlBank
applyGlobals globals cb = cb { switches = Array.mapWithIndex fill cb.switches }
  where
  fill idx sw = case globalAt globals idx of
    Just g -> toSwitch g
    Nothing -> sw

-- | Make a local switch global. Replaces whatever held that slot before.
promote :: Int -> ControlBankSwitch -> Array GlobalSwitch -> Array GlobalSwitch
promote slot sw globals =
  Array.sortWith _.slot (Array.snoc (Array.filter (\g -> g.slot /= slot) globals) fresh)
  where
  fresh =
    { id: "global-" <> switchLetter slot
    , slot
    , label: sw.label
    , longName: sw.longName
    , toToggle: sw.toToggle
    , messages: sw.messages
    }

-- | Remove a global without writing anything into any page.
-- |
-- | The exact undo for `promote`, which is why it exists alongside `dissolve`:
-- | promoting writes nothing to the pages, so unpromoting must not either.
-- | Every page falls back to whatever its own slot already held, which for a
-- | global made by mistake is the state before the mistake.
-- |
-- | Both exits end the same way — the slot is local on every page — so neither
-- | leaves a third state to render. They differ only in *which* local content
-- | the pages are left holding, and that is a question with an obvious answer
-- | in either direction: keep the global (dissolve) or keep the page (discard).
discard :: Int -> Array GlobalSwitch -> Array GlobalSwitch
discard slot = Array.filter (\g -> g.slot /= slot)

-- | Turn a global into an ordinary local copy on every page, and forget it.
-- |
-- | The only way out of a global, and deliberately the *whole* way out: after
-- | this the slot is plainly local on all thirty pages and can be edited one at
-- | a time like anything else. What you give up is the link — which is the
-- | honest price of an exception, paid once rather than thirty times.
dissolve
  :: Int
  -> Array GlobalSwitch
  -> Array ControlBank
  -> { globals :: Array GlobalSwitch, banks :: Array ControlBank }
dissolve slot globals banks =
  dissolveInto (Array.range 0 (Array.length banks - 1)) slot globals banks

-- | `dissolve`, but only writing the copy into the banks named by index.
-- |
-- | Exists for `retireOverrides`, where the pages that had refused the global
-- | must keep what they refused it *for*. Everywhere else the answer is "all of
-- | them", which is what `dissolve` passes.
dissolveInto
  :: Array Int
  -> Int
  -> Array GlobalSwitch
  -> Array ControlBank
  -> { globals :: Array GlobalSwitch, banks :: Array ControlBank }
dissolveInto targets slot globals banks = case globalAt globals slot of
  Nothing -> { globals, banks }
  Just g ->
    { globals: Array.filter (\x -> x.slot /= slot) globals
    , banks: Array.mapWithIndex write banks
    }
    where
    write i cb
      | Array.elem i targets = setSwitch slot (toSwitch g) cb
      | otherwise = cb

-- | Copy one switch onto the named pages, once, with no link afterwards.
-- |
-- | Addressed by wire bank number rather than by array index because that is
-- | what the picker shows and what the pages are called everywhere else; an
-- | index would be a second numbering to keep straight.
stampTo :: Int -> ControlBankSwitch -> Array Int -> Array ControlBank -> Array ControlBank
stampTo slot sw bankNumbers = map write
  where
  write cb
    | Array.elem cb.mc6BankNumber bankNumbers = setSwitch slot sw cb
    | otherwise = cb

setSwitch :: Int -> ControlBankSwitch -> ControlBank -> ControlBank
setSwitch slot sw cb =
  cb { switches = fromMaybe cb.switches (Array.updateAt slot sw cb.switches) }

-- | Retire the per-page overrides written before a global was all-or-nothing.
-- |
-- | A stored override says two things at once: this page wanted something else
-- | *here*, and the rest of the instrument still wanted the global. Only one of
-- | those survives the new rule, and it is the page's — so any global that even
-- | one page refused is dissolved into copies on the pages that accepted it,
-- | while the refusers keep exactly what they had.
-- |
-- | Behaviour-preserving to the byte, and it runs once: nothing writes an
-- | override again, so the next load finds none.
retireOverrides
  :: Array (Array Int)      -- ^ per-page override slots, parallel to `banks`
  -> Array GlobalSwitch
  -> Array ControlBank
  -> { globals :: Array GlobalSwitch, banks :: Array ControlBank }
retireOverrides overrides globals banks =
  foldl step { globals, banks } refused
  where
  refused = Array.nub (Array.concat overrides)

  step acc slot =
    dissolveInto (accepting slot) slot acc.globals acc.banks

  accepting slot =
    Array.catMaybes
      (Array.mapWithIndex
        (\i ov -> if Array.elem slot ov then Nothing else Just i)
        overrides)

-- | Turn the old hardcoded return switch into ordinary switches.
-- |
-- | `returnSwitchIndex` was this idea with the generality taken out: one
-- | behaviour, defined app-wide, placed per page, and the compiler substituted
-- | its messages at sync.
-- |
-- | Where every page agreed on a slot, that is a global — furniture, in the same
-- | place underfoot on every page, which is exactly what the field was trying to
-- | say. Where they did not agree, it is a **stamp**: each page gets the jump
-- | written into its own slot and no global is created, because a global at the
-- | modal slot would silently give the odd pages a second way back. Promote it
-- | later if you tidy the odd ones up.
-- |
-- | Runs once, when there are no globals yet. After that the field means
-- | nothing.
migrateReturns
  :: Int                  -- ^ the bank a return jumps to
  -> Array ControlBank
  -> { globals :: Array GlobalSwitch, banks :: Array ControlBank }
migrateReturns targetBank banks = case modalSlot of
  Nothing -> { globals: [], banks }
  Just slot
    | unanimous ->
        { globals:
            [ { id: "global-" <> switchLetter slot
              , slot
              , label: labelAt slot
              , longName: "Back to Board Bank"
              , toToggle: false
              , messages: [ jump ]
              }
            ]
        , banks
        }
    | otherwise -> { globals: [], banks: map stampOwnReturn banks }

  where
  jump :: MC6Message
  jump = MC6Msg.bankJumpMessage targetBank ActionPress

  unanimous = Array.length (Array.nub (map _.returnSwitchIndex banks)) == 1

  -- Where most pages already keep the way back. Ties go to the lowest index so
  -- the result does not depend on array order.
  modalSlot :: Maybe Int
  modalSlot =
    map fst
      (maximumBy (\a b -> compare (snd a) (snd b) <> compare (fst b) (fst a))
        (Map.toUnfoldable counts :: Array (Tuple Int Int)))

  counts = Map.fromFoldableWith (+) (map (\cb -> Tuple cb.returnSwitchIndex 1) banks)

  -- Whatever a page called it, preferring a page that bothered to name it.
  labelAt slot = fromMaybe "< Back" do
    cb <- Array.find (\b -> b.returnSwitchIndex == slot && namedAt b slot) banks
    sw <- Array.index cb.switches slot
    pure sw.label

  namedAt b slot = case Array.index b.switches slot of
    Just sw -> sw.label /= ""
    Nothing -> false

  stampOwnReturn cb = setSwitch cb.returnSwitchIndex (returnSwitch cb) cb

  returnSwitch cb =
    { label: case Array.index cb.switches cb.returnSwitchIndex of
        Just sw | sw.label /= "" -> sw.label
        _ -> "< Back"
    , longName: "Back to Board Bank"
    , toToggle: false
    , messages: [ jump ]
    }
