-- | Switches that are the same on every page.
-- |
-- | Some footswitches are not part of a performance, they are part of the
-- | instrument: the way back, a tuner, a tap. Authoring those per bank means
-- | typing the same thing thirty times and having no way to keep the copies
-- | honest afterwards — and the MC6's editor gives you no better answer.
-- |
-- | This app already *detects* them. `Survey.universalEdges` finds the switch
-- | that goes to the same bank across most of the instrument and draws it
-- | faint, precisely because furniture should recede. What was missing was the
-- | other half: a way to say "this is furniture" rather than inferring it after
-- | the fact from thirty independent copies that happen to agree.
-- |
-- | **The device has no shared switch, and cannot.** Whatever is defined here
-- | compiles to one copy per bank at sync, exactly as a board preset compiles
-- | to messages. So this is not a feature of the MC6 that we expose; it is a
-- | feature of the *source*, and the sync is the compiler. That is the same
-- | posture as the rest of DESIGN-v2: the app is what is written, the device is
-- | what is produced.
-- |
-- | A bank can refuse a shared switch by listing its slot in `sharedOverrides`,
-- | which is how a page that needs G for something else says so — locally,
-- | visibly, and without breaking the other twenty-nine.
module Data.MC6.Shared
  ( SharedSwitch
  , applyShared
  , sharedAt
  , isOverridden
  , pageCount
  , migrateReturns
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (maximumBy)
import Data.Map as Map
import Data.MC6.ControlBank (ControlBank)
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..), MC6Message)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)

type SharedSwitch =
  { id :: String
  -- | Which switch it occupies, on every page that accepts it. A shared switch
  -- | that moved around would not be furniture — the whole value is that your
  -- | foot knows where it is without looking.
  , slot :: Int
  , label :: String
  , longName :: String
  , toToggle :: Boolean
  , messages :: Array MC6Message
  }

-- | The shared switch that owns a slot, if any.
sharedAt :: Array SharedSwitch -> Int -> Maybe SharedSwitch
sharedAt shared slot = Array.find (\s -> s.slot == slot) shared

-- | Whether this bank has taken the slot back for itself.
isOverridden :: ControlBank -> Int -> Boolean
isOverridden cb slot = Array.elem slot cb.sharedOverrides

-- | How many authored pages a shared switch actually lands on.
-- |
-- | Shown next to the definition, because "on every page" is a claim and the
-- | number is the fact. A shared switch overridden everywhere is one nobody is
-- | using, and that should be visible rather than implied.
pageCount :: Array ControlBank -> SharedSwitch -> Int
pageCount banks s =
  Array.length (Array.filter (\cb -> not (isOverridden cb s.slot)) banks)

-- | Write the shared switches into a bank, ready for compilation.
-- |
-- | Applied on the way to the device rather than stored into the bank, so the
-- | authored page keeps saying what its author wrote and the shared parts stay
-- | in one place. Overridden slots are left exactly as the bank has them.
applyShared :: Array SharedSwitch -> ControlBank -> ControlBank
applyShared shared cb = cb { switches = Array.mapWithIndex fill cb.switches }
  where
  fill idx sw = case sharedAt shared idx of
    Just s | not (isOverridden cb idx) ->
      { label: s.label, longName: s.longName, toToggle: s.toToggle, messages: s.messages }
    _ -> sw

-- | Turn the old hardcoded return switch into an ordinary shared switch.
-- |
-- | `returnSwitchIndex` was this idea with the generality taken out: one
-- | behaviour, defined app-wide, placed per page, and the compiler substituted
-- | its messages at sync. As a shared switch it is just a bank jump that
-- | happens to be on every page — and it gains the thing the hardcoded version
-- | could not do, which is to be removed from *one* bank without dropping back
-- | to authoring it thirty times.
-- |
-- | **Behaviour-preserving by construction.** The shared switch goes at the slot
-- | most pages already use. A page that had its return somewhere else keeps it
-- | exactly where it was — the jump is written into that page's own slot, and
-- | the shared slot is overridden there so the page does not quietly grow a
-- | second way back.
-- |
-- | Runs once, when there are no shared switches yet. After that the shared
-- | switch is ordinary and `returnSwitchIndex` means nothing.
migrateReturns
  :: Int                  -- ^ the bank a return jumps to
  -> Array ControlBank
  -> { shared :: Array SharedSwitch, banks :: Array ControlBank }
migrateReturns targetBank banks = case modalSlot of
  Nothing -> { shared: [], banks }
  Just slot ->
    { shared:
        [ { id: "shared-return"
          , slot
          , label: labelAt slot
          , longName: "Back to Board Bank"
          , toToggle: false
          , messages: [ jump ]
          }
        ]
    , banks: map (relocate slot) banks
    }
  where
  jump :: MC6Message
  jump = MC6Msg.bankJumpMessage targetBank ActionPress

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
    cb <- Array.find (\b -> b.returnSwitchIndex == slot
                              && maybe false (_ /= "") (map _.label (Array.index b.switches slot)))
            banks
    sw <- Array.index cb.switches slot
    pure sw.label
    where maybe d f = case _ of
            Nothing -> d
            Just x -> f x

  relocate slot cb
    | cb.returnSwitchIndex == slot = cb
    | otherwise = cb
        { switches = fromMaybe cb.switches
            (Array.modifyAt cb.returnSwitchIndex
              (\sw -> sw { label = if sw.label == "" then "< Back" else sw.label
                         , longName = "Back to Board Bank"
                         , toToggle = false
                         , messages = [ jump ]
                         }) cb.switches)
        , sharedOverrides = Array.nub (Array.snoc cb.sharedOverrides slot)
        }
