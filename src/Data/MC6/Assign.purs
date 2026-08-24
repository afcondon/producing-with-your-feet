-- | Board presets bound to switches, compiled into the pages that carry them.
-- |
-- | An assignment says "this full-board recall lives on switch A of bank 21".
-- | It is a **source of truth**, and until now it was one the whole-map write
-- | had never heard of: `generatedBanks` did not know assignments existed, so
-- | every sweep wrote a blank over the switch and the app went on believing a
-- | board was there. On 2026-08-24 exactly that had happened to bank 21 switch
-- | A, and nothing in the app could see it — the store said board, the device
-- | said `21A r2`, and no screen compared the two.
-- |
-- | That is rule **B11** in `docs/DESIGN-BANKS.md`, and it is the general
-- | statement of the bug: *every source layer is compiled by the sweep*. If
-- | something can put content on a switch and the sweep does not know about it,
-- | the sweep destroys it — silently, because a blank switch is exactly what a
-- | successful clear looks like.
-- |
-- | So this is the assignment layer, applied to every page on the way to the
-- | device in the same one list the survey checks against.
module Data.MC6.Assign
  ( Assignment
  , Env
  , applyAssignments
  , overBudget
  ) where

import Prelude

import Data.Array as Array
import Data.MC6.Board as Board
import Data.MC6.ControlBank (ControlBank)
import Data.MC6.Types (MC6Message)
import Data.Maybe (Maybe(..))
import Data.Preset (BoardPreset, PedalPreset, PresetId)
import Data.String.CodeUnits as SCU
import Config.Registry (PedalRegistry)

-- | A board bound to one switch of one bank.
-- |
-- | Structurally the same record as `Engine.MC6Assignment`, written out rather
-- | than imported so this module does not depend on the app's whole state type.
type Assignment =
  { bankNumber :: Int
  , switchIndex :: Int
  , boardPresetId :: PresetId
  }

-- | Everything compiling an assignment needs, in one place.
-- |
-- | `controlBank` is the long-press jump a board switch used to carry, back to
-- | whichever control page was active when the switch was written. Kept as a
-- | parameter rather than removed, but the sweep passes `Nothing`: a jump whose
-- | target depends on what was on screen at write time is exactly the kind of
-- | undeclared, position-dependent behaviour Year Zero exists to end, and under
-- | B14 `LongPressRelease` now means something else. The global way home
-- | replaces it.
type Env =
  { registry :: PedalRegistry
  , presets :: Array PedalPreset
  , boards :: Array BoardPreset
  , controlBank :: Maybe Int
  , assignments :: Array Assignment
  }

-- | Write every board assigned to this page onto its switch.
-- |
-- | Compiled exactly as `syncSwitchToMC6` compiles a single switch — same
-- | messages, same eight-character short name, same full name underneath — so
-- | that assigning one board and sweeping the whole map put the same bytes on
-- | the device. Two paths producing two different switches from one assignment
-- | would be the same class of bug as writing one list and checking another.
-- |
-- | An assignment naming a board that no longer exists changes nothing. It is a
-- | dangling reference rather than an instruction to blank the switch, and
-- | blanking would turn a stale row in the store into lost work on the device.
applyAssignments :: Env -> ControlBank -> ControlBank
applyAssignments env cb = cb { switches = Array.mapWithIndex fill cb.switches }
  where
  here = Array.filter (\a -> a.bankNumber == cb.mc6BankNumber) env.assignments

  fill idx sw = case Array.find (\a -> a.switchIndex == idx) here of
    Nothing -> sw
    Just a -> case boardFor a of
      Nothing -> sw
      Just bp ->
        let messages = compile env bp
        in if Array.length messages > Board.messageLimit
             -- Over budget is refused before the write, by `overBudget`. Left
             -- alone here so the page a person is shown is the page that would
             -- be sent, rather than a truncated one the surface invented.
             then sw
             else
               { label: SCU.take 8 bp.name
               , longName: bp.name
               , toToggle: false
               , messages
               }

  boardFor a = Array.find (\bp -> bp.id == a.boardPresetId) env.boards

compile :: Env -> BoardPreset -> Array MC6Message
compile env = Board.boardToMC6Messages env.registry env.presets env.controlBank

-- | Assignments whose board does not fit an MC6 preset.
-- |
-- | **Refuse rather than truncate.** `sysexPresetData` pads with `Array.take
-- | 16`, so an over-budget board programs cleanly and arrives on the hardware
-- | missing its last messages — a switch that silently does most of what you
-- | asked, which is worse than one that says it cannot. The single-switch sync
-- | has refused for a while; the sweep has to refuse on the same grounds or the
-- | slow path is safe and the fast one is not.
overBudget
  :: Env
  -> Array ControlBank
  -> Array { bank :: Int, slot :: Int, board :: String, messages :: Int }
overBudget env banks = Array.mapMaybe check env.assignments
  where
  claimed = map _.mc6BankNumber banks

  check a = do
    _ <- Array.find (_ == a.bankNumber) claimed
    bp <- Array.find (\b -> b.id == a.boardPresetId) env.boards
    let n = Array.length (compile env bp)
    if n <= Board.messageLimit then Nothing
      else Just { bank: a.bankNumber, slot: a.switchIndex, board: bp.name, messages: n }
