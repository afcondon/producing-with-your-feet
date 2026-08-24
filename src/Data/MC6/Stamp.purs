-- | Sign the sweep's work, so a bank can testify about who wrote it.
-- |
-- | The MC6 has no read-back of "when did you last change", and a write that
-- | silently does nothing is indistinguishable from a write that worked — the
-- | bank simply still holds something, and something is what you expected to
-- | see. That gap is the whole reason this module exists: after a whole-map
-- | write, banks 2–6 came back holding their factory contents while banks 7 and
-- | 8 from the same array landed, and there was no way to tell "the write never
-- | happened" from "the write happened and the read is lying" (2026-08-23).
-- |
-- | So every switch this app leaves blank is instead labelled with **where it
-- | thinks it is and which sweep put it there**, and every bank name carries the
-- | sweep number too. An empty label is wasted evidence; the device will happily
-- | show eight characters, and eight characters is enough to carry a whole
-- | diagnosis.
-- |
-- | Three questions the marks answer that nothing else could:
-- |
-- |   * **Did this write land at all?** A bank showing `Bank 3` was never
-- |     written by us; a bank showing `03A r7` was written by sweep 7.
-- |   * **Did *this* run land, or an earlier one?** The run number separates
-- |     "still good from last time" from "written just now", which a fixed
-- |     marker could not.
-- |   * **Did it land where it was aimed?** The bank number travels *in* the
-- |     label, so a frame that went astray shows up as bank 05 holding switches
-- |     that call themselves `03A`. The MC6 ignores the bank number in an upload
-- |     frame and writes to whatever bank it is standing on, so a write landing
-- |     one bank over is a real, documented failure of this device and not a
-- |     hypothetical.
-- |
-- | The marks are cheap to remove — stop calling `mark` — and they cost nothing
-- | but the appearance of blank switches, which were carrying no information to
-- | begin with.
module Data.MC6.Stamp
  ( mark
  , slotMark
  , bankMark
  , maxRun
  ) where

import Prelude

import Data.Array as Array
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch, switchLetter)
import Data.String.CodeUnits as SCU

-- | The MC6's field widths, which are what make this a formatting problem at
-- | all: eight characters for a switch's short name, twenty-four for a bank's.
-- | Both are enforced by `Data.MC6.SysEx`, which pads or truncates without
-- | comment, so anything that would not fit has to be made to fit here where
-- | the choice is visible.
bankNameWidth :: Int
bankNameWidth = 24

-- | Where the run counter wraps.
-- |
-- | A switch mark is `bank` (2) + `slot` (1) + a space + `r` and the run, which
-- | is exactly the eight characters a short name holds when the run is three
-- | digits. `r1000` would be one over, and the device truncates *silently* — so
-- | the counter wraps rather than growing into a mark that quietly means a
-- | different bank. A thousand sweeps between ambiguities is a great many more
-- | than a debugging session needs, and a wrapped counter is honestly wrong in
-- | a way a truncated one is not.
maxRun :: Int
maxRun = 1000

-- | Sign a page: its name, and every switch on it that is genuinely empty.
-- |
-- | Applied *after* globals, never before. A global owns its slot on every page
-- | and arrives with its own label; marking first would write a mark that the
-- | globals then overwrite, which is harmless but makes the two passes look
-- | like they are fighting. Marking last means the marks land exactly on the
-- | slots nothing else claimed, which is what they are for.
mark :: Int -> ControlBank -> ControlBank
mark run cb = cb
  { name = bankMark run cb.mc6BankNumber cb.name
  , switches = Array.mapWithIndex (slotMark run cb.mc6BankNumber) cb.switches
  }

-- | Mark one switch, if there is nothing there to lose.
-- |
-- | **Only a switch with no label *and* no messages is marked.** A switch that
-- | sends something but was never named is still doing a job, and labelling it
-- | `03A r7` would put a lie on the pedal — the player would read a diagnostic
-- | mark on a footswitch that actually does something. Blankness has to mean
-- | blank in both senses before the slot is free to carry evidence.
slotMark :: Int -> Int -> Int -> ControlBankSwitch -> ControlBankSwitch
slotMark run bank slot sw
  | sw.label /= "" = sw
  | not (Array.null sw.messages) = sw
  | otherwise = sw
      { label = pad2 bank <> switchLetter slot <> " " <> runTag run
      , longName = "Sweep " <> runTag run <> " bank " <> pad2 bank
                     <> " sw " <> switchLetter slot
      }

-- | Mark a bank's name.
-- |
-- | Two cases, because a cleared bank and a written one want different things.
-- | A cleared bank has no name to keep, so the mark *is* the name and says so
-- | in a word a person will read on the device: `CLEAR 13 r7`. A generated bank
-- | has a name worth keeping, so the mark rides on the end of it.
-- |
-- | This matters more than it looks. A bank name and its switches travel in
-- | **different SysEx frames** — `sysexBankData` and `sysexPresetData` — so
-- | marking both gives two independent witnesses to the same write. A bank
-- | whose name updated and whose switches did not is a different fault from one
-- | where nothing arrived, and until now the two were the same screen.
bankMark :: Int -> Int -> String -> String
bankMark run bank name =
  if name == "" then "CLEAR " <> pad2 bank <> " " <> tag
  else SCU.take room name <> " " <> tag
  where
  tag = runTag run
  room = bankNameWidth - SCU.length tag - 1

-- | `r7`. Short because it shares eight characters with a bank and a slot.
runTag :: Int -> String
runTag run = "r" <> show (run `mod` maxRun)

-- | Two digits always, so `03A` and `13A` line up in a column and a missing
-- | leading zero cannot be read as a different bank.
pad2 :: Int -> String
pad2 n
  | n < 10 && n >= 0 = "0" <> show n
  | otherwise = show n
