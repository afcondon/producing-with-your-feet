module Data.MC6.ControlBank
  ( ControlBank
  , ControlBankSwitch
  , switchCount
  , switchLetter
  , emptySwitch
  , blankBank
  , doubleClaims
  , padSwitches
  , exampleControlBank
  , ambientControlBank
  , ccToggleMessages
  , ccMomentaryMessages
  , controlBankToPresets
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..), MC6Message, MC6MsgType(..), MC6TogglePosition(..))

type ControlBankSwitch =
  { label :: String       -- 8 char max (MC6 short name)
  , longName :: String    -- 24 char max (MC6 long name)
  , toToggle :: Boolean
  , messages :: Array MC6Message
  }

type ControlBank =
  { id :: String
  , name :: String
  , description :: String
  , mc6BankNumber :: Int
  -- | Where this page used to keep its way back, before that became an
  -- | ordinary switch. Read once by `Global.migrateReturns` and meaningless
  -- | afterwards; kept only so the migration can still run on a store that has
  -- | not seen it yet.
  , returnSwitchIndex :: Int
  , switches :: Array ControlBankSwitch
  }

-- | How many switches a bank has.
-- |
-- | Twelve, because that is what an MC6 bank holds: six on the unit and two
-- | FS3X's worth. Authored pages carried nine for a long time, which meant a
-- | quarter of every page was unreachable from this app — and the space is
-- | worth having whether or not a second FS3X is plugged in, since a page swap
-- | on one of the six brings the rest within reach anyway.
switchCount :: Int
switchCount = 12

-- | A B C … L, in index order. Physical position is a separate question and is
-- | answered by the view's `physicalOrder`.
switchLetter :: Int -> String
switchLetter i = case Array.index letters i of
  Just l -> l
  Nothing -> "?"
  where
  letters = [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L" ]

emptySwitch :: ControlBankSwitch
emptySwitch = { label: "", longName: "", toToggle: false, messages: [] }

-- | A bank with nothing in it — which is how this app CLEARS one.
-- |
-- | There is no erase command worth having: twelve empty presets and an empty
-- | name is exactly what an empty bank is, and writing one travels the same
-- | path as writing a real one. So clearing and programming are a single loop
-- | over a single list, and the code that clears is the code that has been
-- | exercised every time anything was ever written.
-- |
-- | A named alternative would be a second, rarer path that only runs on the day
-- | you are wiping the board, which is the worst possible day for it to be the
-- | least-tested thing in the file.
blankBank :: Int -> ControlBank
blankBank n =
  { id: "blank-" <> show n
  , name: ""
  , description: "Cleared"
  , mc6BankNumber: n
  , returnSwitchIndex: 0
  , switches: Array.replicate switchCount emptySwitch
  }

-- | Banks that more than one page claims, with the pages that claim them.
-- |
-- | **A list with two pages on one bank number cannot be checked.** The write
-- | takes both and the device keeps whichever went last; the survey looks the
-- | bank up and takes whichever comes first. So the page that was written and
-- | the page that is compared are different pages, and the card reports
-- | "device disagrees" about a write that was in fact perfect — while the page
-- | the person was actually looking at silently never reached the hardware.
-- |
-- | This is not hypothetical and it was not rare. Ten pages taken off the
-- | device with *Take a copy of this bank* sat on banks 0-6, 17, 19 and 21;
-- | five of them landed on top of the loop machine's own pages, and the result
-- | was five red cards that survived a day of hunting for a fault in the MC6
-- | (2026-08-24). The device had done exactly as it was told, twice.
-- |
-- | The old guard could not see it: it compared the *set* of generated bank
-- | numbers against the reserved table and then explicitly forgave control
-- | banks anywhere, which is precisely the permission that let one land on a
-- | claimed bank. Sets have no multiplicity, and multiplicity was the bug.
doubleClaims :: Array ControlBank -> Array { bank :: Int, pages :: Array String }
doubleClaims banks =
  Array.filter (\c -> Array.length c.pages > 1)
    (map (\n -> { bank: n, pages: pagesOn n }) numbers)
  where
  numbers = Array.nub (Array.sort (map _.mc6BankNumber banks))
  pagesOn n = map _.id (Array.filter (\cb -> cb.mc6BankNumber == n) banks)

-- | Bring a bank up to the full twelve without disturbing what is there.
-- |
-- | Applied where banks are read back in, so pages authored when a bank had
-- | nine switches gain the other three rather than needing a migration. Takes
-- | rather than errors on an over-long array, since the device would only
-- | ignore the excess anyway.
padSwitches :: ControlBank -> ControlBank
padSwitches cb = cb
  { switches = Array.take switchCount (cb.switches <> Array.replicate switchCount emptySwitch) }

-- | CC toggle pair: ToggleOn sends val 127, ToggleOff sends val 0.
-- | MC6 native toggle mode handles the state; we just provide both positions.
ccToggleMessages :: Int -> Int -> Array MC6Message
ccToggleMessages ch cc =
  [ { msgType: MsgCC, channel: ch, data1: cc, data2: 127
    , data3: 0, data4: 0, action: ActionPress
    , togglePosition: ToggleOn, msgIndex: 0 }
  , { msgType: MsgCC, channel: ch, data1: cc, data2: 0
    , data3: 0, data4: 0, action: ActionPress
    , togglePosition: ToggleOff, msgIndex: 1 }
  ]

-- | CC momentary pair: Press sends val 127, Release sends val 0.
ccMomentaryMessages :: Int -> Int -> Array MC6Message
ccMomentaryMessages ch cc =
  [ MC6Msg.ccMessage ch cc 127 ActionPress
  , MC6Msg.ccMessage ch cc 0 ActionRelease
  ]

-- | Compile a page into the preset records SysEx wants.
-- |
-- | It used to substitute a bank jump into whichever switch the page declared
-- | as its return, which made one switch on every page mean something the page
-- | itself did not say. A global switch (`Data.MC6.Global`) does that job now
-- | and says so, so this compiles what is there and nothing else.
controlBankToPresets
  :: ControlBank
  -> Array { switchIndex :: Int, shortName :: String, longName :: String, toToggle :: Boolean, messages :: Array MC6Message }
controlBankToPresets cb =
  Array.mapWithIndex toPreset cb.switches
  where
  toPreset idx sw =
    { switchIndex: idx
    , shortName: sw.label
    , longName: sw.longName
    , toToggle: sw.toToggle
    , messages: indexMessages sw.messages
    }

  indexMessages :: Array MC6Message -> Array MC6Message
  indexMessages msgs = Array.mapWithIndex (\i m -> m { msgIndex = i }) msgs

-- | The **ambient / evolving** control page: MOOD, Onward, Lost+Found, Habit.
-- |
-- | The group `docs/DESIGN-BANKS.md` names, and the reason this page exists
-- | rather than a wall of twelve bypasses: a group page is a thing you are
-- | currently doing, where a list of every pedal is a list of things.
-- |
-- | **The six expander switches are the three dual-engage pedals, two channels
-- | each, and that is not a coincidence — it is why the group fits.** MOOD,
-- | Onward and Lost+Found each have two independently bypassable channels and
-- | no way to be reduced to one switch; three times two is exactly the six that
-- | `G`–`L` provide. Habit engages with one, so it goes on the unit's own six
-- | with room to spare. That split is also the ergonomic one: the FS3X switches
-- | are sloped and reachable, the unit's are neither, and the thing you reach
-- | for mid-phrase is a channel rather than a whole pedal.
-- |
-- | **`L` and `R` here are the pedal's own two switches, named by what they
-- | do rather than by side**, because the engage table stores them as `a` and
-- | `b` and does not say which is which. If a pair is the wrong way round
-- | under your foot, swapping two lines below is the whole fix.
-- |
-- | Sixteen: the first free bank of the pedal half. Fifteen is
-- | `control-default` and anything below `Reserved.pedalRangeFrom` is refused
-- | as `ControlTooLow`.
ambientControlBank :: ControlBank
ambientControlBank =
  { id: "control-ambient"
  , name: "Ambient"
  , description: "MOOD, Onward, Lost+Found channels on the expanders; Habit on the unit"
  , mc6BankNumber: 16
  , returnSwitchIndex: 0
  , switches:
      -- A-F, the unit's own: the whole-pedal moves, which are the ones you can
      -- afford to reach up for.
      [ { label: "Ht Byp",  longName: "Habit Bypass",        toToggle: true, messages: ccToggleMessages 15 102 }
      , { label: "MD Both", longName: "MOOD Both Channels",  toToggle: true, messages: ccToggleMessages 3 55 }
      , emptySwitch
      , emptySwitch
      , emptySwitch
      , emptySwitch
      -- G-L, the expanders: a channel each, in pedal order.
      , { label: "Ow Freez", longName: "Onward Freeze",      toToggle: true, messages: ccToggleMessages 2 102 }
      , { label: "Ow Gltch", longName: "Onward Glitch",      toToggle: true, messages: ccToggleMessages 2 103 }
      , { label: "MD ML",    longName: "MOOD Micro-Looper",  toToggle: true, messages: ccToggleMessages 3 102 }
      , { label: "MD Wet",   longName: "MOOD Wet Channel",   toToggle: true, messages: ccToggleMessages 3 103 }
      , { label: "L+F Lft",  longName: "Lost+Found Left",    toToggle: true, messages: ccToggleMessages 6 103 }
      , { label: "L+F Rt",   longName: "Lost+Found Right",   toToggle: true, messages: ccToggleMessages 6 102 }
      ]
  }


-- | Hard-coded example: direct pedal controls, in the pedal half of the bank
-- | table (`Data.MC6.Reserved`).
exampleControlBank :: ControlBank
exampleControlBank =
  { id: "control-default"
  , name: "Default Controls"
  , description: "Habit loop, Brig infinite, MOOD freeze, Clean/Mercury7 swell, Lex speed, Brig tap"
  -- Fifteen: the first bank of the pedal half of the table. It sat on 20,
  -- where the probe bank also sat — a collision that does not fail but
  -- *uploads*, the second write landing on top of the first, and which nothing
  -- compared the two lists to notice. `Data.MC6.Reserved` holds the map now,
  -- and `pedalRangeFrom` is the line this sits just above.
  , mc6BankNumber: 15
  , returnSwitchIndex: 6
  , switches:
      [ { label: "Ht Loop",  longName: "Habit Loop Toggle",     toToggle: true,  messages: ccToggleMessages 15 24 }
      , { label: "Ht Clear", longName: "Habit Clear",           toToggle: false, messages: ccMomentaryMessages 15 26 }
      , { label: "Br Infin", longName: "Brig Infinite Toggle",  toToggle: true,  messages: ccToggleMessages 14 97 }
      , { label: "MD Freez", longName: "MOOD Freeze Toggle",    toToggle: true,  messages: ccToggleMessages 3 105 }
      , { label: "Cl Swell", longName: "Clean Swell Toggle",    toToggle: true,  messages: ccToggleMessages 4 103 }
      , { label: "M7 Swell", longName: "Mercury7 Swell Toggle", toToggle: true,  messages: ccToggleMessages 12 28 }
      , { label: "< Back",   longName: "Back to Board Bank",    toToggle: false, messages: [] }  -- filled by a global, if one holds G
      , { label: "Lx Speed", longName: "Lex Speed Toggle",      toToggle: true,  messages: ccToggleMessages 8 22 }
      , { label: "Br Tap",   longName: "Brig Tap Tempo",        toToggle: false, messages: ccMomentaryMessages 14 93 }
      , emptySwitch
      , emptySwitch
      , emptySwitch
      ]
  }
