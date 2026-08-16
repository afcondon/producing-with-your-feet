module Data.MC6.ControlBank
  ( ControlBank
  , ControlBankSwitch
  , switchCount
  , switchLetter
  , emptySwitch
  , padSwitches
  , exampleControlBank
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
  , returnSwitchIndex :: Int
  , switches :: Array ControlBankSwitch
  -- | Slots this page keeps for itself, refusing the instrument-wide shared
  -- | switch that would otherwise fill them (`Data.MC6.Shared`). Empty on every
  -- | page until someone deliberately takes a slot back, which is the point: an
  -- | override should be a decision, not a default.
  , sharedOverrides :: Array Int
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

-- | Convert a ControlBank to an array of preset records ready for SysEx programming.
-- | Injects a BankJump "back" message at the returnSwitchIndex.
controlBankToPresets
  :: Int  -- ^ board bank number to jump back to
  -> ControlBank
  -> Array { switchIndex :: Int, shortName :: String, longName :: String, toToggle :: Boolean, messages :: Array MC6Message }
controlBankToPresets returnBankNum cb =
  Array.mapWithIndex toPreset cb.switches
  where
  toPreset idx sw
    | idx == cb.returnSwitchIndex =
        { switchIndex: idx
        , shortName: sw.label
        , longName: sw.longName
        , toToggle: false
        , messages: indexMessages [ MC6Msg.bankJumpMessage returnBankNum ActionPress ]
        }
    | otherwise =
        { switchIndex: idx
        , shortName: sw.label
        , longName: sw.longName
        , toToggle: sw.toToggle
        , messages: indexMessages sw.messages
        }

  indexMessages :: Array MC6Message -> Array MC6Message
  indexMessages msgs = Array.mapWithIndex (\i m -> m { msgIndex = i }) msgs

-- | Hard-coded example: direct pedal controls on MC6 bank 20
exampleControlBank :: ControlBank
exampleControlBank =
  { id: "control-default"
  , name: "Default Controls"
  , description: "Habit loop, Brig infinite, MOOD freeze, Clean/Mercury7 swell, Lex speed, Brig tap"
  , mc6BankNumber: 20
  , returnSwitchIndex: 6
  , sharedOverrides: []
  , switches:
      [ { label: "Ht Loop",  longName: "Habit Loop Toggle",     toToggle: true,  messages: ccToggleMessages 15 24 }
      , { label: "Ht Clear", longName: "Habit Clear",           toToggle: false, messages: ccMomentaryMessages 15 26 }
      , { label: "Br Infin", longName: "Brig Infinite Toggle",  toToggle: true,  messages: ccToggleMessages 14 97 }
      , { label: "MD Freez", longName: "MOOD Freeze Toggle",    toToggle: true,  messages: ccToggleMessages 3 105 }
      , { label: "Cl Swell", longName: "Clean Swell Toggle",    toToggle: true,  messages: ccToggleMessages 4 103 }
      , { label: "M7 Swell", longName: "Mercury7 Swell Toggle", toToggle: true,  messages: ccToggleMessages 12 28 }
      , { label: "< Back",   longName: "Back to Board Bank",    toToggle: false, messages: [] }  -- replaced by controlBankToPresets
      , { label: "Lx Speed", longName: "Lex Speed Toggle",      toToggle: true,  messages: ccToggleMessages 8 22 }
      , { label: "Br Tap",   longName: "Brig Tap Tempo",        toToggle: false, messages: ccMomentaryMessages 14 93 }
      , emptySwitch
      , emptySwitch
      , emptySwitch
      ]
  }
