module Data.MC6.Message
  ( emptyMessage
  , ccMessage
  , pcMessage
  , bankJumpMessage
  , delayMessage
  , engagePresetMessage
  ) where

import Data.MC6.Types (MC6Action(..), MC6Message, MC6MsgType(..), MC6TogglePosition(..))

-- | Empty message slot at the given index
emptyMessage :: Int -> MC6Message
emptyMessage idx =
  { msgType: MsgEmpty
  , channel: 1
  , data1: 0
  , data2: 0
  , data3: 0
  , data4: 0
  , action: ActionNone
  , togglePosition: ToggleBoth
  , msgIndex: idx
  }

-- | CC message: channel (1-16), cc number, value, action
ccMessage :: Int -> Int -> Int -> MC6Action -> MC6Message
ccMessage ch cc val action =
  { msgType: MsgCC
  , channel: ch
  , data1: cc
  , data2: val
  , data3: 0
  , data4: 0
  , action
  , togglePosition: ToggleBoth
  , msgIndex: 0
  }

-- | PC message: channel (1-16), program number, action
pcMessage :: Int -> Int -> MC6Action -> MC6Message
pcMessage ch program action =
  { msgType: MsgPC
  , channel: ch
  , data1: program
  , data2: 0
  , data3: 0
  , data4: 0
  , action
  , togglePosition: ToggleBoth
  , msgIndex: 0
  }

-- | Bank jump: target bank (0-29), action
bankJumpMessage :: Int -> MC6Action -> MC6Message
bankJumpMessage bank action =
  { msgType: MsgBankJump
  , channel: 1
  , data1: bank
  , data2: 0
  , data3: 0
  , data4: 0
  , action
  , togglePosition: ToggleBoth
  , msgIndex: 0
  }

-- | Delay in milliseconds (stored in data1)
delayMessage :: Int -> MC6Message
delayMessage ms =
  { msgType: MsgDelay
  , channel: 1
  , data1: ms
  , data2: 0
  , data3: 0
  , data4: 0
  , action: ActionNone
  , togglePosition: ToggleBoth
  , msgIndex: 0
  }

-- | Engage preset: target preset index (0-11), action
engagePresetMessage :: Int -> MC6Action -> MC6Message
engagePresetMessage preset action =
  { msgType: MsgEngagePreset
  , channel: 1
  , data1: preset
  , data2: 0
  , data3: 0
  , data4: 0
  , action
  , togglePosition: ToggleBoth
  , msgIndex: 0
  }
