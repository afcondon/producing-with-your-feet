-- | What an MC6 preset *means*, as opposed to what it weighs on the wire.
-- |
-- | The wire type (`Data.MC6.Types.MC6Message`) is a record of four anonymous
-- | integers serving thirty-six message types. `data1` is a CC number, or a
-- | program number, or a bank, or a delay in milliseconds, depending on a tag
-- | in another field — so every reader has to know the tag's meaning and no
-- | reader is made to prove it does. That representation has now produced four
-- | silent failures in this project, and this module is the answer to all of
-- | them: **parse into a type that cannot hold a wrong combination, or refuse.**
-- |
-- | ## The refusal is the point
-- |
-- | `fromWire` produces a precise constructor only when the *whole* wire record
-- | is exactly what that constructor implies — every byte the constructor does
-- | not carry must be zero. Anything else becomes `Raw`, holding the bytes
-- | verbatim.
-- |
-- | That is stricter than it sounds, and deliberately so. It means:
-- |
-- | - `toWire <<< fromWire` is the identity on every message, including ones we
-- |   do not understand, so the model can be introduced without risking a
-- |   single byte of a device somebody has spent years programming;
-- | - the number of messages that land in `Raw` is a **measurement of how much
-- |   of this device we actually understand**, which can be printed, watched,
-- |   and driven down deliberately;
-- | - nothing is ever quietly reinterpreted. A bank jump whose `data3` we
-- |   cannot explain stays raw rather than being read as a jump we would then
-- |   confidently rewrite wrongly.
-- |
-- | The last of those is not hypothetical. The device's own bank jumps carry
-- | `data3 = 6` and `data1 = 0`; ours carry `data1 = bank` and `data3 = 0`.
-- | Both are called `MsgBankJump`. Until that is settled by experiment rather
-- | than by reading, only the shape *we* emit parses as `JumpToBank`, and the
-- | device's own stay `Raw` — which is the honest statement of what is known.
-- |
-- | ## Names cannot be over-long, rather than being truncated
-- |
-- | `SysEx.shortNameTLV` does `SCU.take 8` and `longNameTLV` does `SCU.take 24`.
-- | An over-long label therefore reaches the device meaning something other
-- | than what was written, with no error anywhere. Here the limits are in the
-- | types: `shortName` returns `Nothing` rather than a shortened string, and
-- | shortening is a separate function you have to *ask* for by name.
-- |
-- | Limits are taken from a real device rather than from the manual: across the
-- | 360 presets of the March backup, short and toggle names run to 8, long
-- | names to 24, bank names to 16.
module Data.MC6.Model
  ( ShortName
  , shortName
  , clipShortName
  , unShortName
  , LongName
  , longName
  , clipLongName
  , unLongName
  , BankName
  , bankName
  , clipBankName
  , unBankName
  , BankNumber
  , bankNumber
  , unBankNumber
  , bankCount
  , SwitchIndex
  , switchIndex
  , unSwitchIndex
  , switchesPerBank
  , SlotIndex
  , slotIndex
  , unSlotIndex
  , slotsPerSwitch
  , Emit(..)
  , fromWire
  , toWire
  , isRaw
  , census
  ) where

import Prelude

import Data.Array as Array
import Data.MC6.Types (MC6Action, MC6Message, MC6MsgType(..), MC6TogglePosition)
import Data.Maybe (Maybe(..))
import Data.Midi (CC, Channel, MidiValue, ProgramNumber, makeCC, makeChannel, makeMidiValue, makeProgramNumber, unCC, unChannel, unMidiValue, unProgramNumber)
import Data.String.CodeUnits as SCU

-- Sizes, measured ------------------------------------------------------------

-- | Banks on an MC6 MKII, numbered 0-29 on the wire and one higher in the
-- | editor. Both numbers appear in this project; this is always the wire one.
bankCount :: Int
bankCount = 30

-- | Switches per bank: six on the unit, two FS3X's worth beyond it.
switchesPerBank :: Int
switchesPerBank = 12

-- | Message slots per switch. The device pads to this and `SysEx.sysexPresetData`
-- | drops the excess with `Array.take 16` without saying so, which is the
-- | overflow this count exists to make countable.
slotsPerSwitch :: Int
slotsPerSwitch = 16

-- Names ----------------------------------------------------------------------

-- | A label the device can show on a switch: at most eight characters.
newtype ShortName = ShortName String

derive newtype instance Eq ShortName
derive newtype instance Ord ShortName
derive newtype instance Show ShortName

-- | Refuses what will not fit, rather than quietly shortening it.
shortName :: String -> Maybe ShortName
shortName s = if SCU.length s <= 8 then Just (ShortName s) else Nothing

-- | Shorten deliberately. Exists so that the loss is something a caller asked
-- | for at a place you can grep, rather than something an encoder did.
clipShortName :: String -> ShortName
clipShortName = ShortName <<< SCU.take 8

unShortName :: ShortName -> String
unShortName (ShortName s) = s

newtype LongName = LongName String

derive newtype instance Eq LongName
derive newtype instance Ord LongName
derive newtype instance Show LongName

longName :: String -> Maybe LongName
longName s = if SCU.length s <= 24 then Just (LongName s) else Nothing

clipLongName :: String -> LongName
clipLongName = LongName <<< SCU.take 24

unLongName :: LongName -> String
unLongName (LongName s) = s

newtype BankName = BankName String

derive newtype instance Eq BankName
derive newtype instance Ord BankName
derive newtype instance Show BankName

bankName :: String -> Maybe BankName
bankName s = if SCU.length s <= 16 then Just (BankName s) else Nothing

clipBankName :: String -> BankName
clipBankName = BankName <<< SCU.take 16

unBankName :: BankName -> String
unBankName (BankName s) = s

-- Positions ------------------------------------------------------------------

-- | Which bank, 0-29. Wire numbering.
newtype BankNumber = BankNumber Int

derive newtype instance Eq BankNumber
derive newtype instance Ord BankNumber
derive newtype instance Show BankNumber

bankNumber :: Int -> Maybe BankNumber
bankNumber n = if n >= 0 && n < bankCount then Just (BankNumber n) else Nothing

unBankNumber :: BankNumber -> Int
unBankNumber (BankNumber n) = n

-- | Which switch, 0-11 — A through L.
newtype SwitchIndex = SwitchIndex Int

derive newtype instance Eq SwitchIndex
derive newtype instance Ord SwitchIndex
derive newtype instance Show SwitchIndex

switchIndex :: Int -> Maybe SwitchIndex
switchIndex n = if n >= 0 && n < switchesPerBank then Just (SwitchIndex n) else Nothing

unSwitchIndex :: SwitchIndex -> Int
unSwitchIndex (SwitchIndex n) = n

-- | Which message slot, 0-15.
newtype SlotIndex = SlotIndex Int

derive newtype instance Eq SlotIndex
derive newtype instance Ord SlotIndex
derive newtype instance Show SlotIndex

slotIndex :: Int -> Maybe SlotIndex
slotIndex n = if n >= 0 && n < slotsPerSwitch then Just (SlotIndex n) else Nothing

unSlotIndex :: SlotIndex -> Int
unSlotIndex (SlotIndex n) = n

-- What a message does --------------------------------------------------------

-- | One thing a switch emits.
-- |
-- | Each constructor carries exactly the fields its message type has, so the
-- | four-anonymous-integers problem does not survive into anything that reads
-- | this. `Nothing` here — a CC with no value, a jump with no bank — is not
-- | representable rather than being zero.
data Emit
  = Silent
  | SendCC Channel CC MidiValue
  | SendPC Channel ProgramNumber
  | JumpToBank BankNumber
  | EngageSwitch SwitchIndex
  | Pause Int
  -- | Not understood, and *said so*. Carries the wire bytes verbatim, which is
  -- | what lets an unmodelled message survive a read/write cycle untouched
  -- | while still being visibly unmodelled to anything that pattern matches.
  | Raw
      { msgType :: MC6MsgType
      , channel :: Int
      , data1 :: Int
      , data2 :: Int
      , data3 :: Int
      , data4 :: Int
      }

derive instance Eq Emit

isRaw :: Emit -> Boolean
isRaw = case _ of
  Raw _ -> true
  _ -> false

-- | How much of a device we understand, as two numbers.
-- |
-- | Worth printing rather than inferring: "412 of 5760 modelled" is a fact
-- | about our grasp of the protocol that changes as the model grows, and a
-- | number that goes *down* after a change is a regression nobody would
-- | otherwise notice.
census :: Array Emit -> { modelled :: Int, raw :: Int }
census es =
  { modelled: Array.length (Array.filter (not <<< isRaw) es)
  , raw: Array.length (Array.filter isRaw es)
  }

-- | Read a wire message, refusing to guess.
-- |
-- | Total, and total in the strong sense: every input produces a value, and
-- | `toWire` of that value is the input again. The `Raw` fallthrough is what
-- | buys that, and every precise case below insists that the bytes it does not
-- | carry are zero — otherwise the identity would not hold and the model would
-- | be quietly discarding a field the device cares about.
fromWire :: MC6Message -> Emit
fromWire m = case m.msgType of
  -- Channel 1 as well as zero data, because `toWire` has to put *some* channel
  -- back and an empty slot carrying channel 2 would round-trip to channel 1.
  -- All 5649 empty slots of the March backup are channel 1, so this costs
  -- nothing and closes the hole.
  MsgEmpty | blank && m.channel == 1 -> Silent

  MsgCC | m.data3 == 0 && m.data4 == 0 ->
    case makeChannel m.channel, makeCC m.data1, makeMidiValue m.data2 of
      Just ch, Just c, Just v -> SendCC ch c v
      _, _, _ -> raw

  MsgPC | m.data2 == 0 && m.data3 == 0 && m.data4 == 0 ->
    case makeChannel m.channel, makeProgramNumber m.data1 of
      Just ch, Just p -> SendPC ch p
      _, _ -> raw

  -- Only the shape this app emits. The device writes its own jumps with
  -- `data3 = 6` and `data1 = 0`, which is a different message wearing the same
  -- type byte; until that is settled by stomping one rather than by reading
  -- about it, those stay raw and keep working exactly as they did.
  MsgBankJump | m.channel == 1 && m.data2 == 0 && m.data3 == 0 && m.data4 == 0 ->
    case bankNumber m.data1 of
      Just b -> JumpToBank b
      Nothing -> raw

  MsgEngagePreset | m.channel == 1 && m.data2 == 0 && m.data3 == 0 && m.data4 == 0 ->
    case switchIndex m.data1 of
      Just s -> EngageSwitch s
      Nothing -> raw

  MsgDelay | m.channel == 1 && m.data2 == 0 && m.data3 == 0 && m.data4 == 0 ->
    Pause m.data1

  _ -> raw

  where
  blank = m.data1 == 0 && m.data2 == 0 && m.data3 == 0 && m.data4 == 0

  raw = Raw
    { msgType: m.msgType
    , channel: m.channel
    , data1: m.data1
    , data2: m.data2
    , data3: m.data3
    , data4: m.data4
    }

-- | Write a message back to the wire.
-- |
-- | Action, toggle position and slot index are carried separately rather than
-- | inside `Emit`, because they are facts about *when* a switch fires a
-- | message, not about what the message is — and the same emit under a press
-- | and under a release is one thing said twice, not two things.
toWire :: MC6Action -> MC6TogglePosition -> Int -> Emit -> MC6Message
toWire action togglePosition msgIndex = case _ of
  Silent -> base MsgEmpty 1 0 0 0 0
  SendCC ch c v -> base MsgCC (unChannel ch) (unCC c) (unMidiValue v) 0 0
  SendPC ch p -> base MsgPC (unChannel ch) (unProgramNumber p) 0 0 0
  JumpToBank b -> base MsgBankJump 1 (unBankNumber b) 0 0 0
  EngageSwitch s -> base MsgEngagePreset 1 (unSwitchIndex s) 0 0 0
  Pause ms -> base MsgDelay 1 ms 0 0 0
  Raw r -> base r.msgType r.channel r.data1 r.data2 r.data3 r.data4
  where
  base msgType channel data1 data2 data3 data4 =
    { msgType, channel, data1, data2, data3, data4
    , action, togglePosition, msgIndex
    }
