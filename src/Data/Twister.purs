-- | The Midifighter Twister, as messages.
-- |
-- | Sixteen encoders, each of which is **both a knob and a button**, in four
-- | banks — and the banks are the part this module was blind to until
-- | 2026-08-25. The device shifts the CC by sixteen per bank, so bank *b*
-- | encoder *i* arrives as CC `16b + i`; this treated the CC *as* the encoder
-- | index, so banks 2, 3 and 4 indexed past the end of a sixteen-slot array and
-- | vanished without a word. Three quarters of the controller was inert and
-- | nothing said so.
-- |
-- | **The device owns which bank is showing, and the CC carries it.** That is
-- | worth stating because the alternative was seriously considered and is
-- | worse: the app could track the bank itself and drive it with the side
-- | buttons, which would make the bank a piece of state in two places, able to
-- | disagree, with no way to ask the device which it believed. Reading it off
-- | every message costs nothing and cannot drift.
module Data.Twister
  ( TwisterEncoder(..)
  , TwisterButton(..)
  , TwisterMapping
  , TwisterMsg(..)
  , Knob
  , SideBtn(..)
  , banks
  , encodersPerBank
  , bankOf
  , bankSelectMessage
  , parseTwisterMsg
  ) where

import Prelude

import Data.Array as Array
import Data.Int.Bits (and)
import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue)

data TwisterEncoder
  = TwisterCC { cc :: CC, center :: Maybe MidiValue, options :: Maybe (Array MidiValue) }

data TwisterButton
  = TwisterToggle { cc :: CC }
  | TwisterMomentary { cc :: CC }
  | TwisterSet { cc :: CC, value :: MidiValue }

-- | A pedal's sixteen encoders. **Bank one only**, and deliberately: a pedal is
-- | a page of knobs and the twelve of them that have mappings all fit on one.
-- | The looper is the surface that needed more, and it has its own table in
-- | `Data.Looper.Twister` rather than four of these.
type TwisterMapping =
  { hue :: Int
  , encoders :: Array (Maybe TwisterEncoder)
  , buttons :: Array (Maybe TwisterButton)
  }

-- | How many pages the device has, and how many encoders on each.
-- |
-- | Both are facts about the hardware rather than choices, which is why they
-- | are here and not in a layout table.
banks :: Int
banks = 4

encodersPerBank :: Int
encodersPerBank = 16

-- | Ask the device to show a page.
-- |
-- | **Unverified, and labelled as such everywhere it is used.** The Midifighter
-- | is documented as taking a bank change on channel 4 — CC 0 to 3, value 127,
-- | for banks one to four — but nothing in this repo has ever sent one and
-- | nobody here has watched it land. It is written down as a candidate rather
-- | than as a fact, in the same spirit as the hues in `Data.Looper.Twister`.
-- |
-- | It is cheap to settle: send it, then turn any encoder. The bank travels in
-- | every message the device sends, so `bankOf` on the next turn says which
-- | page the device believes it is on, and that is an observation rather than a
-- | claim. `Component.App` keeps that answer and the Looper page prints it.
-- |
-- | If it turns out to do nothing, the side buttons remain the only way and
-- | this should be deleted rather than left lying about looking official.
bankSelectMessage :: Int -> Array Int
bankSelectMessage bank = [ 0xB3, clamp 0 (banks - 1) bank, 127 ]

-- | Which page a raw message came from, for anything that wants to know where
-- | the device is without decoding what it said.
-- |
-- | The one *certain* answer to "which page am I on": it is in every message.
bankOf :: Array Int -> Maybe Int
bankOf bytes = do
  status <- Array.index bytes 0
  cc <- Array.index bytes 1
  let channel = (and status 0x0F) + 1
  -- Only the encoder channels carry a bank. The side buttons are on channel 5
  -- with a CC block of their own, and reading a bank out of one would be
  -- reading a number that means something else.
  if channel == 1 || channel == 2 then Just (cc / encodersPerBank) else Nothing

-- | Which encoder, on which page. Zero-based on both counts — the device's own
-- | numbering is one-based on the bank, and converting once here is cheaper
-- | than remembering which convention a given table is written in.
type Knob = { bank :: Int, index :: Int }

data TwisterMsg
  = EncoderTurn Knob Int       -- value 0-127
  | EncoderPress Knob
  | EncoderRelease Knob
  | SideButton SideBtn

data SideBtn = PrevPedal | NextPedal | RefreshLEDs

parseTwisterMsg :: Array Int -> Maybe TwisterMsg
parseTwisterMsg bytes = do
  status <- Array.index bytes 0
  cc <- Array.index bytes 1
  val <- Array.index bytes 2
  let channel = (and status 0x0F) + 1 -- 1-indexed
      knob = { bank: cc / encodersPerBank, index: cc `mod` encodersPerBank }
  case channel of
    1 -> Just (EncoderTurn knob val)
    2
      | val == 127 -> Just (EncoderPress knob)
      | otherwise -> Just (EncoderRelease knob)
    -- The three side buttons, which are **not** the bank switches: those the
    -- device handles by itself and never tells us about, except by the CC block
    -- everything afterwards arrives in.
    5 -> case cc of
      8 -> Just (SideButton PrevPedal)
      9 -> Just (SideButton NextPedal)
      10 -> Just (SideButton RefreshLEDs)
      _ -> Nothing
    _ -> Nothing
