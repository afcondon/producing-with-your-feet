-- | The Midifighter Twister, as this app binds it.
-- |
-- | **The device itself is Helm's** (`Helm.Twister`): how its messages decode,
-- | the CC-per-layer arithmetic, the lights, and the press-nudge guard. What
-- | stays here is what a knob means to PWYF — a pedal's encoder table, the
-- | side buttons' jobs, and which layer the device is pinned to.
-- |
-- | **The layer is the device's, and the CC carries it.** Bank *b* encoder *i*
-- | arrives as CC `16b + i`; reading the layer off every message costs nothing
-- | and cannot drift, where tracking it app-side would be one fact in two
-- | places with no way to ask the device which it believed. (Until 2026-08-25
-- | this treated the CC *as* the encoder index, and three quarters of the
-- | controller vanished without a word.)
module Data.Twister
  ( TwisterEncoder(..)
  , TwisterButton(..)
  , encoderCC
  , buttonCC
  , TwisterMapping
  , TwisterMsg(..)
  , Knob
  , SideBtn(..)
  , banks
  , deviceBank
  , encodersPerBank
  , bankOf
  , bankSelectMessage
  , fromHelm
  , toHelmKnob
  ) where

import Data.Maybe (Maybe(..))
import Data.Midi (CC, MidiValue)
import Helm.Twister as Helm

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
-- | The CC a control speaks on, whatever kind it is.
-- |
-- | Wanted by anything that has to *name* a cell rather than act on it: a
-- | pedal's labels are keyed by CC, so this is the join between a Twister page
-- | and the words a player would recognise.
encoderCC :: TwisterEncoder -> CC
encoderCC (TwisterCC r) = r.cc

buttonCC :: TwisterButton -> CC
buttonCC = case _ of
  TwisterToggle r -> r.cc
  TwisterMomentary r -> r.cc
  TwisterSet r -> r.cc

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
banks = Helm.layers

encodersPerBank :: Int
encodersPerBank = Helm.encodersPerLayer

-- | **The one block this app ever puts the Twister on.**
-- |
-- | The device has four, and for a while the looper used them as its pages: a
-- | page turn sent `bankSelectMessage` and the LED writes followed the device
-- | across. That is where the pager kept dying, and the reason is a hardware
-- | fact the design never took account of — *the Twister stores a value per
-- | encoder per block*. Four stores behind one knob, and they survive a reload.
-- | So paging also swapped which store the pager read from, the new one held
-- | whatever it had last been left at, and "the device owns the position"
-- | quietly meant a different position each time.
-- |
-- | Mirroring the value into the other blocks was the obvious repair and it is
-- | the wrong one: four copies of one fact kept in step by hand, and kept in
-- | step *while* a hand is turning one of them.
-- |
-- | So the device stays here and the app owns the paging outright. This is not
-- | a new position, it is the one the rest of the code already took —
-- | `App.onPage` throws the device's block away before deciding what an encoder
-- | means, and `sendAllLEDs` has only ever written to block 0. The looper was
-- | the one surface disagreeing.
-- |
-- | It lives here rather than beside the MIDI writes because two places need
-- | it and one of them is a view: the card has to know that a heard block other
-- | than this one means the encoders are misaddressed, which is not the same
-- | question as which *page* the app is showing. Conflating those two is what
-- | made the card's warning fire on every visit to page 2.
deviceBank :: Int
deviceBank = 0

-- | Ask the device to show a page: channel 4, CC 0 to 3, value 127.
-- |
-- | Verified on hardware 2026-10-17. Press one of the device's own bank
-- | buttons, use an encoder there, and the pin in `App.TwisterMidiReceived`
-- | sends this and the device comes straight back to `deviceBank`. The bank
-- | buttons themselves send nothing, so the pin acts on the first message
-- | from the other bank, not on the press.
bankSelectMessage :: Int -> Array Int
bankSelectMessage = Helm.layerSelect

-- | Which page a raw message came from, for anything that wants to know where
-- | the device is without decoding what it said.
-- |
-- | The one *certain* answer to "which page am I on": it is in every message.
bankOf :: Array Int -> Maybe Int
bankOf = Helm.layerOf

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

-- | One message the guard let through, in this app's terms. The device's
-- | layer is carried as `bank`, which is what the rest of the app calls it
-- | (and, after `App.onPage`, the looper page — not the same thing).
fromHelm :: Helm.TwisterMsg -> Maybe TwisterMsg
fromHelm = case _ of
  Helm.Turn k v -> Just (EncoderTurn (knob k) v)
  Helm.Press k -> Just (EncoderPress (knob k))
  Helm.Release k -> Just (EncoderRelease (knob k))
  -- The three side buttons PWYF binds, which are **not** the layer switches:
  -- those the device handles by itself and never tells us about, except by
  -- the CC block everything afterwards arrives in.
  Helm.Side 0 -> Just (SideButton PrevPedal)
  Helm.Side 1 -> Just (SideButton NextPedal)
  Helm.Side 2 -> Just (SideButton RefreshLEDs)
  Helm.Side _ -> Nothing
  where
  knob k = { bank: k.layer, index: k.index }

-- | The device's knob, from this app's.
toHelmKnob :: Knob -> Helm.Knob
toHelmKnob k = { layer: k.bank, index: k.index }
