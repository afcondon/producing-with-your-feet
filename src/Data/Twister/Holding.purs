-- | **Who has the Twister, as the nav says it** (plan `pwyf-layout.md`, step 1;
-- | `quadrat-the-sampler.md`, "After phase 4").
-- |
-- | Three states, because there are three things a player needs to know before
-- | turning a knob: there is nothing to turn, the knobs reach the pedals, or
-- | the knobs reach the looper. Who holds it is decided by the MC6's bank
-- | (`Data.Looper.TwisterHolder`), never by this page, which is why the mark
-- | has no "take it back" button: the MC6 decides.
module Data.Twister.Holding
  ( TwisterHolding(..)
  , twisterHolding
  , holdingWord
  , holdingSentence
  ) where

import Prelude

import Data.Maybe (Maybe(..))

data TwisterHolding
  = TwisterNotConnected
  -- | This page holds it: the knobs reach the focused pedal or a scene.
  | TwisterWithPedals
  -- | The looper's process holds it, because the MC6 is on the Loops bank.
  | TwisterWithLooper
  -- | Some other client of Helm's hub holds it, by the name it gave the hub:
  -- | a second PWYF tab, or anything else that took it. Said by name, so a
  -- | second tab is never mistaken for the looper.
  | TwisterWithOther String

derive instance eqTwisterHolding :: Eq TwisterHolding

instance showTwisterHolding :: Show TwisterHolding where
  show = case _ of
    TwisterNotConnected -> "TwisterNotConnected"
    TwisterWithPedals -> "TwisterWithPedals"
    TwisterWithLooper -> "TwisterWithLooper"
    TwisterWithOther who -> "(TwisterWithOther " <> show who <> ")"

-- | Derived from what the app already keeps: whether the Twister's output is
-- | open, whether this connection holds it on Helm's hub (`twisterHeld`,
-- | always true through plain Web MIDI, where there is no one to hold it
-- | from), and the holder's name as the hub last said it (`twisterHolder`).
-- |
-- | **The looper by name, not by elimination.** A second PWYF tab can hold the
-- | Twister too, so "not held here" is not enough to say "the looper";
-- | the hub's claims carry the holder's name, and that is what is read.
-- | Nobody holding it is momentary (this page then takes it), and is shown as
-- | this page's rather than as a fourth thing to explain.
twisterHolding
  :: { connected :: Boolean, heldHere :: Boolean, holder :: Maybe String, looper :: String }
  -> TwisterHolding
twisterHolding r
  | not r.connected = TwisterNotConnected
  | r.heldHere = TwisterWithPedals
  | otherwise = case r.holder of
      Just who | who == r.looper -> TwisterWithLooper
      Just who -> TwisterWithOther who
      Nothing -> TwisterWithPedals

-- | The mark's word, beside its dot.
holdingWord :: TwisterHolding -> String
holdingWord = case _ of
  TwisterNotConnected -> "not connected"
  TwisterWithPedals -> "pedals"
  TwisterWithLooper -> "looper"
  TwisterWithOther who -> who

-- | The sentence on hover, and on anything drawn greyed while the looper has
-- | it, so the reason is the same words wherever it is given.
holdingSentence :: TwisterHolding -> String
holdingSentence = case _ of
  TwisterNotConnected -> "Twister: not connected"
  TwisterWithPedals -> "Twister: with this app (the knobs reach the pedals)"
  TwisterWithLooper -> "Twister: with the looper (the MC6 is on the Loops bank)"
  TwisterWithOther who -> "Twister: held by " <> who <> " (another app on Helm's hub)"
