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

data TwisterHolding
  = TwisterNotConnected
  -- | This page holds it: the knobs reach the focused pedal or a scene.
  | TwisterWithPedals
  -- | The looper's process holds it, because the MC6 is on the Loops bank.
  | TwisterWithLooper

derive instance eqTwisterHolding :: Eq TwisterHolding

instance showTwisterHolding :: Show TwisterHolding where
  show = case _ of
    TwisterNotConnected -> "TwisterNotConnected"
    TwisterWithPedals -> "TwisterWithPedals"
    TwisterWithLooper -> "TwisterWithLooper"

-- | Derived from what the app already keeps, not from a field of its own:
-- | whether the Twister's output is open, and whether this page holds it on
-- | Helm's hub (`twisterHeld`, always true through plain Web MIDI, where there
-- | is no one to hold it from).
-- |
-- | **Not held here is read as the looper's.** The hub's table has exactly two
-- | holders for the Twister, this page and the looper's process, so a Twister
-- | that is connected and not ours is the looper's. The one case that reads
-- | wrong is a second PWYF tab holding it, which the hub allows and nothing
-- | here can tell apart without asking the hub for the holder's name.
twisterHolding :: { connected :: Boolean, heldHere :: Boolean } -> TwisterHolding
twisterHolding = case _ of
  { connected: false } -> TwisterNotConnected
  { heldHere: true } -> TwisterWithPedals
  _ -> TwisterWithLooper

-- | The mark's word, beside its dot.
holdingWord :: TwisterHolding -> String
holdingWord = case _ of
  TwisterNotConnected -> "not connected"
  TwisterWithPedals -> "pedals"
  TwisterWithLooper -> "looper"

-- | The sentence on hover, and on anything drawn greyed while the looper has
-- | it, so the reason is the same words wherever it is given.
holdingSentence :: TwisterHolding -> String
holdingSentence = case _ of
  TwisterNotConnected -> "Twister: not connected"
  TwisterWithPedals -> "Twister: with this app (the knobs reach the pedals)"
  TwisterWithLooper -> "Twister: with the looper (the MC6 is on the Loops bank)"
