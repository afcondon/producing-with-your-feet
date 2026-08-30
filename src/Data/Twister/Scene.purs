-- | A Twister page whose cells each name the pedal they speak to.
-- |
-- | ## Why this is not a new kind of page
-- |
-- | A pedal's `TwisterMapping` leaves the pedal implicit, because a mapping is
-- | owned by a pedal and there is only one it could mean. That is exactly right
-- | for a pedal's own page and exactly wrong for a **live-controls page**, which
-- | wants Onward's two switches beside MOOD's beside Lost+Found's.
-- |
-- | The obvious repair was to put a pedal on every cell of `TwisterEncoder` and
-- | `TwisterButton`. It is the wrong one: those tables are generated into JSON
-- | and decoded back, so every mapping and every line of config would have grown
-- | a field that says `Nothing` — and a field that is almost always absent is a
-- | field nobody keeps true.
-- |
-- | So the pedal is bound **where the surface is resolved**, not in the table.
-- | Standing on a pedal's page the answer comes from focus; standing on a scene
-- | it comes from the cell. `ofPedal` is the first of those written down, and it
-- | is what makes the two kinds of page one kind: everything downstream takes a
-- | `Scene` and never asks which sort it came from.
-- |
-- | ## What a scene deliberately cannot do
-- |
-- | Reach the looper. A scene is CCs to pedals, and Itajara is not a pedal —
-- | `Pedals.Itajara.twister` is `Nothing` on purpose, because filling it in
-- | would install a second route to the daemon beside `Machine.perform`. The
-- | looper's surface is `Data.Looper.Twister`, which speaks `Duty` and shares
-- | nothing with this but the hardware.
module Data.Twister.Scene
  ( Bound
  , Scene
  , ofPedal
  , encoderAt
  , buttonAt
  , pedalsIn
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Pedal (PedalId)
import Data.Twister (TwisterButton, TwisterEncoder, TwisterMapping)

-- | A control, and the pedal it speaks to.
-- |
-- | The pair travels together for the same reason a CC and its channel do:
-- | either half alone will address *something*, silently and wrongly. The
-- | alternative considered was a parallel `owners` array beside `encoders` —
-- | two arrays that must stay the same length, which is the shape this project
-- | has already been bitten by, where two fields describe one fact and drift.
type Bound a = { pedal :: PedalId, control :: a }

-- | Sixteen encoders and sixteen buttons, each free to name a different pedal.
-- |
-- | `hue` is the page's colour and comes from where a pedal mapping's does.
-- | `name` is new, because a scene has no owner to borrow a name from and the
-- | surface has to be able to say what you are standing on.
type Scene =
  { name :: String
  , hue :: Int
  , encoders :: Array (Maybe (Bound TwisterEncoder))
  , buttons :: Array (Maybe (Bound TwisterButton))
  }

-- | A pedal's own page, read as a scene: every cell names its owner.
-- |
-- | This is the whole of the widening. No mapping changed, no generated JSON
-- | changed and no existing signature changed — a pedal page gains the ability
-- | to be treated as a mixed one by being *told* the thing it always knew.
ofPedal :: PedalId -> String -> TwisterMapping -> Scene
ofPedal pedal name tw =
  { name
  , hue: tw.hue
  , encoders: map (map bindTo) tw.encoders
  , buttons: map (map bindTo) tw.buttons
  }
  where
  bindTo :: forall a. a -> Bound a
  bindTo control = { pedal, control }

-- | The control at an index, or nothing.
-- |
-- | An index past the end and a cell that is deliberately dark give the same
-- | answer, because they are the same thing to a hand: a knob that does nothing.
encoderAt :: Scene -> Int -> Maybe (Bound TwisterEncoder)
encoderAt s i = join (Array.index s.encoders i)

buttonAt :: Scene -> Int -> Maybe (Bound TwisterButton)
buttonAt s i = join (Array.index s.buttons i)

-- | Every pedal this scene touches, without repeats.
-- |
-- | Wanted by whoever gathers state before the page can be drawn: a scene's
-- | lights come from the values of as many pedals as it names, where a pedal
-- | page's come from one. The two arrays hold different control types, so each
-- | is reduced to the only field this question asks about before they meet.
pedalsIn :: Scene -> Array PedalId
pedalsIn s = Array.nub
  (map _.pedal (Array.catMaybes s.encoders) <> map _.pedal (Array.catMaybes s.buttons))
