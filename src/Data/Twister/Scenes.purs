-- | The live-controls pages: the knobs worth reaching for mid-phrase.
-- |
-- | ## Why these are picks and not a new table of CCs
-- |
-- | See `Data.Twister.Scene.Pick`. Every entry below borrows a control from the
-- | pedal's own Twister page, so "MOOD's Clock" is described once, in
-- | `Pedals.Mood`, with the centre it detents to and the options it steps
-- | through. A scene only says *where a hand should find it*.
-- |
-- | ## Why a scene is one group's pedals and not all of them
-- |
-- | The control page a scene attaches to is a functional group —
-- | `docs/DESIGN-BANKS.md` on why the pages are grouped by what a pedal is
-- | *for* rather than by pedal. Standing there, the feet are on that group's
-- | bypasses, so the hands should be on the same group's knobs. A scene that
-- | also carried the delay pedals would be a page where half the knobs act on
-- | pedals the feet cannot reach, which is the back-and-forth the whole
-- | arrangement exists to remove.
module Data.Twister.Scenes
  ( ambient
  , sceneForControlBank
  , scenes
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Pedal (PedalId(..))
import Data.Twister.Scene (SceneDef)

mood :: PedalId
mood = PedalId "mood"

onward :: PedalId
onward = PedalId "onward"

lostAndFound :: PedalId
lostAndFound = PedalId "lostandfound"

habit :: PedalId
habit = PedalId "habit"

-- | `Just` a borrowing, said shortly, because the table below is the content
-- | and sixteen `Just { pedal: _, index: _ }`s would bury it.
at :: PedalId -> Int -> Maybe { pedal :: PedalId, index :: Int }
at pedal index = Just { pedal, index }

-- | The ambient page: **MOOD, Onward, Lost+Found and Habit**, a row each.
-- |
-- | The four are not a taste; they are the group `docs/DESIGN-BANKS.md` calls
-- | *ambient / evolving*, and the control page they share is the bank this
-- | scene answers to. The first draft of this had three of them, because the
-- | six expander switches hold three pedals' worth of channels — and that is a
-- | fact about the **feet**. Habit engages with one switch rather than two, so
-- | it sits on the unit's own six and is every bit as much part of the group.
-- | Leaving its knobs off would have made the hand's page disagree with the
-- | page it is attached to.
-- |
-- | ## The grid
-- |
-- | A row per pedal, three knobs and a switch, which is the shape a pedal's own
-- | page already uses — column four is where a switch lives, learned once. Four
-- | pedals, four rows, and the colour of a row is its pedal's own, which is the
-- | only thing on the device that says whose knob is under your hand.
-- |
-- | ## The knobs, and why each
-- |
-- | - **MOOD Clock** is the sample rate, and the one control on that pedal that
-- |   changes what kind of machine it is rather than how much of it you hear.
-- |   **Length** is the micro-looper's slice. **Mix** last, because it is what
-- |   you reach for to get out of trouble.
-- | - **Onward Error** is the glitch, which is why the pedal is on the board.
-- |   **Texture** and **Size** shape what it glitches.
-- | - **Lost+Found Spill**, **Glue** and **Blend** decide how much of the
-- |   wreckage comes back.
-- | - **Habit Modify** and **Scan** are the two that move its window through
-- |   what it has already heard, which is the whole trick; **Spread** is where
-- |   that window sits.
-- |
-- | **Nothing here is precious.** These are opening bids on four pedals with
-- | enormous control surfaces; changing one is a line, and changing one cannot
-- | break the pedal it borrows from.
ambient :: SceneDef
ambient =
  { name: "Ambient"
  , hue: 110
  , encoders:
      [ at mood 5, at mood 2, at mood 1, Nothing              -- Clock, Length, Mix
      , at onward 4, at onward 6, at onward 0, Nothing        -- Error, Texture, Size
      , at lostAndFound 9, at lostAndFound 13, at lostAndFound 5, Nothing
                                                              -- Spill, Glue, Blend
      , at habit 4, at habit 6, at habit 5, Nothing           -- Modify, Scan, Spread
      ]
  , buttons:
      [ Nothing, Nothing, Nothing, at mood 3                  -- Micro-Looper
      , Nothing, Nothing, Nothing, at onward 3                -- Glitch
      , Nothing, Nothing, Nothing, at lostAndFound 3          -- Left
      , Nothing, Nothing, Nothing, at habit 3                 -- Mode
      ]
  }

-- | Every scene this rig knows, in the order a menu would list them.
-- |
-- | One so far. It is a list rather than a single value because the second is
-- | expected — the delay/reverb group is the obvious next — and because
-- | whatever associates a scene with a page needs something to look through.
scenes :: Array SceneDef
scenes = [ ambient ]

-- | Which scene a **control page** calls up, by its id.
-- |
-- | Keyed on the page's id and not its bank number, which was the first
-- | attempt and was wrong twice over. Control banks are stored, editable data —
-- | the user can move one — and the block bases in `docs/DESIGN-BANKS.md` are
-- | explicitly not fixed yet, so a number written here would be a number that
-- | goes stale in two different ways. An id survives both.
-- |
-- | `Nothing` is the ordinary answer and means "leave the Twister alone": the
-- | looper's pages or the focused pedal's, as before. Only a page that has
-- | given its switches to a group of pedals asks for a scene, because only
-- | there is the hand's surface a foregone conclusion.
sceneForControlBank :: String -> Maybe SceneDef
sceneForControlBank id
  | id == "control-ambient" = Just ambient
  | otherwise = Nothing
