-- | The live-controls pages: the knobs worth reaching for mid-phrase.
-- |
-- | ## Why these are picks and not a new table of CCs
-- |
-- | See `Data.Twister.Scene.Pick`. Every entry below borrows a control from the
-- | pedal's own Twister page, so "MOOD's Clock" is described once, in
-- | `Pedals.Mood`, with the centre it detents to and the options it steps
-- | through. A scene only says *where a hand should find it*.
-- |
-- | ## Why a scene is one bank's worth of pedals and not all of them
-- |
-- | The MC6 bank this pairs with puts six switches under a foot — Onward left
-- | and right, MOOD left and right, Lost+Found left and right — so the hands
-- | should be on the same three pedals. A scene that also carried Habit and
-- | Hedra would be a page where two thirds of the knobs act on pedals the feet
-- | cannot reach, which is the back-and-forth the whole arrangement exists to
-- | remove. Habit, Hedra and Brig get their own bank and their own scene.
-- |
-- | ## The grid
-- |
-- | A row per pedal, three knobs and a switch, which is the shape a pedal's own
-- | page already uses — column four is where a switch lives, learned once. The
-- | bottom row breaks it deliberately: those are the three pedals' *second*
-- | switches, in the same left-to-right order as the rows above, so the column
-- | you are in still tells you whose pedal you are on.
-- |
-- | **Nothing here is precious.** These are opening bids on six pedals with
-- | enormous control surfaces; the point of the file is that changing one is a
-- | line, and changing one cannot break the pedal it borrows from.
module Data.Twister.Scenes
  ( liveThree
  , liveThreeBank
  , sceneForBank
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

-- | `Just` a borrowing, said shortly, because the table below is the content
-- | and sixteen `Just { pedal: _, index: _ }`s would bury it.
at :: PedalId -> Int -> Maybe { pedal :: PedalId, index :: Int }
at pedal index = Just { pedal, index }

-- | Onward, MOOD and Lost+Found — the three the feet are already on.
-- |
-- | The knobs, and why each:
-- |
-- | - **MOOD Clock** is the sample rate, and it is the one control on that
-- |   pedal that changes what kind of machine it is rather than how much of it
-- |   you hear. **Length** is the micro-looper's slice. **Mix** last, because
-- |   it is the one you reach for to get out of trouble.
-- | - **Onward Error** is the glitch, which is the reason the pedal is on the
-- |   board. **Texture** and **Size** shape what it glitches.
-- | - **Lost+Found Spill**, **Glue** and **Blend** — the three that decide how
-- |   much of the wreckage comes back.
-- |
-- | Hue is the page's own rather than any pedal's, because it belongs to none
-- | of them; the cells are lit in their own pedal's colour, which is what makes
-- | a row readable without a label on the device.
liveThree :: SceneDef
liveThree =
  { name: "Live three"
  , hue: 110
  , encoders:
      [ at mood 5, at mood 2, at mood 1, Nothing            -- Clock, Length, Mix
      , at onward 4, at onward 6, at onward 0, Nothing      -- Error, Texture, Size
      , at lostAndFound 9, at lostAndFound 13, at lostAndFound 5, Nothing
                                                            -- Spill, Glue, Blend
      , Nothing, Nothing, Nothing, Nothing
      ]
  , buttons:
      [ Nothing, Nothing, Nothing, at mood 3               -- Micro-Looper
      , Nothing, Nothing, Nothing, at onward 3             -- Glitch
      , Nothing, Nothing, Nothing, at lostAndFound 3       -- Left
      -- The seconds, in the same order the rows are in: MOOD's Wet Freeze,
      -- Onward's Freeze, Lost+Found's Right. Column four stays the switch
      -- column, so the fourth cell here is the one that is free.
      , at mood 11, at onward 7, at lostAndFound 7, Nothing
      ]
  }

-- | Every scene this rig knows, in the order a menu would list them.
-- |
-- | One so far. It is a list rather than a single value because the second is
-- | expected — Habit, Hedra and Brig want their own — and because whatever
-- | associates a scene with an MC6 bank needs something to look through.
scenes :: Array SceneDef
scenes = [ liveThree ]

-- | The MC6 bank whose six switches are Onward, MOOD and Lost+Found.
-- |
-- | **A placeholder, and the one number here to change.** Banks 1 to 12 and 15
-- | are spoken for — the board mirror, the looper's eight pages, the probe, two
-- | diagnostics pages and the control bank — so 13 is free rather than chosen.
-- | Nothing has been written to the device for it yet; this says where the
-- | Twister should go when you stand there.
liveThreeBank :: Int
liveThreeBank = 13

-- | Which scene an MC6 bank calls up, if any.
-- |
-- | `Nothing` is the ordinary answer and means "leave the Twister alone" — the
-- | looper's pages or the focused pedal's, as before. Only a bank that has
-- | given its switches to pedals asks for a scene, because only there is the
-- | hand's surface a foregone conclusion.
sceneForBank :: Int -> Maybe SceneDef
sceneForBank bank
  | bank == liveThreeBank = Just liveThree
  | otherwise = Nothing
