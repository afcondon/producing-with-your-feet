-- | The Twister's current page, small and permanent, beside the loops.
-- |
-- | ## Why a card and not only the panel
-- |
-- | There is a full Twister reference behind a button already, and it stays —
-- | it is where you go to learn the layout. This answers a different question,
-- | asked constantly and never worth opening a panel for: **what is under my
-- | hand right now.** The board has four looper pages, a page per pedal and a
-- | scene per control group, and the device says which by a colour and a ring
-- | position, neither of which helps when the thing you are looking at is the
-- | screen.
-- |
-- | It replaced the MC6 board simulator in this slot. That was the right thing
-- | to show when the MC6 was the whole surface; it is not now — `A`–`F` are
-- | labelled on the device's own screen and `G`–`L` are six switches that mean
-- | the same six things on every bank.
-- |
-- | ## One renderer, three surfaces
-- |
-- | A scene, a pedal's own page and the looper's four are drawn by the same
-- | code, which is the whole reason this was built last: doing it before scenes
-- | existed would have meant building it twice. They differ only in where a
-- | cell's *name* and *colour* come from, and each surface answers that its own
-- | way — the looper from its layout table, the other two from the pedal's CC
-- | labels and its hue.
module Component.Looper.TwisterCard
  ( CardCell
  , Card
  , cardFor
  , render
  ) where

import Prelude

import Config.Preset (buildCCLabels)
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Int (round, toNumber)
import Data.Looper as Looper
import Data.Looper.Twister as LoopTw
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Pedal (PedalId)
import Data.Twister (encoderCC, buttonCC)
import Data.Twister as TwisterData
import Data.Twister.Scene (Scene)
import Data.Twister.Scene as Scene
import Engine.Twister as ETw
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | One cell as the card draws it: a word and an ink.
-- |
-- | `Nothing` for the ink where the surface does not fix one — the looper's
-- | loop encoders take theirs from what the loop is doing, which this card does
-- | not attempt to follow. A cell with no name is a dark one.
type CardCell = { name :: String, ink :: Maybe String }

type Card = { title :: String, cells :: Array CardCell }

-- | Which page the Twister is on, and what is on it.
-- |
-- | **The scene is asked first, because a scene wins.** That is the same order
-- | `handleEncoderTurn` resolves in, and it has to be: a card that showed the
-- | looper's page while the encoders were driving MOOD would be worse than no
-- | card, because it would be believed.
cardFor
  :: PedalRegistry
  -> Maybe Scene
  -> Maybe PedalId
  -> Int
  -> Card
cardFor registry mScene mFocus page = case mScene of
  Just sc -> fromScene registry sc
  Nothing -> case mFocus of
    Just pid | not (Looper.isItajara pid) -> fromPedal registry pid
    _ -> fromLooper page

-- | The looper's own table, which already knows every word on it.
fromLooper :: Int -> Card
fromLooper page =
  case Array.index LoopTw.pages page of
    Nothing -> { title: "Twister", cells: [] }
    Just pg ->
      { title: pg.name
      , cells: map
          (\c -> { name: c.name, ink: c.tone >>= LoopTw.swatchNamed })
          pg.cells
      }

-- | A scene: every cell named by the pedal it borrows from.
-- |
-- | The pedal is per cell, so the label map is looked up per cell too. That is
-- | sixteen small lookups on a render rather than one, and it is what a mixed
-- | page costs — the alternative was to carry the labels in the scene, which is
-- | the duplication `Data.Twister.Scene.Pick` exists to refuse.
fromScene :: PedalRegistry -> Scene -> Card
fromScene registry sc =
  { title: sc.name
  , cells: map cell (Array.range 0 (TwisterData.encodersPerBank - 1))
  }
  where
  cell i =
    let mEnc = Scene.encoderAt sc i
        mBtn = Scene.buttonAt sc i
        mPedal = map _.pedal mEnc <|> map _.pedal mBtn
        mCC = map (encoderCC <<< _.control) mEnc
                <|> map (buttonCC <<< _.control) mBtn
    in case mPedal of
         Nothing -> { name: "", ink: Nothing }
         Just pid -> case CRegistry.findPedal registry pid of
           Nothing -> { name: "", ink: Nothing }
           Just def ->
             { name: maybe "" (\cc -> fromMaybe "" (Map.lookup cc (buildCCLabels def))) mCC
             , ink: Just (inkOfHue (ETw.pedalHue def))
             }

-- | A pedal's own page. Bank one only, which is all a pedal has.
fromPedal :: PedalRegistry -> PedalId -> Card
fromPedal registry pid = case CRegistry.findPedal registry pid of
  Nothing -> { title: "Twister", cells: [] }
  Just def -> case def.twister of
    Nothing -> { title: def.meta.name, cells: [] }
    Just tw ->
      let labels = buildCCLabels def
          ink = inkOfHue (ETw.pedalHue def)
          nameAt i =
            let mCC = map encoderCC (Array.index tw.encoders i >>= identity)
                        <|> map buttonCC (Array.index tw.buttons i >>= identity)
            in maybe "" (\cc -> fromMaybe "" (Map.lookup cc labels)) mCC
      in { title: def.meta.name
         , cells: map
             (\i -> let n = nameAt i
                    in { name: n, ink: if n == "" then Nothing else Just ink })
             (Array.range 0 (TwisterData.encodersPerBank - 1))
         }

-- | A Twister hue as a colour a browser can draw.
-- |
-- | **Approximate, and it has to be said.** The device's hue is a position on
-- | its own wheel and the mapping to sRGB is a table nobody here has measured;
-- | this spreads 0–127 around a circle and picks a saturation that reads on a
-- | light page. It is right about *which cells match each other*, which is the
-- | only thing the card uses colour for, and wrong about the exact shade.
inkOfHue :: Int -> String
inkOfHue h =
  "hsl(" <> show (round (toNumber (clamp 0 127 h) / 127.0 * 360.0)) <> ",55%,52%)"

render :: forall w i. Card -> HH.HTML w i
render card =
  HH.div [ HP.class_ (HH.ClassName "twister-card") ]
    [ HH.div [ HP.class_ (HH.ClassName "twister-card-title") ] [ HH.text card.title ]
    , HH.div [ HP.class_ (HH.ClassName "twister-card-grid") ] (map cell card.cells)
    ]
  where
  cell c =
    HH.div
      [ HP.class_ (HH.ClassName ("twister-card-cell" <> if c.name == "" then " is-dark" else ""))
      -- The left edge only. A whole box in a pedal's colour is a block of
      -- colour with a word on it; a rule down one side groups the row and
      -- leaves the word to be read.
      , HP.style (maybe "" (\ink -> "border-left-color:" <> ink) c.ink)
      ]
      [ HH.text c.name ]
