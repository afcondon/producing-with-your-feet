-- | The Twister's layout, printed in the Looper page's bindings panel.
-- |
-- | **Every word of it comes from `Data.Looper.Twister`.** Nothing here knows
-- | what any encoder does; it knows how to draw a four-by-four grid of things
-- | that describe themselves. Move a control and the card moves with it, which
-- | is the only way a printed layout stays true — a cheat sheet typed out beside
-- | the table it describes is right when written and wrong in the one place
-- | nobody thinks to check, because it is only documentation.
-- |
-- | The same lesson as `Data.Looper.Banks.auxLegend`, which exists because three
-- | screens in one day restated something that module already knew.
-- |
-- | ## The colours are a claim, not an observation
-- |
-- | The tones are what the app *asks* the device for, and no one has yet
-- | compared them with what it does (`DESIGN-TWISTER` §12). The card says so,
-- | because a legend that quietly presented an unverified intention as fact
-- | would be the thing to blame when a knob turns out to be the wrong colour.
module Component.Looper.TwisterMap (render) where

import Prelude

import Data.Looper.Twister as TW
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | The card. Takes whether a Twister output is selected, which page the device
-- | last spoke from, and what to do about wanting a different one.
-- |
-- | **No longer its own disclosure.** It was a `<details>` folded shut at the
-- | foot of the page; it now lives in a panel that is itself the disclosure, and
-- | a card you have to open twice is a card you do not open.
render :: forall w i. Boolean -> Int -> Maybe Int -> (Int -> i) -> HH.HTML w i
render connected showing heard goTo =
  HH.div [ HP.class_ (HH.ClassName "twister-map") ]
    [ HH.div [ HP.class_ (HH.ClassName "twister-map-body") ]
        [ HH.p [ HP.class_ (HH.ClassName "twister-map-status") ]
            [ HH.text (status connected showing heard) ]
        , HH.p [ HP.class_ (HH.ClassName "twister-map-intro") ]
            -- **Corrected 2026-08-27.** This used to say the side buttons switch
            -- the page. They did for an afternoon; the pager is the bottom-right
            -- encoder now and the side buttons went back to walking between
            -- pedals (DESIGN-TWISTER §9.1). Exactly the failure this module's own
            -- header warns about — a legend is right when written and wrong in
            -- the one place nobody thinks to check.
            [ HH.text
                "Each encoder is a knob and a button. The bottom-right encoder is \
                \the pager: turn it for the next page, press it to come home. \
                \Every message the device sends says which page it came from, so \
                \the card and the controller cannot disagree for longer than one \
                \turn of a knob."
            ]
        , HH.div [ HP.class_ (HH.ClassName "twister-pages") ]
            (map (page showing goTo) TW.pages)
        , phases
        , HH.p [ HP.class_ (HH.ClassName "twister-map-caveat") ]
            [ HH.text
                "Pages 3 and 4 are deliberately empty; the per-layer surface is \
                \the tenant they are being kept for. The colours are what the app \
                \asks the device for and have not been checked against it."
            ]
        ]
    ]

-- | Where the device last spoke from.
-- |
-- | **Read off the wire, never tracked.** Every encoder message carries its
-- | page, so this cannot be wrong for longer than one turn of a knob — which is
-- | the reason it is worth printing at all. Silent until the device says
-- | something: the app has no way to ask, and guessing "page 1" would be
-- | inventing the one fact this line exists to report.
status :: Boolean -> Int -> Maybe Int -> String
status false _ _ = "no Twister output selected"
status true showing heard =
  "showing page " <> show (showing + 1)
    <> case heard of
        -- The two facts only ever differ when the device will not take a bank
        -- change, and that is worth saying out loud rather than hiding: it is
        -- the difference between "the encoders mean the other page" and "the
        -- device is somewhere else and the lights have followed it there".
        Just b | b /= showing -> " — the device is on its own page " <> show (b + 1)
        _ -> ""

page :: forall w i. Int -> (Int -> i) -> TW.Page -> HH.HTML w i
page showing goTo p =
  HH.div [ HP.class_ (HH.ClassName "twister-page") ]
    [ HH.h4_
        [ HH.span [ HP.class_ (HH.ClassName "twister-page-num") ]
            [ HH.text ("Page " <> show (p.bank + 1)) ]
        , HH.text p.name
        , if showing == p.bank
            then HH.span [ HP.class_ (HH.ClassName "twister-page-here") ] [ HH.text "here" ]
            else HH.button
              [ HP.class_ (HH.ClassName "twister-page-go")
              , HE.onClick \_ -> goTo p.bank
              , HP.title "Turn to this page. The app decides what the encoders mean; the device is asked to follow."
              ]
              [ HH.text "turn to this page" ]
        ]
    , HH.p [ HP.class_ (HH.ClassName "twister-page-note") ] [ HH.text p.note ]
    , HH.div [ HP.class_ (HH.ClassName "twister-grid") ] (map cell p.cells)
    ]

-- | One encoder, drawn where it physically sits: four across, four down, in
-- | index order, because that is how the device is laid out and any other
-- | arrangement would be a second mapping to hold in your head while using the
-- | first one.
cell :: forall w i. TW.Cell -> HH.HTML w i
cell c
  | c.name == "" = HH.div [ HP.class_ (HH.ClassName "twister-cell is-empty") ] []
  | otherwise =
      HH.div [ HP.class_ (HH.ClassName "twister-cell") ]
        [ HH.div [ HP.class_ (HH.ClassName "twister-cell-head") ]
            [ HH.span [ HP.class_ (HH.ClassName "twister-cell-name") ] [ HH.text c.name ]
            , case c.tone of
                Nothing -> HH.text ""
                Just t -> HH.span
                  [ HP.class_ (HH.ClassName ("twister-swatch tone-" <> t))
                  , HP.title t
                  ] []
            ]
        , line "turn" c.turn
        , line "press" c.press
        , case c.shows of
            Nothing -> HH.text ""
            Just s -> HH.div [ HP.class_ (HH.ClassName "twister-cell-shows") ] [ HH.text s ]
        ]
  where
  line _ Nothing = HH.text ""
  line label (Just what) =
    HH.div [ HP.class_ (HH.ClassName "twister-cell-line") ]
      [ HH.span [ HP.class_ (HH.ClassName "twister-cell-verb") ] [ HH.text label ]
      , HH.text what
      ]

-- | What each phase colours a loop encoder.
-- |
-- | Enumerated from `LoopPhase` rather than listed, so a seventh state would
-- | show up here rather than being quietly absent from the only place anyone
-- | would look for it.
phases :: forall w i. HH.HTML w i
phases =
  HH.div [ HP.class_ (HH.ClassName "twister-phases") ]
    [ HH.h4_ [ HH.text "A loop's colour, on page 1" ]
    , HH.div [ HP.class_ (HH.ClassName "twister-phase-row") ]
        (map one TW.phaseKey <> [ empty ])
    ]
  where
  one k =
    HH.span [ HP.class_ (HH.ClassName "twister-phase") ]
      [ HH.span [ HP.class_ (HH.ClassName ("twister-swatch tone-" <> k.tone)) ] []
      , HH.text k.phase
      ]
  -- Not a phase, so it cannot come from the enumeration — an empty slot is
  -- dark, and that is the state you will see most often.
  empty =
    HH.span [ HP.class_ (HH.ClassName "twister-phase") ]
      [ HH.span [ HP.class_ (HH.ClassName "twister-swatch tone-off") ] []
      , HH.text "empty"
      ]
