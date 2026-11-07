-- | The looper's banks, on the MC6 page: what this app can write to an MC6 so
-- | the board can drive Itajara.
-- |
-- | **PWYF programs controllers; the looper is something an MC6 bank can
-- | control** (AC, 2026-11-07). The looper's page is Quadrat's Loop tab now,
-- | and this app owns nothing of the looper but this: putting its banks on the
-- | hardware, the same job it does for every other page on the device, and the
-- | printed sheet of them. So the panel sits with the board's own banks rather
-- | than on a looper tab. The banks themselves are the looper's (the six-loop
-- | family is `Data.Looper.Banks`, in itajara); only the write is this app's,
-- | because it goes out through this app's MIDI ports.
-- |
-- | Until 2026-11-07 this was the Looper page's "MC6 banks" panel
-- | (`Component.Looper.Banks`), and it drew the six-loop table twice: the
-- | transport card carried the family card inside it as well as beside it.
-- | Drawn once here.
module Component.Controls.LooperBanks
  ( State
  , Handlers
  , render
  ) where

import Prelude

import Data.Array as Array
import Data.Looper as Looper
import Data.Looper.Banks as LoopBanks
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | Everything the panel reads. Open, so the MC6 page's input passes unchanged.
type State r =
  ( looperProgramStatus :: Maybe String
  , mc6LooperBankNum :: Int
  , mc6LoopBankBase :: Int
  , mc6BoardBankNum :: Int
  | r
  )

-- | The two writes the panel can ask for, and the sheet.
type Handlers i =
  { programLooperBank :: i
  , programLoopBanks :: i
  , printSheet :: i
  }

-- | The panel, given whether there is an MC6 to write to.
render
  :: forall w i r
   . Handlers i
  -> Boolean
  -> Record (State r)
  -> HH.HTML w i
render h mc6 state =
  HH.div [ HP.class_ (HH.ClassName "controls-looper-banks") ]
    [ HH.h4_ [ HH.text "The looper's banks" ]
    , transportCard
    , loopFamilyCard
    -- **Not a write, but it belongs with them.** The sheet is the board's
    -- pages and the recipes on paper, opened in its own tab so the paper can be
    -- read beside the rig while the looper's page keeps focus — a background
    -- tab stops handling Twister messages.
    , HH.div [ HP.class_ (HH.ClassName "controls-looper-actions") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "files-btn")
            , HP.title "The looper's pages and recipes, in a new tab, laid out for A4. \
                       \Print it or save it as a PDF from there."
            , HE.onClick \_ -> h.printSheet
            ]
            [ HH.text "Print sheet" ]
        ]
    , case state.looperProgramStatus of
        Nothing -> HH.text ""
        Just msg -> HH.p [ HP.class_ (HH.ClassName "controls-looper-note") ] [ HH.text msg ]
    ]
  where
  -- Itajara is a pedal now, so its full surface lives on its own Detail page
  -- and any switch can be assigned to any of it. What remains here is the one
  -- thing that page cannot do: put a usable bank on the hardware today.
  transportCard =
    HH.div [ HP.class_ (HH.ClassName "controls-looper-card") ]
      [ HH.h5_ [ HH.text "Footswitch control" ]
      , HH.p [ HP.class_ (HH.ClassName "controls-looper-note") ]
          [ HH.text $
              "Itajara is a pedal on channel " <> show Looper.itajaraChannel
              <> ", so the MC6 addresses it exactly as it addresses Habit or MOOD — "
              <> "and every control is on its own page, assignable to any switch. "
              <> "This writes a starter transport bank to MC6 bank "
              <> show state.mc6LooperBankNum <> "."
          ]
      , HH.table [ HP.class_ (HH.ClassName "docs-table") ]
          [ HH.tbody_ (map bankRow (Looper.looperBank state.mc6LooperBankNum state.mc6BoardBankNum).switches) ]
      , HH.button
          [ HP.class_ (HH.ClassName "files-btn")
          , HP.disabled (not mc6)
          , HE.onClick \_ -> h.programLooperBank
          ]
          [ HH.text "Program MC6 looper bank" ]
      ]

  -- The six-loop machine's own banks, which are a different thing wearing a
  -- similar name to the transport bank above: that one drives one loop through
  -- Itajara's pedal CCs, this one gives the looper twelve labelled places to
  -- stand on each of its pages and lets the looper decide what standing there
  -- means.
  loopFamilyCard =
    HH.div [ HP.class_ (HH.ClassName "controls-looper-card") ]
      [ HH.h5_ [ HH.text "Six-loop banks" ]
      , HH.p [ HP.class_ (HH.ClassName "controls-looper-note") ]
          [ HH.text $
              show (Array.length LoopBanks.allSlots)
              <> " banks from MC6 bank " <> show state.mc6LoopBankBase
              <> ", uploaded once. Every switch sends its own CC on channel "
              <> show LoopBanks.switchChannel
              <> ", so a press says which bank it came from and the looper never has "
              <> "to remember which page the board is showing."
          ]
      , HH.table [ HP.class_ (HH.ClassName "docs-table") ]
          [ HH.tbody_ (map familyRow (LoopBanks.banks
              { base: state.mc6LoopBankBase, boardBank: state.mc6BoardBankNum })) ]
      , HH.button
          [ HP.class_ (HH.ClassName "files-btn")
          , HP.disabled (not mc6)
          , HE.onClick \_ -> h.programLoopBanks
          ]
          [ HH.text "Program MC6 loop banks" ]
      ]

  familyRow cb =
    HH.tr_
      [ HH.td [ HP.class_ (HH.ClassName "docs-cc") ] [ HH.text (show cb.mc6BankNumber) ]
      , HH.td_ [ HH.text cb.name ]
      , HH.td_
          [ HH.text $ Array.intercalate ", "
              (Array.filter (_ /= "") (map _.label cb.switches)) ]
      ]

  -- Switch letters run A–F on the MC6 itself, then G/H/I on the first FS3X.
  bankRow sw =
    if sw.label == "" then HH.text ""
    else HH.tr_
      [ HH.td [ HP.class_ (HH.ClassName "docs-cc") ] [ HH.text sw.label ]
      , HH.td_ [ HH.text sw.longName ]
      ]
