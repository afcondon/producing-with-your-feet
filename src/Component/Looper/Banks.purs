-- | The Looper page's MC6 banks panel: what this app can write to an MC6.
-- |
-- | Split from the page when the page moved to `Itajara.Surface.Looper.Page`
-- | (2026-11-07). The page is any app's; programming an MC6 is this one's —
-- | the transport bank addresses Itajara as a pedal on this app's channel
-- | (`Data.Looper`), and the write goes out through this app's MIDI ports. So
-- | the page draws the panel only when an app hands it one, and this is the
-- | one PWYF hands it.
module Component.Looper.Banks
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

-- | Everything the panel reads. Open, like the page's, so `AppState` passes
-- | unchanged.
type State r =
  ( looperProgramStatus :: Maybe String
  , mc6LooperBankNum :: Int
  , mc6LoopBankBase :: Int
  , mc6BoardBankNum :: Int
  | r
  )

-- | The two writes the panel can ask for.
type Handlers i =
  { programLooperBank :: i
  , programLoopBanks :: i
  }

-- | The panel's contents, given whether there is an MC6 to write to.
render
  :: forall w i r
   . Handlers i
  -> Boolean
  -> Record (State r)
  -> Array (HH.HTML w i)
render h mc6 state = [ footswitchCard, loopFamilyCard ]
  where
  -- Itajara is a pedal now, so its full surface lives on its own Detail page
  -- and any switch can be assigned to any of it. What remains here is the one
  -- thing that page cannot do: put a usable bank on the hardware today.
  footswitchCard =
    HH.div [ HP.class_ (HH.ClassName "looper-footswitch") ]
      [ HH.h3_ [ HH.text "Footswitch control" ]
      , HH.p [ HP.class_ (HH.ClassName "looper-muted") ]
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
      , loopFamilyCard
      , case state.looperProgramStatus of
          Nothing -> HH.text ""
          Just msg -> HH.p [ HP.class_ (HH.ClassName "looper-muted") ] [ HH.text msg ]
      ]

  -- The six-loop machine's own banks, which are a different thing wearing a
  -- similar name to the transport bank above: that one drives one loop through
  -- Itajara's pedal CCs, this one gives the app twelve labelled places to stand
  -- on each of six pages and lets the app decide what standing there means.
  loopFamilyCard =
    HH.div [ HP.class_ (HH.ClassName "looper-footswitch") ]
      [ HH.h3_ [ HH.text "Six-loop banks" ]
      , HH.p [ HP.class_ (HH.ClassName "looper-muted") ]
          [ HH.text $
              show (Array.length LoopBanks.allSlots)
              <> " banks from MC6 bank " <> show state.mc6LoopBankBase
              <> ", uploaded once. Every switch sends its own CC on channel "
              <> show LoopBanks.switchChannel
              <> ", so a press says which bank it came from and the app never has "
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
