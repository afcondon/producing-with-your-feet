-- | The looper's banks, on the MC6 page: what this app can write to an MC6 so
-- | the board can drive Itajara.
-- |
-- | **PWYF programs controllers; the looper is something an MC6 bank can
-- | control** (AC, 2026-11-07). The looper's page is Quadrat's Loop tab now,
-- | and this app owns nothing of the looper but this: putting its banks on the
-- | hardware, the same job it does for every other page on the device, and the
-- | printed sheet of them. So the panel sits with the board's own banks rather
-- | than on a looper tab. The banks themselves are the looper's (the Loops
-- | bank is `Data.Looper.Banks`, in itajara); only the write is this app's,
-- | because it goes out through this app's MIDI ports.
-- |
-- | Until 2026-11-07 this was the Looper page's "MC6 banks" panel
-- | (`Component.Looper.Banks`). The same day the single-loop transport bank
-- | and the looper's six other pages went (AC), leaving the Loops bank and
-- | the channel it speaks on.
module Component.Controls.LooperBanks
  ( State
  , Handlers
  , render
  ) where

import Prelude

import Data.Array as Array
import Data.Looper.Banks as LoopBanks
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | Everything the panel reads. Open, so the MC6 page's input passes unchanged.
-- | `looperSwitchChannelClash` is computed by the app, which knows the
-- | registry (`Data.Looper.switchChannelClash`).
type State r =
  ( looperProgramStatus :: Maybe String
  , mc6LoopBankBase :: Int
  , mc6BoardBankNum :: Int
  , looperSwitchChannel :: LoopBanks.SwitchChannel
  , looperSwitchChannelClash :: Maybe String
  | r
  )

-- | The write the panel can ask for, the channel it can set, and the sheet.
type Handlers i =
  { programLoopBanks :: i
  , setSwitchChannel :: LoopBanks.SwitchChannel -> i
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
    [ HH.h4_ [ HH.text "The looper's bank" ]
    , loopsCard
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
  channelNumber = LoopBanks.switchChannelNumber state.looperSwitchChannel

  -- The looper's own bank: twelve labelled places to stand, each sending its
  -- own CC on the looper's channel, and the looper decides what standing there
  -- means.
  loopsCard =
    HH.div [ HP.class_ (HH.ClassName "controls-looper-card") ]
      [ HH.h5_ [ HH.text "Loops bank" ]
      , HH.p [ HP.class_ (HH.ClassName "controls-looper-note") ]
          [ HH.text $
              "MC6 bank " <> show state.mc6LoopBankBase
              <> ", uploaded once. Every switch sends its own CC on channel "
              <> show channelNumber
              <> ", so a press says which bank it came from and the looper never has "
              <> "to remember which page the board is showing."
          ]
      , HH.table [ HP.class_ (HH.ClassName "docs-table") ]
          [ HH.tbody_ (map familyRow (LoopBanks.banks
              { base: state.mc6LoopBankBase
              , boardBank: state.mc6BoardBankNum
              , channel: state.looperSwitchChannel
              })) ]
      , channelPicker
      , HH.button
          [ HP.class_ (HH.ClassName "files-btn")
          , HP.disabled (not mc6)
          , HE.onClick \_ -> h.programLoopBanks
          ]
          [ HH.text "Program MC6 loops bank" ]
      ]

  -- **One setting, two readers.** This app writes the bank on it and answers
  -- presses on it when the looper's process is not running; the process reads
  -- its own copy from `LOOPER_MC6_CHANNEL`. So the sentence says both halves
  -- of a change, because doing only the first leaves a board whose presses
  -- the process never hears.
  channelPicker =
    HH.div [ HP.class_ (HH.ClassName "controls-looper-channel") ]
      [ HH.label_
          [ HH.text "MIDI channel "
          , HH.select
              [ HE.onSelectedIndexChange \index ->
                  h.setSwitchChannel
                    (fromMaybe LoopBanks.defaultSwitchChannel
                      (Array.index LoopBanks.allSwitchChannels index))
              ]
              (map channelOption LoopBanks.allSwitchChannels)
          ]
      , HH.p [ HP.class_ (HH.ClassName "controls-looper-note") ]
          [ HH.text $
              "Changing it means writing the bank again (the button below) and \
              \restarting the looper's process with LOOPER_MC6_CHANNEL="
              <> show channelNumber
              <> ". Nine unless something else on the board needs it: the MC6 \
                 \routes channels this app does not know about (LoopyPro is 16)."
          ]
      , case state.looperSwitchChannelClash of
          Nothing -> HH.text ""
          Just why -> HH.p [ HP.class_ (HH.ClassName "controls-looper-note controls-looper-warning") ]
            [ HH.text why ]
      ]

  channelOption channel =
    HH.option
      [ HP.selected (channel == state.looperSwitchChannel) ]
      [ HH.text (show (LoopBanks.switchChannelNumber channel)) ]

  familyRow cb =
    HH.tr_
      [ HH.td [ HP.class_ (HH.ClassName "docs-cc") ] [ HH.text (show cb.mc6BankNumber) ]
      , HH.td_ [ HH.text cb.name ]
      , HH.td_
          [ HH.text $ Array.intercalate ", "
              (Array.filter (_ /= "") (map _.label cb.switches)) ]
      ]
