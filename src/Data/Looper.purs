-- | The looper, as this app knows it: something an MC6 bank can control.
-- |
-- | **PWYF programs controllers; the looper is something an MC6 bank can
-- | control** (AC, 2026-11-07). The looper's page is Quadrat's Loop tab, its
-- | banks are itajara's (`Data.Looper.Banks`), and this app writes them to the
-- | MC6 and answers their presses only while the looper's own process is not
-- | holding `itajara:switches`.
-- |
-- | Until 2026-11-07 the looper was also a pedal here: a registry entry on
-- | channel 13 with a pill, a Detail page, an engage CC that board presets
-- | sent, and a single-loop "transport" bank written to MC6 bank 9. All of
-- | that went (AC: "remove the pill and that control page and simplify
-- | everything"), so the looper reaches the board through its own bank and
-- | channel alone.
-- |
-- | What is left here is the one question about that channel this app can
-- | answer and the looper cannot: does anything else on the board speak on it?
module Data.Looper
  ( switchChannelClash
  ) where

import Prelude

import Data.Array as Array
import Data.Looper.Banks (SwitchChannel, switchChannelNumber)
import Data.MC6.Board (boardRecallChannel)
import Data.Maybe (Maybe(..))
import Data.Pedal (PedalId(..))

-- | Why a chosen switch channel would be misread, if it would.
-- |
-- | Given the pedals the registry has on that channel. **Two things are
-- | checked, and a third cannot be**: a pedal on the channel would hear every
-- | loop press as one of its CCs (the switch CCs run 16-27, squarely in pedal
-- | territory), and channel 1 is this app's board-recall relay, which would
-- | read a press as a board. What else the MC6 routes to the channel (LoopyPro
-- | on 16, found from the device's own table) is known only to the device, so
-- | it is said in the sentence beside the setting rather than checked here.
switchChannelClash :: SwitchChannel -> Array PedalId -> Maybe String
switchChannelClash channel pedalsHere
  | switchChannelNumber channel == boardRecallChannel =
      Just ("Channel " <> show boardRecallChannel
        <> " is this app's board-recall channel; a loop press there would recall a board.")
  | not (Array.null pedalsHere) =
      Just ("Channel " <> show (switchChannelNumber channel) <> " is "
        <> Array.intercalate ", " (map (\(PedalId p) -> p) pedalsHere)
        <> "'s; the pedal would hear every loop press as one of its CCs.")
  | otherwise = Nothing
