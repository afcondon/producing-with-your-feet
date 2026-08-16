-- | Compiling a board preset into MC6 messages.
-- |
-- | The useful framing (DESIGN-v2 §5): **a board preset is a source program and
-- | the MC6 preset is compiled output**, with a hard budget of sixteen
-- | messages. That makes compilation a thing with a result, and the result can
-- | be too big.
-- |
-- | This lives in its own module rather than inside the App component so that
-- | the thing which *sends* the messages and the thing which *counts* them are
-- | the same function. A budget display that reported 14 while the sync sent 16
-- | would be worse than no display at all, and that is exactly the drift you get
-- | when a view re-implements the arithmetic.
module Data.MC6.Board
  ( messageLimit
  , boardToMC6Messages
  , boardMessageCount
  , boardFits
  ) where

import Prelude

import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Data.Array as Array
import Data.MC6.Message as MC6Msg
import Data.MC6.Types (MC6Action(..), MC6Message)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (unCC, unProgramNumber)
import Data.Pedal (PedalId)
import Data.Pedal.Engage (EngageState(..), bypassCCs)
import Data.Preset (BoardPreset, PedalPreset, PresetId)
import Data.Tuple (Tuple(..))

-- | An MC6 MKII preset carries at most this many messages. The device does not
-- | report the overflow and `SysEx.sysexPresetData` pads with `Array.take 16`,
-- | so anything past here is dropped in silence.
messageLimit :: Int
messageLimit = 16

-- | Compile: a Program Change per pedal that has a flashed preset, bypass CCs
-- | for pedals being switched off, and optionally a bank jump on long press.
-- |
-- | Pedals set to `EngageNoChange` cost nothing — a partial board is cheaper
-- | than an all-twelve one, which is usually the way to get back under budget.
boardToMC6Messages
  :: PedalRegistry
  -> Array PedalPreset
  -> Maybe Int
  -> BoardPreset
  -> Array MC6Message
boardToMC6Messages registry presets mControlBankNum bp =
  Array.mapWithIndex (\idx msg -> msg { msgIndex = idx }) (pedalMsgs <> jumpMsg)
  where
  entries =
    Map.toUnfoldable bp.pedals
      :: Array (Tuple PedalId { presetId :: Maybe PresetId, engage :: EngageState })

  pedalMsgs = Array.concatMap entryToMessages entries

  jumpMsg = case mControlBankNum of
    Nothing -> []
    Just bankNum -> [ MC6Msg.bankJumpMessage bankNum ActionLongPressRelease ]

  entryToMessages (Tuple pid entry) = case entry.engage of
    EngageNoChange -> []
    _ -> pcMsg <> bypassMsg
    where
    ch = fromMaybe 1 (map _.meta.defaultChannel (CRegistry.findPedal registry pid))

    pcMsg = case entry.presetId of
      Nothing -> []
      Just presetId -> case Array.find (\p -> p.id == presetId) presets of
        Nothing -> []
        Just preset -> case preset.savedSlot of
          Nothing -> []
          Just slot -> [ MC6Msg.pcMessage ch (unProgramNumber slot) ActionPress ]

    -- One message where a dual pedal declares a whole-pedal bypass, two where
    -- it does not. Four of the thirteen pedals are dual, so this is the
    -- difference between an all-twelve board costing twelve and sixteen.
    bypassMsg = case entry.engage of
      EngageOff -> case CRegistry.findPedal registry pid of
        Nothing -> []
        Just def ->
          map (\c -> MC6Msg.ccMessage ch (unCC c) 0 ActionPress) (bypassCCs def.engage)
      _ -> []

boardMessageCount
  :: PedalRegistry -> Array PedalPreset -> Maybe Int -> BoardPreset -> Int
boardMessageCount registry presets mControlBankNum =
  Array.length <<< boardToMC6Messages registry presets mControlBankNum

boardFits :: PedalRegistry -> Array PedalPreset -> Maybe Int -> BoardPreset -> Boolean
boardFits registry presets mControlBankNum bp =
  boardMessageCount registry presets mControlBankNum bp <= messageLimit
