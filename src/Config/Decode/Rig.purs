module Config.Decode.Rig
  ( decodeRig
  ) where

import Prelude

import Config.Types (BrandSlots, MidiMatch, MidiRouting, PedalEntry, RigConfig, SlotRange)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Foreign.Object as FO

lookupStr :: String -> FO.Object Json -> Maybe String
lookupStr key obj = FO.lookup key obj >>= Json.toString

lookupNum :: String -> FO.Object Json -> Maybe Int
lookupNum key obj = do
  j <- FO.lookup key obj
  n <- Json.toNumber j
  Int.fromNumber n

decodeRig :: Json -> Maybe RigConfig
decodeRig json = do
  obj <- Json.toObject json
  name <- lookupStr "name" obj
  storagePrefix <- lookupStr "storagePrefix" obj
  pedalsJson <- FO.lookup "pedals" obj >>= Json.toArray
  pedals <- traverse decodePedalEntry pedalsJson
  routingJson <- FO.lookup "midiRouting" obj
  midiRouting <- decodeMidiRouting routingJson
  slotsJson <- FO.lookup "slotRanges" obj >>= Json.toArray
  slotRanges <- traverse decodeSlotRangeEntry slotsJson
  let controller = case lookupStr "controller" obj of
        Just c -> c
        Nothing -> ""
  Just { name, storagePrefix, pedals, midiRouting, slotRanges, controller }

decodePedalEntry :: Json -> Maybe PedalEntry
decodePedalEntry json = do
  obj <- Json.toObject json
  file <- lookupStr "file" obj
  channel <- lookupNum "channel" obj
  Just { file, channel }

decodeMidiRouting :: Json -> Maybe MidiRouting
decodeMidiRouting json = do
  obj <- Json.toObject json
  pedalOutput <- FO.lookup "pedalOutput" obj >>= decodeMidiMatch
  twisterInput <- FO.lookup "twisterInput" obj >>= decodeMidiMatch
  twisterOutput <- FO.lookup "twisterOutput" obj >>= decodeMidiMatch
  let mc6Input = case FO.lookup "mc6Input" obj >>= decodeMidiMatch of
        Just m -> m
        Nothing -> { match: "Morningstar" }
  Just { pedalOutput, twisterInput, twisterOutput, mc6Input }

decodeMidiMatch :: Json -> Maybe MidiMatch
decodeMidiMatch json = do
  obj <- Json.toObject json
  match <- lookupStr "match" obj
  Just { match }

-- | `managed` is optional: a rig.json written before the browse/save split
-- | still decodes, and simply has no house convention for where to save.
decodeSlotRangeEntry :: Json -> Maybe { brand :: String, slots :: BrandSlots }
decodeSlotRangeEntry json = do
  obj <- Json.toObject json
  brand <- lookupStr "brand" obj
  range <- FO.lookup "range" obj >>= decodeSpan
  let managed = FO.lookup "managed" obj >>= decodeSpan
  Just { brand, slots: { range, managed } }

decodeSpan :: Json -> Maybe SlotRange
decodeSpan json = do
  obj <- Json.toObject json
  start <- lookupNum "start" obj
  count <- lookupNum "count" obj
  Just { start, count }
