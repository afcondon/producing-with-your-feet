module Config.Decode.Rig
  ( decodeRig
  ) where

import Prelude

import Config.Types (MidiMatch, MidiRouting, PedalEntry, RigConfig, SlotRange)
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
  let looper = case lookupStr "looper" obj of
        Just l -> l
        Nothing -> ""
      controller = case lookupStr "controller" obj of
        Just c -> c
        Nothing -> ""
  Just { name, storagePrefix, pedals, midiRouting, slotRanges, looper, controller }

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
  loopyOutput <- FO.lookup "loopyOutput" obj >>= decodeMidiMatch
  let loopyChannel = case lookupNum "loopyChannel" obj of
        Just ch -> ch
        Nothing -> 16
  Just { pedalOutput, twisterInput, twisterOutput, loopyOutput, loopyChannel }

decodeMidiMatch :: Json -> Maybe MidiMatch
decodeMidiMatch json = do
  obj <- Json.toObject json
  match <- lookupStr "match" obj
  Just { match }

decodeSlotRangeEntry :: Json -> Maybe { brand :: String, range :: SlotRange }
decodeSlotRangeEntry json = do
  obj <- Json.toObject json
  brand <- lookupStr "brand" obj
  rangeJson <- FO.lookup "range" obj >>= Json.toObject
  start <- lookupNum "start" rangeJson
  count <- lookupNum "count" rangeJson
  Just { brand, range: { start, count } }
