module Config.Decode.Modes
  ( decodeDualChannelModes
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Midi (makeCC)
import Data.Pedal.Modes (ChannelDef, DualChannelModes, EffectDef, EffectVariant)
import Data.Traversable (traverse)
import Foreign.Object as FO

lookupStr :: String -> FO.Object Json -> Maybe String
lookupStr key obj = FO.lookup key obj >>= Json.toString

lookupNum :: String -> FO.Object Json -> Maybe Int
lookupNum key obj = do
  j <- FO.lookup key obj
  n <- Json.toNumber j
  Int.fromNumber n

decodeDualChannelModes :: Json -> Maybe DualChannelModes
decodeDualChannelModes json = do
  obj <- Json.toObject json
  effectsJson <- FO.lookup "effects" obj >>= Json.toArray
  effects <- traverse decodeEffectDef effectsJson
  leftJson <- FO.lookup "left" obj
  left <- decodeChannelDef leftJson
  rightJson <- FO.lookup "right" obj
  right <- decodeChannelDef rightJson
  Just { effects, left, right }

decodeEffectDef :: Json -> Maybe EffectDef
decodeEffectDef json = do
  obj <- Json.toObject json
  id <- lookupStr "id" obj
  label <- lookupStr "label" obj
  aJson <- FO.lookup "a" obj
  a <- decodeVariant aJson
  bJson <- FO.lookup "b" obj
  b <- decodeVariant bJson
  hold <- lookupStr "hold" obj
  Just { id, label, a, b, hold }

decodeVariant :: Json -> Maybe EffectVariant
decodeVariant json = do
  obj <- Json.toObject json
  name <- lookupStr "name" obj
  time <- lookupStr "time" obj
  modify <- lookupStr "modify" obj
  alt <- lookupStr "alt" obj
  Just { name, time, modify, alt }

decodeChannelDef :: Json -> Maybe ChannelDef
decodeChannelDef json = do
  obj <- Json.toObject json
  modeCC <- lookupNum "modeCC" obj >>= makeCC
  swapCC <- lookupNum "swapCC" obj >>= makeCC
  nativeJson <- FO.lookup "native" obj >>= Json.toArray
  native <- traverse (\j -> Json.toString j) nativeJson
  Just { modeCC, swapCC, native }
