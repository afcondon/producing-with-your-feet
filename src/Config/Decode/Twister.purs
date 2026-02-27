module Config.Decode.Twister
  ( decodeTwisterMapping
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Midi (makeCC, makeMidiValue)
import Data.Traversable (traverse)
import Data.Twister (TwisterButton(..), TwisterEncoder(..), TwisterMapping)
import Foreign.Object as FO

lookupStr :: String -> FO.Object Json -> Maybe String
lookupStr key obj = FO.lookup key obj >>= Json.toString

lookupNum :: String -> FO.Object Json -> Maybe Int
lookupNum key obj = do
  j <- FO.lookup key obj
  n <- Json.toNumber j
  Int.fromNumber n

decodeTwisterMapping :: Json -> Maybe TwisterMapping
decodeTwisterMapping json = do
  obj <- Json.toObject json
  hue <- lookupNum "hue" obj
  encodersJson <- FO.lookup "encoders" obj >>= Json.toArray
  encoders <- traverse decodeMaybeEncoder encodersJson
  buttonsJson <- FO.lookup "buttons" obj >>= Json.toArray
  buttons <- traverse decodeMaybeButton buttonsJson
  Just { hue, encoders, buttons }

decodeMaybeEncoder :: Json -> Maybe (Maybe TwisterEncoder)
decodeMaybeEncoder json =
  if Json.isNull json
    then Just Nothing
    else do
      enc <- decodeEncoder json
      Just (Just enc)

decodeEncoder :: Json -> Maybe TwisterEncoder
decodeEncoder json = do
  obj <- Json.toObject json
  cc <- lookupNum "cc" obj >>= makeCC
  let center = lookupNum "center" obj >>= makeMidiValue
      options = case FO.lookup "options" obj of
        Just arrJson -> do
          arr <- Json.toArray arrJson
          traverse (\j -> Json.toNumber j >>= Int.fromNumber >>= makeMidiValue) arr
        Nothing -> Nothing
  Just (TwisterCC { cc, center, options })

decodeMaybeButton :: Json -> Maybe (Maybe TwisterButton)
decodeMaybeButton json =
  if Json.isNull json
    then Just Nothing
    else do
      btn <- decodeButton json
      Just (Just btn)

decodeButton :: Json -> Maybe TwisterButton
decodeButton json = do
  obj <- Json.toObject json
  typ <- lookupStr "type" obj
  cc <- lookupNum "cc" obj >>= makeCC
  case typ of
    "toggle" -> Just (TwisterToggle { cc })
    "momentary" -> Just (TwisterMomentary { cc })
    "set" -> do
      value <- lookupNum "value" obj >>= makeMidiValue
      Just (TwisterSet { cc, value })
    _ -> Nothing
