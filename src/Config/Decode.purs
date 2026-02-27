module Config.Decode
  ( loadRig
  , loadPedal
  , LoadError(..)
  ) where

import Prelude

import Config.Decode.Pedal (decodePedal)
import Config.Decode.Rig (decodeRig)
import Config.Types (RigConfig)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Pedal (PedalDef)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Aff as Aff
import Effect.Exception (Error)

foreign import fetchTextImpl
  :: String
  -> (String -> Effect Unit)
  -> (Error -> Effect Unit)
  -> Effect Unit

fetchText :: String -> Aff String
fetchText url = makeAff \cb -> do
  fetchTextImpl url
    (\text -> cb (Right text))
    (\err -> cb (Left err))
  pure nonCanceler

data LoadError
  = FetchError String String
  | ParseError String
  | DecodeError String

instance Show LoadError where
  show (FetchError url msg) = "Failed to fetch " <> url <> ": " <> msg
  show (ParseError msg) = "JSON parse error: " <> msg
  show (DecodeError msg) = "Decode error: " <> msg

-- | Load and decode rig.json, then load all referenced pedal files
loadRig :: String -> Aff (Either LoadError { rig :: RigConfig, pedals :: Array PedalDef })
loadRig basePath = do
  let rigUrl = basePath <> "config/rig.json"
  eRigText <- tryFetch rigUrl
  case eRigText of
    Left err -> pure (Left err)
    Right rigText ->
      case jsonParser rigText of
        Left msg -> pure (Left (ParseError $ "rig.json: " <> msg))
        Right rigJson ->
          case decodeRig rigJson of
            Nothing -> pure (Left (DecodeError "Failed to decode rig.json"))
            Just rig -> do
              ePedals <- traverse (\entry -> do
                let url = basePath <> "config/" <> entry.file
                loadPedal url (Just entry.channel)
              ) rig.pedals
              case sequence ePedals of
                Left err -> pure (Left err)
                Right pedals -> pure (Right { rig, pedals })

-- | Load and decode a single pedal JSON file
loadPedal :: String -> Maybe Int -> Aff (Either LoadError PedalDef)
loadPedal url channelOverride = do
  eText <- tryFetch url
  case eText of
    Left err -> pure (Left err)
    Right text ->
      case jsonParser text of
        Left msg -> pure (Left (ParseError $ url <> ": " <> msg))
        Right json ->
          case decodePedal channelOverride json of
            Nothing -> pure (Left (DecodeError $ "Failed to decode " <> url))
            Just def -> pure (Right def)

tryFetch :: String -> Aff (Either LoadError String)
tryFetch url = do
  result <- Aff.attempt (fetchText url)
  case result of
    Left err -> pure (Left (FetchError url (Aff.message err)))
    Right text -> pure (Right text)

-- Re-sequence an array of Eithers
sequence :: forall e a. Array (Either e a) -> Either e (Array a)
sequence = traverse identity
