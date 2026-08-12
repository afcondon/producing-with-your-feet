-- | HTTP client for pwyf-store.
-- |
-- | Every call returns `Either Error`, never throws: the store being
-- | unreachable is an ordinary condition, not a failure. The app falls back to
-- | its localStorage cache and carries on, which is the whole point of the
-- | cache-and-sync arrangement — the iPad has to keep working when the machine
-- | serving the store is asleep.
module Foreign.Remote
  ( storeBaseUrl
  , getSnapshot
  , putSnapshot
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, makeAff, nonCanceler)
import Effect.Exception (Error)

foreign import storeBaseUrlImpl :: Effect String

foreign import requestImpl
  :: String
  -> String
  -> String
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

-- | Derived from the origin the app was served from — see Remote.js.
storeBaseUrl :: Effect String
storeBaseUrl = storeBaseUrlImpl

request :: String -> String -> String -> Aff String
request method url body = makeAff \cb -> do
  requestImpl method url body (cb <<< Left) (cb <<< Right)
  pure nonCanceler

getSnapshot :: String -> Aff (Either Error String)
getSnapshot base = attempt (request "GET" (base <> "/api/snapshot") "")

-- | The store refuses a body that would empty a non-empty store, so a failed
-- | push here can mean "your state looked empty" rather than "network down".
-- | The 409 body says which.
putSnapshot :: String -> String -> Aff (Either Error String)
putSnapshot base body = attempt (request "PUT" (base <> "/api/snapshot") body)
