module Foreign.FileIO
  ( downloadJson
  , confirm
  , readFileAsText
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)
import Data.Either (Either(..))

foreign import downloadJsonImpl :: String -> String -> Effect Unit
foreign import confirmImpl :: String -> Effect Boolean
foreign import readFileAsTextImpl :: String -> (Error -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit

downloadJson :: String -> String -> Effect Unit
downloadJson = downloadJsonImpl

confirm :: String -> Effect Boolean
confirm = confirmImpl

-- | Open a file picker dialog and read the selected file as text.
-- | The accept parameter filters file types (e.g. ".json").
readFileAsText :: String -> Aff String
readFileAsText accept = makeAff \cb -> do
  readFileAsTextImpl accept
    (\err -> cb (Left err))
    (\text -> cb (Right text))
  pure nonCanceler
