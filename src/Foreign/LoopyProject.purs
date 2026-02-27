module Foreign.LoopyProject
  ( generateAndDownload
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)
import Data.Either (Either(..))

foreign import generateAndDownloadImpl :: String -> (Error -> Effect Unit) -> (Unit -> Effect Unit) -> Effect Unit

-- | Generate a LoopyPro .lpproj bundle and download it.
-- | The project name becomes the filename (<name>.lpproj).
generateAndDownload :: String -> Aff Unit
generateAndDownload projectName = makeAff \cb -> do
  generateAndDownloadImpl projectName
    (\err -> cb (Left err))
    (\_ -> cb (Right unit))
  pure nonCanceler
