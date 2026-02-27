module Foreign.FileIO
  ( downloadJson
  , confirm
  ) where

import Prelude
import Effect (Effect)

foreign import downloadJsonImpl :: String -> String -> Effect Unit
foreign import confirmImpl :: String -> Effect Boolean

downloadJson :: String -> String -> Effect Unit
downloadJson = downloadJsonImpl

confirm :: String -> Effect Boolean
confirm = confirmImpl
