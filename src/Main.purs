module Main where

import Prelude

import Component.App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  mEl <- HA.selectElement (QuerySelector "#app")
  case mEl of
    Nothing -> liftEffect $ throw "No #app element found"
    Just el -> runUI App.component unit el
