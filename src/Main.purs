module Main (main) where

import Prelude

import App.Main as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  target <- HA.selectElement (QuerySelector "#app")
  case target of
    Just el -> void (runUI App.component unit el)
    Nothing -> HA.awaitBody >>= \body -> void (runUI App.component unit body)
