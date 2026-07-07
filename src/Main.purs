module Main where

import Prelude

import App.REPL as REPL
import Effect
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI REPL.component unit body
