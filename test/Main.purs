module Test.Main where

import Prelude

import CSS.Render.Concur.ReactSpec (reactSpec)
import CVSite.Data.Tags.EncodeSpec (encodeSpec)
import Data.HashSet.ExtSpec (extSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  extSpec
  reactSpec
  encodeSpec
