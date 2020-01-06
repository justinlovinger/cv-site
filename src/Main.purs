module Main where

import Prelude

import Component.App (app, stylesheet)
import Concur.React.DOM (text)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)

main ∷ Effect Unit
main = do
  runWidgetInDom "styleroot" $ text stylesheet
  runWidgetInDom "root" app
