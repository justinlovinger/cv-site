module Component.App where

import Prelude

import CSS (GenericFontFamily(..), element, fontFamily, fromString, key, render, renderedSheet, sansSerif, (?))
import Component.Indicator (bouncePauseStylesheet)
import Component.Welcome (welcome)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div')
import Data.Maybe (fromJust)
import Data.NonEmpty ((:|))
import Partial.Unsafe (unsafePartial)

stylesheet ∷ String
stylesheet = unsafePartial fromJust $ renderedSheet $ render do
  element "body" ? do
    key (fromString "margin") "0"
    fontFamily [ ] (GenericFontFamily (fromString "system-ui") :| [ sansSerif ])
  
  bouncePauseStylesheet

app ∷ ∀ a. Widget HTML a
app = div' [ welcome ]    
