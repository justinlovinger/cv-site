module Component.App where

import Prelude

import CSS ((?), GenericFontFamily(..), Predicate(Pseudo), Refinement(Refinement), background, color, element, fontFamily, fontSize, fromString, key, noneTextDecoration, pct, render, renderedSheet, sansSerif, textDecoration, with)
import Color.Scheme.Website as C
import Component.Contact (contact, contactStylesheet)
import Component.Indicator (bouncePauseStylesheet)
import Component.Introduction (introduction)
import Component.Lifeline (lifeline, lifelineStylesheet)
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
    fontSize (pct 100.0)
    color C.foreground
    background C.background

  element "a" ? textDecoration noneTextDecoration
  (element "a" `with` Refinement [ Pseudo "hover" ]) ? textDecoration noneTextDecoration
  (element "a" `with` Refinement [ Pseudo "link" ]) ? color C.blue
  (element "a" `with` Refinement [ Pseudo "link", Pseudo "active" ]) ? color C.blue
  (element "a" `with` Refinement [ Pseudo "visited" ]) ? color C.magenta
  (element "a" `with` Refinement [ Pseudo "visited", Pseudo "active" ]) ? color C.magenta
  
  bouncePauseStylesheet
  lifelineStylesheet
  contactStylesheet

app ∷ ∀ a. Widget HTML a
app = div'
  [ introduction
  , lifeline
  , contact
  ]
