module Component.Welcome (welcome) where

import CSS (CSS, alignItems, backgroundColor, black, color, column, display, flex, flexDirection, justifyContent, minHeight, vh, white)
import CSS.Common (center)
import CSS.Render.Concur.React (style, styledEl)
import Component.Button (button)
import Component.Heading (heading)
import Component.Indicator (indicator)
import Component.Subhead (subhead)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El, div, text)
import Concur.React.Props (_id, onClick)
import Prelude (($), (*>), (<>), bind, discard)

heroStyle ∷ CSS
heroStyle = do
  display flex
  flexDirection column
  alignItems center
  justifyContent center
  minHeight $ vh 100.0

hero ∷ El
hero = styledEl div heroStyle

welcome ∷ ∀ a. Widget HTML a
welcome =
  let
    makeWelcome props ctaText = hero
      ([ _id "welcome" ] <> props)
      [ heading [] [ text "Welcome to the Internet" ]
      , subhead [] [ text "The Internet Is Full of Buttons" ]
      , indicator
      , button [ onClick ] [ text ctaText ]
      ]
  in do
    _ ← makeWelcome [] "It's Too Bright!"
    _ ← makeWelcome
      [ style $ heroStyle *> color white *> backgroundColor black ]
      "It's Too Dark!"
    welcome
