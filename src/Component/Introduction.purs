module Component.Introduction (introduction) where

import CSS (CSS, absolute, alignItems, bottom, color, column, display, flex, flexDirection, fromString, justifyContent, key, left, minHeight, pct, position, px, vh)
import CSS.Common (center)
import CSS.Render.Concur.React (style, styledEl)
import CSS.TextAlign as TA
import Color.Scheme.Website (altForeground)
import Component.Heading (heading)
import Component.Indicator (indicator)
import Component.Subhead (subhead)
import Component.Subsubhead (subsubhead)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El, div, text)
import Concur.React.Props (_id)
import Prelude (($), discard)

heroStyle ∷ CSS
heroStyle = do
  display flex
  flexDirection column
  alignItems center
  justifyContent center
  TA.textAlign TA.center
  minHeight $ vh 100.0

hero ∷ El
hero = styledEl div heroStyle

introduction ∷ ∀ a. Widget HTML a
introduction = hero
  [ _id "welcome" ]
  [ subhead [] [ text "Hello! I'm" ]
  , heading [] [ text "Justin Lovinger" ]
  , div
    [ style $ color altForeground ]
    [ subsubhead [] [ text "Machine Learning Expert" ]
    , subsubhead [] [ text "Full Stack Web Developer" ]
    , subsubhead [] [ text "Amateur Bunny Photographer" ]
    ]
  , div
      [ style $ do
          position absolute
          bottom (px 20.0)
          left (pct 50.0)
          key (fromString "transform") "translateX(-50%)"
      ]
      [ indicator ]
  ]
