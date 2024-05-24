module CVSite.Component.Introduction (introduction) where

import CSS (CSS, absolute, alignItems, bottom, color, column, display, flex, flexDirection, fromString, justifyContent, key, left, minHeight, paddingLeft, paddingRight, pct, position, px, relative, vh)
import CSS.Common (center)
import CSS.Render.Concur.React (style, styledEl)
import CSS.TextAlign as TA
import CVSite.Color.Scheme (altForeground)
import Component.Heading (heading, headingStyle)
import Component.Indicator (indicator)
import Component.Subhead (subhead, subheadStyle)
import Component.Subsubhead (subsubhead)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El, div, text)
import Concur.React.Props (_id)
import Prelude (($), (*>), discard)

heroStyle ∷ CSS
heroStyle = do
  display flex
  position relative
  flexDirection column
  alignItems center
  justifyContent center
  TA.textAlign TA.center
  minHeight $ vh 100.0

hero ∷ El
hero = styledEl div heroStyle

introduction ∷ ∀ a. Widget HTML a
introduction = hero
    [ _id "introduction" ]
    [ subhead [ style $ subheadStyle *> hpadding ] [ text "Hello! I'm" ]
    , heading [ style $ headingStyle *> hpadding ] [ text "Justin Lovinger" ]
    , div
        [ style $ color altForeground *> hpadding ]
        [ subsubhead [] [ text "Machine Learning Expert" ]
        , subsubhead [] [ text "Full Stack Web Developer" ]
        , subsubhead [] [ text "Amateur Bunny Photographer" ]
        ]
    , div
        [ style $ do
            position absolute
            bottom (vh 3.0)
            left (pct 50.0)
            key (fromString "transform") "translateX(-50%)"
        ]
        [ indicator ]
    ]
  where
    hpadding = paddingLeft (px 4.0) *> paddingRight (px 4.0)
