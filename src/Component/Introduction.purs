module Component.Introduction (introduction) where

import CSS (CSS, absolute, alignItems, bottom, color, column, display, em, flex, flexDirection, fontSize, fromString, height, justifyContent, key, left, marginBottom, marginTop, minHeight, pct, position, px, top, vh, width, zIndex)
import CSS.Common (center)
import CSS.Overflow (hidden, overflow)
import CSS.Render.Concur.React (style, styledEl)
import CSS.TextAlign as TA
import Color.Scheme.Website (altForeground)
import Component.DynamicCircles (dynamicCircles)
import Component.Heading (heading)
import Component.Indicator (indicator)
import Component.Subhead (subhead)
import Component.Subsubhead (subsubhead)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El, div, text)
import Concur.React.Props (_id)
import Control.Alt ((<|>))
import Data.Int (toNumber)
import Effect.AVar (empty, tryPut)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Prelude (($), bind, discard, negate)
import Web.Event.Resize (waitForResize)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

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
introduction = do
    hero
      [ _id "welcome" ]
      [ fullScreenBackgroundDynamicCircles
      , div
          [ style do
              color altForeground
              fontSize (em 0.7)
              marginBottom space
          ]
          [ text "click-hold the background" ] -- Hint
      , subhead [] [ text "Hello! I'm" ]
      , heading [] [ text "Justin Lovinger" ]
      , div
        [ style $ color altForeground ]
        [ subsubhead [] [ text "Machine Learning Expert" ]
        , subsubhead [] [ text "Full Stack Web Developer" ]
        , subsubhead [] [ text "Amateur Bunny Photographer" ]
        ]
      , div [ style $ marginTop space ] [] -- Spacer, for hint
      , div
          [ style $ do
              position absolute
              bottom (px 20.0)
              left (pct 50.0)
              key (fromString "transform") "translateX(-50%)"
          ]
          [ indicator ]
      ]
  where
    space = vh 10.0
    fullScreenBackgroundDynamicCircles = do
      _window ← liftEffect window
      _innerWidth ← liftEffect $ innerWidth _window
      _innerHeight ← liftEffect $ innerHeight _window
      dynamicCirclesWillUnmount ← liftEffect empty
      div
        [ style $ do
            position absolute
            top (px 0.0)
            left (px 0.0)
            width (pct 100.0)
            height (pct 100.0)
            zIndex (-1000)
            -- The `dynamicCircles` component may end up slightly larger
            -- than the window
            -- due to scrollbars.
            -- Hiding the overflow
            -- fixes that.
            overflow hidden
        ]
        [ dynamicCircles dynamicCirclesWillUnmount (toNumber _innerWidth) (toNumber _innerHeight) ]
        <|> (liftAff waitForResize) -- Update canvas size on resize
      _ ← liftEffect $ tryPut true dynamicCirclesWillUnmount
      fullScreenBackgroundDynamicCircles
