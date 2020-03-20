module CVSite.Component.Introduction (introduction) where

import CSS (CSS, absolute, alignItems, bottom, color, column, display, flex, flexDirection, fromString, height, justifyContent, key, left, marginBottom, marginTop, minHeight, paddingLeft, paddingRight, pct, position, px, relative, top, vh, width, zIndex)
import CSS.Common (center)
import CSS.Overflow (hidden, overflow)
import CSS.Render.Concur.React (style, styledEl)
import CSS.TextAlign as TA
import CVSite.Color.Scheme (altForeground)
import CVSite.Component.DynamicCircles (dynamicCircles)
import CVSite.Component.Subtext (subtext, subtextStyle)
import Component.Heading (heading, headingStyle)
import Component.Indicator (indicator)
import Component.Subhead (subhead, subheadStyle)
import Component.Subsubhead (subsubhead)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El, div, text)
import Concur.React.Props (_id)
import Control.Alt ((<|>))
import Data.Int (toNumber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Prelude (($), (*>), bind, discard, negate)
import Web.Event.Resize (waitForResize)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

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
    [ div
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
        [ fullScreenDynamicCircles ]
    , subtext
        [ style $ subtextStyle *> marginBottom hintspace *> hpadding ]
        [ text "click-hold the background" ] -- Hint
    , subhead [ style $ subheadStyle *> hpadding ] [ text "Hello! I'm" ]
    , heading [ style $ headingStyle *> hpadding ] [ text "Justin Lovinger" ]
    , div
        [ style $ color altForeground *> hpadding ]
        [ subsubhead [] [ text "Machine Learning Expert" ]
        , subsubhead [] [ text "Full Stack Web Developer" ]
        , subsubhead [] [ text "Amateur Bunny Photographer" ]
        ]
    , div [ style $ marginTop hintspace ] [] -- Spacer, for hint
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
    hintspace = vh 10.0

    hpadding = paddingLeft (px 4.0) *> paddingRight (px 4.0)

    fullScreenDynamicCircles ∷ ∀ b. Widget HTML b
    fullScreenDynamicCircles = do
      _window ← liftEffect window
      _innerWidth ← liftEffect $ innerWidth _window
      _innerHeight ← liftEffect $ innerHeight _window
      dynamicCircles (toNumber _innerWidth) (toNumber _innerHeight)
        <|> (liftAff waitForResize) -- Update canvas size on resize
      fullScreenDynamicCircles
