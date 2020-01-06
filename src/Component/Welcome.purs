module Component.Welcome (welcome, welcomeStyle) where

import CSS (CSS, alignItems, backgroundColor, black, borderRadius, color, column, display, em, flex, flexDirection, fontSize, fontWeight, fromString, inlineBlock, justifyContent, key, keyframes, letterSpacing, marginBottom, marginTop, minHeight, noneTextDecoration, padding, px, rgb, textDecoration, vh, weight, white)
import CSS.Common (center, middle)
import CSS.Render.Concur.React (style, styledEl)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign as TextAlign
import CSS.VerticalAlign (verticalAlign)
import Component.Heading (heading)
import Component.Subhead (subhead)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El, div, text)
import Concur.React.DOM as D
import Concur.React.Props (_id, onClick)
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
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

buttonStyle ∷ CSS
buttonStyle = do
  key (fromString "-webkit-appearance") "none"
  key (fromString "-moz-appearance") "none"
  key (fromString "border") "none" -- Remove default button drop shadow

  display inlineBlock
  verticalAlign middle
  TextAlign.textAlign TextAlign.center

  padding (px 16.0) (px 32.0) (px 16.0) (px 32.0)
  borderRadius (px 4.0) (px 4.0) (px 4.0) (px 4.0)

  color white
  backgroundColor $ rgb 0 103 238

  textTransform uppercase

  textDecoration noneTextDecoration

  fontSize $ fromString "calc(12px + 0.5vmin)"
  fontWeight $ weight 700.0
  letterSpacing $ em 0.1

  key (fromString "cursor") "pointer"

button ∷ El
button = styledEl D.button buttonStyle

-- No support for multiple percents per frame, such as
-- 0%, 10%, 20%, 30% {
--   transform: translateY(0) translateX(-50%);
-- }
-- 5%, 15%, 25% {
--   transform: translateY(-50%) translateX(-50%);
-- }
bouncePause ∷ CSS
bouncePause = keyframes "bounce-pause" $
    0.0  /\ key (fromString "transform") "translateY(0)" :|
  [ 5.0  /\ key (fromString "transform") "translateY(-50%)"
  , 10.0 /\ key (fromString "transform") "translateY(0)"
  , 15.0 /\ key (fromString "transform") "translateY(-50%)"
  , 20.0 /\ key (fromString "transform") "translateY(0)"
  , 25.0 /\ key (fromString "transform") "translateY(-50%)"
  , 30.0 /\ key (fromString "transform") "translateY(0)" ]

indicatorStyle ∷ CSS
indicatorStyle = do
  marginTop $ px 8.0
  marginBottom $ px 8.0

  -- animation is missing support for 'ease' timing function and 'none' fill mode
  -- animation (fromString "bounce-pause") (sec 10.0) ease (sec 0.0) infinite normalAnimationDirection none
  key (fromString "animation") "bounce-pause 10s ease infinite"

  -- Missing support for following. Using escape hatch.
  key (fromString "cursor") "default"
  key (fromString "user-select") "none"
  key (fromString "-webkit-touch-callout") "none"

indicator ∷ ∀ a. Widget HTML a
indicator = el [ ] [ text "╲╱" ] where
  el = styledEl div indicatorStyle 

welcomeStyle ∷ CSS
welcomeStyle = do
  bouncePause

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
