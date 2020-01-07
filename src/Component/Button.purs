module Component.Button (button, buttonStyle) where

import Prelude

import CSS (CSS, backgroundColor, borderRadius, color, display, em, fontSize, fontWeight, fromString, inlineBlock, key, letterSpacing, noneTextDecoration, padding, px, rgb, textDecoration, weight, white)
import CSS.Common (middle)
import CSS.Render.Concur.React (styledEl)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (textAlign, center)
import CSS.VerticalAlign (verticalAlign)
import Concur.React.DOM as D

buttonStyle ∷ CSS
buttonStyle = do
  key (fromString "-webkit-appearance") "none"
  key (fromString "-moz-appearance") "none"
  key (fromString "border") "none" -- Remove default button drop shadow

  display inlineBlock
  verticalAlign middle
  textAlign center

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

button ∷ D.El
button = styledEl D.button buttonStyle
