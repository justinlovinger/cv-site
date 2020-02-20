module Component.Subtext where

import CSS (CSS, color, fontSize, fromString, margin, px)
import CSS.Render.Concur.React (styledEl)
import Color.Scheme.Website (altForeground)
import Concur.React.DOM (El, span)
import Prelude (($), discard)

subtextStyle ∷ CSS
subtextStyle = do
  color altForeground
  fontSize $ fromString "calc(0.85em + 0.35vmin)"
  margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)

subtext ∷ El
subtext = styledEl span subtextStyle 
