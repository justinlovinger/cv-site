module Component.Subtext where

import Prelude

import CSS (CSS, color, fontSize, fromString, margin, px)
import CSS.Render.Concur.React (styledEl)
import Color.Scheme.SixteenAnsi (ColorScheme)
import Concur.React.DOM (El, span)

subtextStyle ∷ ColorScheme → CSS
subtextStyle c = do
  color c.altForeground
  fontSize $ fromString "calc(0.85em + 0.35vmin)"
  margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)

subtext ∷ ColorScheme → El
subtext = styledEl span <<< subtextStyle 
