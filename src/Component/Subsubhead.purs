module Component.Subsubhead where

import CSS (CSS, em, fontSize, fromString, margin, px)
import CSS.Render.Concur.React (styledEl)
import CSS.TextAlign (textAlign, center)
import Concur.React.DOM (El, h3)
import Prelude (($), discard)

subsubheadStyle ∷ CSS
subsubheadStyle = do
  fontSize $ fromString "calc(1em + 1.5vmin)"
  margin (px 0.0) (px 0.0) (em 0.1) (px 0.0)
  textAlign center

subsubhead ∷ El
subsubhead = styledEl h3 subsubheadStyle 
