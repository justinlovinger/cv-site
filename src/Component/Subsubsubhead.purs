module Component.Subsubsubhead where

import CSS (CSS, em, fontSize, fromString, margin, px)
import CSS.Render.Concur.React (styledEl)
import CSS.TextAlign (textAlign, center)
import Concur.React.DOM (El, h4)
import Prelude (($), discard)

subsubsubheadStyle ∷ CSS
subsubsubheadStyle = do
  fontSize $ fromString "calc(1em + 0.5vmin)"
  margin (px 0.0) (px 0.0) (em 0.1) (px 0.0)
  textAlign center

subsubsubhead ∷ El
subsubsubhead = styledEl h4 subsubsubheadStyle 
