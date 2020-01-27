module Component.Subsubhead where

import CSS (CSS, em, fontSize, fromString, margin, px)
import CSS.Render.Concur.React (styledEl)
import Concur.React.DOM (El, h3)
import Prelude (($), discard)

subsubheadStyle ∷ CSS
subsubheadStyle = do
  fontSize $ fromString "calc(12px + 1.0vmin)"
  margin (px 0.0) (px 0.0) (em 0.1) (px 0.0)

subsubhead ∷ El
subsubhead = styledEl h3 subsubheadStyle 
