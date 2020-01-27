module Component.Subhead where

import CSS (CSS, em, fontSize, fromString, margin, px)
import CSS.Render.Concur.React (styledEl)
import Concur.React.DOM (El, h2)
import Prelude (($), discard)

subheadStyle ∷ CSS
subheadStyle = do
  fontSize $ fromString "calc(16px + 1.5vmin)"
  margin (px 0.0) (px 0.0) (em 0.1) (px 0.0)

subhead ∷ El
subhead = styledEl h2 subheadStyle
