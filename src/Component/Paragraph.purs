module Component.Paragraph where

import CSS (CSS, em, fontSize, fromString, margin, px)
import CSS.Render.Concur.React (styledEl)
import Concur.React.DOM (El, p)
import Prelude (($), discard)

paragraphStyle ∷ CSS
paragraphStyle = do
  fontSize $ fromString "calc(1em + 0.5vmin)"
  margin (em 0.5) (px 0.0) (px 0.0) (px 0.0)

paragraph ∷ El
paragraph = styledEl p paragraphStyle 
