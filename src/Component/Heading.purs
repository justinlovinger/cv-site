module Component.Heading where

import CSS (CSS, em, fontSize, fromString, margin, px)
import CSS.Render.Concur.React (styledEl)
import CSS.TextAlign (textAlign, center)
import Concur.React.DOM (El, h1)
import Prelude (($), discard)

headingStyle ∷ CSS
headingStyle = do
  fontSize $ fromString "calc(32px + 2.5vmin)"
  margin (px 0.0) (px 0.0) (em 0.1) (px 0.0)
  textAlign center

heading ∷ El
heading = styledEl h1 headingStyle
