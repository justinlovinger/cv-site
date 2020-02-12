module Component.Contact (contact, contactStylesheet) where

import CSS ((?), CSS, Predicate(Class), Refinement(Refinement), display, displayNone, element, em, margin, marginBottom, maxWidth, minHeight, paddingTop, px, vh, with)
import CSS.Common (auto)
import CSS.Render.Concur.React (style)
import Component.Subhead (subhead, subheadStyle)
import Component.Subsubhead (subsubhead)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, span, text)
import Concur.React.Props (className, href)
import Prelude ((*>), ($))

hiddenClassName ∷ String
hiddenClassName = "TGNBncFgLRbPNks4"

contactStylesheet ∷ CSS
contactStylesheet = (element "span" `with` Refinement [ Class hiddenClassName ]) ? display displayNone

contact ∷ ∀ a. Widget HTML a
contact = div
  [ style $ maxWidth (px 800.0) *> margin auto auto auto auto *> minHeight (vh 100.0) ]
  [ subhead
      [ style $ subheadStyle *> paddingTop (em 1.0) *> marginBottom (em 1.0)]
      [ text "Want to know more?" ]
  , subsubhead [] -- Email
      [ text "cv@"
      , span [ className hiddenClassName ] [ text "notpartofemail" ] -- Obfuscate from bots
      , text "justinlovinger.com"
      ]
  , subsubhead [] [ a [ href "https://github.com/JustinLovinger" ] [ text "https://github.com/JustinLovinger" ] ]
  ]
