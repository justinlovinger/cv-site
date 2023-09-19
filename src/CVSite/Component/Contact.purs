module CVSite.Component.Contact (contact, contactStylesheet) where

import CSS (CSS, Predicate(Class), Refinement(Refinement), display, displayNone, element, em, marginBottom, minHeight, paddingLeft, paddingRight, paddingTop, px, vh, with, (?))
import CSS.Render.Concur.React (style)
import Component.Subhead (subhead, subheadStyle)
import Component.Subsubhead (subsubhead, subsubheadStyle)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, span, text)
import Concur.React.Props (className, href)
import Prelude (discard, (*>), ($))

hiddenClassName ∷ String
hiddenClassName = "TGNBncFgLRbPNks4"

contactStylesheet ∷ CSS
contactStylesheet = (element "span" `with` Refinement [ Class hiddenClassName ]) ? display displayNone

contact ∷ ∀ a. Widget HTML a
contact = div
    [ style $ minHeight (vh 100.0) ]
    [ subhead
        [ style do
            subheadStyle
            paddingTop (em 1.0)
            marginBottom (em 1.0)
            paddingLeft (px 4.0)
            paddingRight (px 4.0)
        ]
        [ text "Want to know more?" ]
    , subsubhead' -- Email
        [ text "cv@"
        , span [ className hiddenClassName ] [ text "notpartofemail" ] -- Obfuscate from bots
        , text "justinlovinger.com"
        ]
    , subsubhead' [ a [ href "https://github.com/justinlovinger" ] [ text "https://github.com/justinlovinger" ] ]
    ]
  where
    subsubhead' = subsubhead [ style $ subsubheadStyle *> marginBottom (em 1.0) ]
