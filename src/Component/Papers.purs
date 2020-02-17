module Component.Papers (papers) where

import CSS (em, margin, marginBottom, marginTop, maxWidth, px)
import CSS.Common (auto)
import CSS.TextAlign (leftTextAlign, textAlign)
import CSS.Render.Concur.React (style)
import Component.Paragraph (paragraph)
import Component.Subhead (subhead, subheadStyle)
import Component.Subsubhead (subsubhead, subsubheadStyle)
import Component.Subtext (subtext)
import Component.Timeline (timeline)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, div', text)
import Concur.React.Props (href)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Tuple (Tuple(Tuple))
import Prelude ((*>), ($), (<>), map, show)
import Papers as P
import Web.HTML.History (URL(URL))

papers ∷ ∀ a. Widget HTML a
papers = div
    [ style $ maxWidth (px 800.0) *> margin auto auto auto auto ]
    [ subhead
        [ style $ subheadStyle *> marginTop (em 1.0) *> marginBottom (em 1.0)]
        [ text "What about academic works?" ]
    , timeline $ map
        (\paper → Tuple
          paper.published
          (div'
            [ subsubhead
                [ style $ subsubheadStyle *> textAlign leftTextAlign ]
                [ a [ href $ fromURL paper.documentUrl ] [ text paper.name ] ]
            , subtext []
                [ case paper.url of
                    Just (URL url) → a [ href url ] [ text $ show paper.type ]
                    Nothing → text $ show paper.type
                ]
            , paragraph [] [ text (paper.description <> ".") ]
            ]
          )
        )
        P.papers
    ]
  where
    fromURL (URL url) = url
