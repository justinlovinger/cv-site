module Component.Projects (projects) where

import CSS (em, margin, marginBottom, maxWidth, px)
import CSS.Common (auto)
import CSS.Render.Concur.React (style)
import Component.Subhead (subhead, subheadStyle)
import Component.Subsubhead (subsubhead)
import Component.Timeline (timeline)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, div', p, text)
import Concur.React.Props (href)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Tuple (Tuple(Tuple))
import Prelude ((*>), ($), map)
import Projects as P
import Web.HTML.History (URL(URL))

projects ∷ ∀ a. Widget HTML a
projects = div
  [ style $ maxWidth (px 800.0) *> margin auto auto auto auto ]
  [ subhead
      [ style $ subheadStyle *> marginBottom (em 1.0)]
      [ text "Wondering what I've done?" ]
  , timeline $ map
      (\project → Tuple
        project.date
        (div'
          [ subsubhead []
              [ case project.url of
                  Just (URL url) → a [ href url ] [ text project.name ]
                  Nothing → text project.name
              ]
          , p [] [ text project.description ]
          ]
        )
      )
      P.projects
  ]
