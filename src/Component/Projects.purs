module Component.Projects (projects) where

import CSS (display, em, flex, flexWrap, inlineBlock, justifyContent, margin, marginBottom, maxWidth, paddingLeft, px, spaceAround, wrap)
import CSS.Common (auto, none)
import CSS.ListStyle.Type (listStyleType)
import CSS.Render.Concur.React (style)
import CSS.Text.Transform (capitalize, textTransform)
import CSS.TextAlign (center, leftTextAlign, textAlign)
import Component.Subhead (subhead, subheadStyle)
import Component.Subsubsubhead (subsubsubhead)
import Component.Timeline (timeline)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, input, label', li', text, ul)
import Concur.React.Props (_type, checked, onChange)
import Concur.React.Widgetable (toWidget)
import Data.Array (concat, filter, foldl, zip)
import Data.Date (Date)
import Data.HashSet (HashSet, delete, fromArray, insert, intersection)
import Data.HeytingAlgebra (conj)
import Data.Tag (Tag, has, hasIn, isIn, tags, toTag)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Prelude ((*>), ($), (<>), (<$), (<<<), bind, discard, map, show)
import Projects as P

type TimelineItem a = { date ∷ Date, tags ∷ HashSet Tag, widget ∷ Widget HTML a }

projects ∷ ∀ a. Widget HTML a
projects = projects' defaultActiveFilters
  where defaultActiveFilters = fromArray $ map fst $ filter snd $ concat $ map snd filtersMaster

projects' ∷ ∀ a. HashSet Tag → Widget HTML a
projects' activeFilters = do
    cb ← div
      [ style $ maxWidth (px 800.0) *> margin auto auto auto auto ]
      [ subhead
          [ style $ subheadStyle *> marginBottom (em 1.0) ]
          [ text "Wondering what I've done?" ]
      , div
          [ style do
              display flex
              flexWrap wrap
              justifyContent spaceAround
          ]
          (map
            (\(Tuple c t) → filterBlock c $ zip t $ map (has activeFilters) t)
            filtersUI
          )
      , timeline $ map
          (\i → Tuple i.date i.widget)
          (filter
            (\i → foldl conj true $ map (\f → intersection f activeFilters `hasIn` i.tags) filtersByCategory)
            timelineItems
          )
      ]

    -- Recurse with given checkbox flipped
    projects' $ if cb `isIn` activeFilters
                  then delete cb activeFilters
                  else insert cb activeFilters
  where
    filtersUI ∷ Array (Tuple String (Array Tag))
    filtersUI = map (map (map fst)) filtersMaster -- Note: `map` over `Tuple` is like `over2`

    filtersByCategory ∷ Array (HashSet Tag)
    filtersByCategory = map (fromArray <<< map fst <<< snd) filtersMaster

-- | The filters master data structure.
-- | Deconstruct it
-- | for the filter UI,
-- | active filters,
-- | and more.
filtersMaster ∷ Array (Tuple String (Array (Tuple Tag Boolean)))
filtersMaster =
  [ Tuple "type"
      [ Tuple (toTag P.Website) true
      , Tuple (toTag P.Library) true
      , Tuple (toTag P.Game) true
      ]
  , Tuple "language"
      [ Tuple (toTag P.Python) true
      , Tuple (toTag P.Javascript) true
      , Tuple (toTag P.CSharp) true
      ]
  , Tuple "scope"
      [ Tuple (toTag P.Major) true
      , Tuple (toTag P.Medium) true
      , Tuple (toTag P.Minor) false
      ]
  ]

filterBlock ∷ String → Array (Tuple Tag Boolean) → Widget HTML Tag
filterBlock category filters = div
    [ style $ textAlign center *> textTransform capitalize ]
    [ subsubsubhead [] [ text $ category ]
    , ul
        [ style do
            listStyleType none
            paddingLeft (px 0.0)
            display inlineBlock
            textAlign leftTextAlign
        ]
        (map
          (\(Tuple tag isChecked) → li' [ filterWidget tag isChecked ])
          filters
        )
    ]

filterWidget ∷ Tag → Boolean → Widget HTML Tag
filterWidget tag isChecked = label'
  [ input [ tag <$ onChange, _type "checkbox", checked isChecked ]
  , text $ " " <> (show tag)
  ]

timelineItems ∷ ∀ a. Array (TimelineItem a)
timelineItems = map
  (\p → { date : P.updated p, tags : tags p, widget : toWidget p })
  P.projects
