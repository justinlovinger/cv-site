module Component.Lifeline (lifeline) where

import CSS (color, display, em, flex, flexBasis, flexGrow, flexWrap, fromString, inlineBlock, margin, marginBottom, marginLeft, marginRight, marginTop, maxWidth, padding, paddingLeft, paddingRight, px, textWhitespace, whitespaceNoWrap, wrap)
import CSS.Common (auto, none)
import CSS.ListStyle.Type (listStyleType)
import CSS.Render.Concur.React (style)
import CSS.Text.Transform (capitalize, lowercase, textTransform)
import CSS.TextAlign (center, leftTextAlign, textAlign)
import Color (Color)
import Color.Scheme.Website (brightBlue, brightGreen, brightYellow)
import Component.Checkbox (checkbox')
import Component.Paragraph (paragraph)
import Component.Subhead (subhead, subheadStyle)
import Component.Subsubhead (subsubhead, subsubheadStyle)
import Component.Subsubsubhead (subsubsubhead, subsubsubheadStyle)
import Component.Subtext (subtext, subtextStyle)
import Component.Timeline (timeline)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, div', label', li, li', span, text, ul)
import Concur.React.Props (href, onChange)
import Control.MultiAlternative (orr)
import Data.Array (concat, filter, foldl, mapMaybe, sort, sortBy, uncons, zip)
import Data.Date (Date)
import Data.HashSet (HashSet, delete, fromArray, insert, intersection, toArray, union)
import Data.HeytingAlgebra ((&&), conj, disj, not)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(Pattern), contains, joinWith, split, stripPrefix)
import Data.Tag (Tag, has, hasIn, isIn, tag, tags, toTag)
import Data.Tag.Encode (urlDecode, urlEncode)
import Data.Tuple (Tuple(Tuple), fst, lookup, snd)
import Data.Tuple.Nested (over1)
import Education as E
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Papers as Pa
import Prelude (bind, compare, discard, map, pure, show, unit, ($), (*>), (-), (<$), (<<<), (<>), (/))
import Projects as Pr
import Web.HTML (window)
import Web.HTML.HTMLDocument (title)
import Web.HTML.History (DocumentTitle(DocumentTitle), URL(URL), replaceState)
import Web.HTML.Location (search)
import Web.HTML.Window (document, history, location)

type TimelineItem a = { borderColor ∷ Color
                      , date ∷ Date
                      , tags ∷ HashSet Tag
                      , widget ∷ Widget HTML a
                      }

lifeline ∷ ∀ a. Widget HTML a
lifeline = do
    -- Try to get from url, before falling back to defaults
    window_ ← liftEffect window
    location_ ← liftEffect $ location window_
    querystring ← liftEffect $ search location_

    let activeFilters ∷ Maybe (HashSet Tag)
        activeFilters = maybe Nothing urlDecode $ lookup urlFiltersKey queries

        queries ∷ Array (Tuple String String)
        queries = mapMaybe (maybe Nothing (\{head, tail} → Just $ Tuple head (joinWith "=" $ tail)) <<< uncons) $ map (split $ Pattern "=") $ filter (contains $ Pattern "=") $ split (Pattern "&") querystring'

        querystring' ∷ String
        querystring' = fromMaybe querystring $ stripPrefix (Pattern "?") querystring 

    lifeline' $ fromMaybe defaultActiveFilters activeFilters
  where
    defaultActiveFilters =
      (fromArray $ map (fst <<< fst) filtersMaster) -- Super-category tags
      `union`
      (fromArray $ map fst $ filter snd $ concat $ map snd $ concat $ map snd $ filtersMaster) -- Category tags

lifeline' ∷ ∀ a. HashSet Tag → Widget HTML a
lifeline' activeFilters = do
    cb ← div'
      [ subhead
          [ style do
              subheadStyle
              marginBottom (em 1.0)
              paddingLeft (px 4.0)
              paddingRight (px 4.0)
          ]
          [ text "Wondering what I've done?" ]
      , div
          [ style do
              display flex
              flexWrap wrap
              maxWidth (px 1200.0)
              margin auto auto auto auto
              paddingLeft $ em (filterSuperblockMarginEm / 2.0)
              paddingRight $ em (filterSuperblockMarginEm / 2.0)
          ]
          (map
            (\(Tuple (Tuple t c) cs) → filterSuperblock
              t
              c
              (activeFilters `has` t)
              -- Zip filter tags with active boolean
              (map (over2 (\ts → zip ts $ map (has activeFilters) ts)) cs)
            )
            filtersUI
          )
      , div
          [ style do
              maxWidth (px 850.0)
              margin auto auto auto auto
              paddingLeft $ px 4.0
              paddingRight $ px 4.0
          ]
          [ timeline $ map
              (\i → { borderColor : Just i.borderColor, date : i.date, widget : i.widget })
              (filter
                (\i → foldl disj false $ map
                  (\(Tuple t tagSets) →
                    activeFilters `has` t
                    && i.tags `has` t
                    -- Item has a least 1 tag from each category
                    && (foldl conj true $ map (\f → intersection f activeFilters `hasIn` i.tags) tagSets))
                  filtersByCategory
                )
                timelineItems
              )
          ]
      ]

    -- Update `activeFilters`
    let activeFilters' = if cb `isIn` activeFilters
      then delete cb activeFilters
      else insert cb activeFilters


    -- Update url
    window_ ← liftEffect window
    document_ ← liftEffect $ document window_
    documentTitle ← liftEffect $ title document_
    history_ ← liftEffect $ history window_
    liftEffect case urlEncode activeFilters' of
      Just filtersStr → replaceState (unsafeToForeign "") (DocumentTitle documentTitle) (URL $ "?" <> urlFiltersKey <> "=" <> filtersStr) history_
      Nothing → pure unit

    -- Recurse with given checkbox flipped
    lifeline' activeFilters'
  where
    filtersUI ∷ Array (Tuple (Tuple Tag Color) (Array (Tuple String (Array Tag))))
    filtersUI = map (over2 (map (over2 (map fst)))) filtersMaster

    filtersByCategory ∷ Array (Tuple Tag (Array (HashSet Tag)))
    filtersByCategory = map (over1 fst) $ map (over2 (map (fromArray <<< map fst <<< snd))) filtersMaster

    over2 = map -- `map` over `Tuple` is like `over2`

urlFiltersKey ∷ String
urlFiltersKey = "f"

-- | The filters master data structure.
-- | Deconstruct it
-- | for the filter UI,
-- | active filters,
-- | and more.
filtersMaster ∷ Array (Tuple (Tuple Tag Color) (Array (Tuple String (Array (Tuple Tag Boolean)))))
filtersMaster =
  [ Tuple (Tuple educationTag educationColor)
      [ Tuple "degree"
          [ Tuple (toTag E.Masters) true
          , Tuple (toTag E.Bachelors) false
          ]
      ]
  , Tuple (Tuple paperTag paperColor)
      [ Tuple "type"
          [ Tuple (toTag Pa.Thesis) true
          , Tuple (toTag Pa.Journal) true
          , Tuple (toTag Pa.Conference) false
          ]
      , Tuple "topic"
          [ Tuple (toTag Pa.MachineLearning) true
          , Tuple (toTag Pa.Optimization) true
          , Tuple (toTag Pa.IncrementalLearning) true
          , Tuple (toTag Pa.Search) true
          , Tuple (toTag Pa.TextSummarization) true
          ]
      ]
  , Tuple (Tuple projectTag projectColor)
      [ Tuple "type"
          [ Tuple (toTag Pr.Website) true
          , Tuple (toTag Pr.PWA) true
          , Tuple (toTag Pr.Library) true
          , Tuple (toTag Pr.Template) true
          , Tuple (toTag Pr.Game) false
          ]
      , Tuple "topic"
          [ Tuple (toTag Pr.MachineLearning) true
          , Tuple (toTag Pr.Optimization) true
          , Tuple (toTag Pr.TextSummarization) true
          , Tuple (Pr.noTopics) true
          ]
      , Tuple "language"
          [ Tuple (toTag Pr.Nix) true
          , Tuple (toTag Pr.PureScript) true
          , Tuple (toTag Pr.Python) true
          , Tuple (toTag Pr.Javascript) true
          , Tuple (toTag Pr.CSharp) true
          ]
      , Tuple "scope"
          [ Tuple (toTag Pr.Major) true
          , Tuple (toTag Pr.Medium) true
          , Tuple (toTag Pr.Minor) false
          ]
      ]
  ]

filterSuperblock ∷ Tag → Color → Boolean → Array (Tuple String (Array (Tuple Tag Boolean))) → Widget HTML Tag
filterSuperblock t c isChecked filters = div
  [ style do
      flexGrow 1
      flexBasis (px 0.0)
      marginLeft $ em (filterSuperblockMarginEm / 2.0)
      marginRight $ em (filterSuperblockMarginEm / 2.0)
      textTransform capitalize
  ]
  [ subsubhead
      [ style do
          subsubheadStyle
          color c
          marginBottom (em 0.5)
          marginLeft (fromString $ "calc(-2ex - " <> checkboxLabelSpace <> ")")
          textWhitespace whitespaceNoWrap -- Keep checkbox and label on one line
      ]
      [ filterWidget t true isChecked ]
  , div
      [ style do
          display flex
          flexWrap wrap
          padding (px 0.0) (px $ filterBlockMarginPx / 2.0) (px 0.0) (px $ filterBlockMarginPx / 2.0)
      ]
      (map (\(Tuple ct fs) → filterBlock ct fs isChecked) filters)
  ]

filterSuperblockMarginEm ∷ Number
filterSuperblockMarginEm = 2.0

filterBlock ∷ String → Array (Tuple Tag Boolean) → Boolean → Widget HTML Tag
filterBlock category filters isEnabled = div
    [ style do
        flexGrow 1
        flexBasis (px 0.0)
        margin (px 0.0) (px $ filterBlockMarginPx / 2.0) (px 0.0) (px $ filterBlockMarginPx / 2.0)
        textAlign center
        textTransform capitalize
    ]
    [ subsubsubhead [] [ text $ category ]
    , ul
        [ style do
            listStyleType none
            paddingLeft (px 0.0)
            marginBottom (em (1.0 - liMarginEm))
            display inlineBlock
            textAlign leftTextAlign
            textWhitespace whitespaceNoWrap
        ]
        (map
          (\(Tuple tag isChecked) → li [ style $ marginBottom (em liMarginEm) ] [ filterWidget tag isEnabled isChecked ])
          filters
        )
    ]
  where
    liMarginEm = 0.25

filterBlockMarginPx ∷ Number
filterBlockMarginPx = 10.0

filterWidget ∷ Tag → Boolean → Boolean → Widget HTML Tag
filterWidget tag isEnabled isChecked = label'
  [ checkbox' (not isEnabled) isChecked [ tag <$ onChange ]
  , span [ style $ marginLeft (fromString checkboxLabelSpace) ] [ text $ (show tag) ]
  ]

checkboxLabelSpace ∷ String
checkboxLabelSpace = "0.5ch"

timelineItems ∷ ∀ a. Array (TimelineItem a)
timelineItems = sortBy (\a b → compare b.date a.date) $ -- Sorted by descending date
    (map
      (\e → { borderColor : educationColor
            , date : E.graduated e
            , tags : insert educationTag (tags e)
            , widget : educationToWidget e
            }
      )
      E.education
    )
    <> (map
      (\p → { borderColor : paperColor
            , date : Pa.published p
            , tags : insert paperTag (tags p)
            , widget : paperToWidget p
            }
      )
      Pa.papers
    )
    <> (map
      (\p → { borderColor : projectColor
            , date : Pr.updated p
            , tags : insert projectTag (tags p)
            , widget : projectToWidget p
            }
      )
      Pr.projects
    )
  where
    educationToWidget e = orr $
      [ subsubhead
          [ style $ subsubheadStyle *> textAlign leftTextAlign ]
          [ text $ E.schoolName e ]
      , subsubsubhead
          [ style $ subsubsubheadStyle *> textAlign leftTextAlign ]
          [ span [ style $ textTransform capitalize ] [ text $ (show $ E.degreeType e) ]
          , text " in "
          , span
              -- Inline block to de-prioritize splitting text
              [ style $ display inlineBlock *> textTransform capitalize ]
              [ text $ E.degreeProgram e ]
          ]
      , subsubsubhead
          [ style $ subsubsubheadStyle *> textAlign leftTextAlign ]
          [ text $ (\a → a.district <> ", " <> a.state <> ", " <> a.country) (E.schoolAddress e) ]
      , subtext' [ text $ show educationTag ]
      , subtext' [ text $ show (E.degreeType e) ]
      , subtext' [ text $ "gpa: " <> (show $ E.gpa e) ]
      ] <> case (E.accolades e) of
        Just acs →
          [ paragraph
              []
              [ ul
                  [ style do
                      margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
                      paddingLeft (px 20.0)
                  ]
                  (map (\ac → li' [ text ac ]) acs)
              ]
          ]
        Nothing → []
    paperToWidget p = orr
      [ subsubhead
          [ style $ subsubheadStyle *> textAlign leftTextAlign ]
          [ a [ href $ unwrap $ Pa.documentUrl p ] [ text $ Pa.name p ] ]
      , subtext' [ text $ show paperTag ]
      , subtext'
          [ case Pa.url p of
              Just (URL url) → a
                [ href url, style do
                    -- Add some space
                    -- between this link
                    -- and the one above,
                    -- for easier tapping.
                    display inlineBlock
                    marginTop (em 0.75)
                ]
                [ text $ show $ Pa.type_ p ]
              Nothing → text $ show $ Pa.type_ p
          ]
      , tagsWidget $ Pa.topics p
      , paragraph [] [ text (Pa.description p <> ".") ]
      ]
    projectToWidget p = orr
      [ subsubhead
          [ style $ subsubheadStyle *> textAlign leftTextAlign ]
          [ case Pr.url p of
              Just (URL url) → a [ href url ] [ text $ Pr.name p ]
              Nothing → text $ Pr.name p
          ]
      -- Note: We sort tags
      -- because `HashSet` has nondeterministic ordering.
      , subtext' [ text $ show projectTag ]
      , tagsWidget $ Pr.types p
      , tagsWidget $ Pr.topics p
      , tagsWidget $ Pr.languages p
      , subtext' [ text $ show $ Pr.scope p ]
      , paragraph [] [ text (Pr.description p <> ".") ]
      ]
    tagsWidget ts = subtext' [ text $ joinWith ", " $ sort $ map show $ toArray $ ts ]
    subtext' = subtext
      [ style do
          subtextStyle
          display inlineBlock
          marginRight (em 1.0)
          textTransform lowercase
      ]

educationTag ∷ Tag
educationTag = tag "education"

educationColor ∷ Color
educationColor = brightYellow

paperTag ∷ Tag
paperTag = tag "paper"

paperColor ∷ Color
paperColor = brightBlue

projectTag ∷ Tag
projectTag = tag "project"

projectColor ∷ Color
projectColor = brightGreen
