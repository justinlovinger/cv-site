module Component.Lifeline (lifeline) where

import CSS (Abs, Size, color, display, em, flex, flexBasis, flexGrow, flexWrap, fromString, inlineBlock, margin, marginBottom, marginLeft, marginTop, maxWidth, padding, paddingLeft, paddingRight, px, textWhitespace, whitespaceNoWrap, wrap)
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
import Concur.React.DOM (a, div, div', label', li, li', span, span', text, ul)
import Concur.React.Props (href, onChange)
import Control.MultiAlternative (orr)
import Data.Array (concat, filter, foldl, mapMaybe, sort, sortBy, uncons, zip)
import Data.Date (Date)
import Data.HashSet (HashSet, delete, fromArray, insert, intersection, toArray, union)
import Data.HeytingAlgebra ((&&), conj, disj, not)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(Pattern), contains, joinWith, split, stripPrefix)
import Data.Tag (Tag, has, hasIn, isIn, tag, tags, toTag)
import Data.Tag.Encode (urlDecode, urlEncode)
import Data.Tuple (Tuple(Tuple), fst, lookup, snd)
import Data.Tuple.Nested (over1)
import Education as E
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Publications as Pu
import Prelude (bind, compare, discard, map, negate, pure, show, unit, ($), (*>), (/), (<$), (<<<), (<>))
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
              marginBottom (px 0.0)
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
              padding filterSuperblockVSpace filterSuperblockHSpace filterSuperblockVSpace filterSuperblockHSpace
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
              (\i →
                { borderColor : Just i.borderColor
                , date : i.date
                , hidden : not $ isActive i
                , widget : i.widget
                }
              )
              timelineItems
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

    isActive ∷ ∀ b. TimelineItem b → Boolean
    isActive i = foldl disj false $ map
      (\(Tuple t tagSets) →
        activeFilters `has` t
        && i.tags `has` t
        -- Item has a least 1 tag from each category
        && (foldl conj true $ map (\f → intersection f activeFilters `hasIn` i.tags) tagSets))
      filtersByCategory

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
  , Tuple (Tuple publicationTag publicationColor)
      [ Tuple "type"
          [ Tuple (toTag Pu.Thesis) true
          , Tuple (toTag Pu.Journal) true
          , Tuple (toTag Pu.Conference) false
          ]
      , Tuple "topic"
          [ Tuple (toTag Pu.MachineLearning) true
          , Tuple (toTag Pu.Optimization) true
          , Tuple (toTag Pu.IncrementalLearning) true
          , Tuple (toTag Pu.Search) true
          , Tuple (toTag Pu.TextSummarization) true
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
      margin filterSuperblockVSpace filterSuperblockHSpace filterSuperblockVSpace filterSuperblockHSpace
      textTransform capitalize
  ]
  [ subsubhead
      [ style do
          subsubheadStyle
          color c
          marginBottom (px 0.0)
          marginLeft (fromString $ "calc(-2ex - " <> checkboxLabelSpace <> ")")
          textWhitespace whitespaceNoWrap -- Keep checkbox and label on one line
      ]
      [ filterWidget t true isChecked ]
  , div
      [ style do
          display flex
          flexWrap wrap
          padding (px 0.0) filterBlockSpace (px 0.0) filterBlockSpace
      ]
      (map (\(Tuple ct fs) → filterBlock ct fs isChecked) filters)
  ]

filterSuperblockHSpace ∷ Size Abs
filterSuperblockHSpace = em (2.0 / 2.0) -- Applied twice. Double in practice.

filterSuperblockVSpace ∷ Size Abs
filterSuperblockVSpace = em (3.0 / 2.0) -- Applied twice. Double in practice.

filterBlock ∷ String → Array (Tuple Tag Boolean) → Boolean → Widget HTML Tag
filterBlock category filters isEnabled = div
    [ style do
        flexGrow 1
        flexBasis (px 0.0)
        margin (px 0.0) filterBlockSpace  (px 0.0) filterBlockSpace
        textAlign center
        textTransform capitalize
    ]
    [ subsubsubhead
        [ style do
            subsubsubheadStyle
            marginTop (em 1.0)
            marginBottom (em 1.0)
        ]
        [ text $ category ]
    , ul
        [ style do
            listStyleType none
            paddingLeft (px 0.0)
            marginTop (px 0.0)
            marginBottom (em (-liMarginEm))
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

filterBlockSpace ∷ Size Abs
filterBlockSpace = px (10.0 / 2.0) -- Applied twice. Double in practice.

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
      (\p → { borderColor : publicationColor
            , date : Pu.published p
            , tags : insert publicationTag (tags p)
            , widget : publicationToWidget p
            }
      )
      Pu.publications
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
      , subtext'
          [ span'
              [ text $
                  show educationTag
                  <> categorySep
                  <> show (E.degreeType e)
                  <> categorySep
              ]
            -- Don't split
          , span
              [ style $ textWhitespace whitespaceNoWrap ]
              [ text $ (show $ E.gpa e) <> " gpa" ]
          ]
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
    publicationToWidget p = orr
      [ subsubhead
          [ style do
              subsubheadStyle
              textAlign leftTextAlign
              if isJust $ Pu.url p
                -- Add extra space
                -- between this link
                -- and the one below,
                -- for easier tapping.
                then marginBottom (px 12.0)
                else pure unit
          ]
          [ a [ href $ unwrap $ Pu.documentUrl p ] [ text $ Pu.name p ] ]
      , subtext' case Pu.url p of
          Just (URL url) →
            [ span' [ text $ show publicationTag <> categorySep ]
            , a
                [ href url ]
                [ text $ show (Pu.type_ p) ]
            , span' [ text $ categorySep <> (showTags $ Pu.topics p) ]
            ]
          Nothing →
            [ text $
                show publicationTag
                <> categorySep
                <> show (Pu.type_ p)
                <> categorySep
                <> showTags (Pu.topics p)
            ]
      , paragraph [] [ text (Pu.description p <> ".") ]
      ]
    projectToWidget p = orr
      [ subsubhead
          [ style $ subsubheadStyle *> textAlign leftTextAlign ]
          [ case Pr.url p of
              Just (URL url) → a [ href url ] [ text $ Pr.name p ]
              Nothing → text $ Pr.name p
          ]
      , subtext'
        [ text $
            show projectTag
            <> categorySep
            <> showTags (Pr.types p)
            <> categorySep
            <> showTags (Pr.topics p)
            <> categorySep
            <> showTags (Pr.languages p)
            <> categorySep
            <> show (Pr.scope p)
        ]
      , paragraph [] [ text (Pr.description p <> ".") ]
      ]
    subtext' = subtext
      [ style do
          subtextStyle
          textTransform lowercase
      ]
    categorySep = "; "
    -- Note: We sort tags
    -- because `HashSet` has nondeterministic ordering.
    showTags = joinWith ", " <<< sort <<< map show <<< toArray

educationTag ∷ Tag
educationTag = tag "education"

educationColor ∷ Color
educationColor = brightYellow

publicationTag ∷ Tag
publicationTag = tag "publication"

publicationColor ∷ Color
publicationColor = brightBlue

projectTag ∷ Tag
projectTag = tag "project"

projectColor ∷ Color
projectColor = brightGreen
