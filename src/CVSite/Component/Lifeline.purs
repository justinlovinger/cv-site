module CVSite.Component.Lifeline (lifeline, lifelineStylesheet) where

import CSS (AnimationName(..), CSS, alternate, animation, backwards, display, displayInherit, displayNone, easeOut, fixed, fromString, height, inlineBlock, inset, insetBoxShadow, iterationCount, key, keyframes, left, margin, marginBottom, maxHeight, maxWidth, paddingLeft, paddingRight, pct, position, px, sec, textWhitespace, top, vh, vw, whitespaceNoWrap, width, zIndex)
import CSS.Common (auto)
import CSS.Render.Concur.React (style)
import CSS.Text.Transform (capitalize, lowercase, textTransform)
import CSS.TextAlign (leftTextAlign, textAlign)
import CVSite.Color.Scheme (brightBlue, brightGreen, brightRed, brightYellow)
import CVSite.Component.Filter (filterSuperblockContainer)
import CVSite.Component.Subtext (subtext, subtextStyle)
import CVSite.Component.Timeline (timeline)
import CVSite.Data.Education as E
import CVSite.Data.Projects as Pr
import CVSite.Data.Publications as Pu
import CVSite.Data.Tags (Tag(..), tags)
import CVSite.Data.Tags.Encode (urlDecode, urlEncode)
import Color (Color)
import Component.Paragraph (paragraph)
import Component.Subhead (subhead, subheadStyle)
import Component.Subsubhead (subsubhead, subsubheadStyle)
import Component.Subsubsubhead (subsubsubhead, subsubsubheadStyle)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, div', li', span, span', text, ul)
import Concur.React.Props (href)
import Control.Alt ((<|>))
import Control.MultiAlternative (orr)
import Data.Array (concat, filter, foldl, mapMaybe, sort, sortBy, uncons, zip)
import Data.Date (Date)
import Data.HashSet (HashSet, delete, fromArray, insert, intersection, toArray, union)
import Data.HashSet.Ext (has, hasIn, isIn)
import Data.HeytingAlgebra ((&&), conj, disj, not)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.String (Pattern(Pattern), contains, joinWith, split, stripPrefix)
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.Tuple (Tuple(Tuple), fst, lookup, snd)
import Data.Tuple.Nested (over1, tuple4, (/\))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Prelude (class Functor, Unit, bind, compare, discard, map, pure, show, unit, ($), (*>), (/), (<$), (<<<), (<>))
import Web.HTML (window)
import Web.HTML.HTMLDocument (title)
import Web.HTML.History (DocumentTitle(DocumentTitle), URL(URL), replaceState)
import Web.HTML.Location (hash, pathname, search)
import Web.HTML.Window (Window, document, history, location)

type TimelineItem a = { borderColor ∷ Color
                      , date ∷ Date
                      , tags ∷ HashSet Tag
                      , widget ∷ Widget HTML a
                      }

lifelineStylesheet ∷ CSS
lifelineStylesheet = keyframes "show" $
    0.0    /\ key (fromString "opacity") (show 0) :|
  [ 100.0  /\ key (fromString "opacity") (show 100)
  ]

lifeline ∷ ∀ a. Widget HTML a
lifeline = do
    -- Try to get from url, before falling back to defaults
    window_ ← liftEffect window
    location_ ← liftEffect $ location window_
    querystring ← liftEffect $ search location_

    let encodedFilters ∷ Maybe String
        encodedFilters = lookup urlFiltersKey queries

        queries ∷ Array (Tuple String String)
        queries = mapMaybe (maybe Nothing (\{head, tail} → Just $ Tuple head (joinWith "=" $ tail)) <<< uncons) $ map (split $ Pattern "=") $ filter (contains $ Pattern "=") $ split (Pattern "&") querystring'

        querystring' ∷ String
        querystring' = fromMaybe querystring $ stripPrefix (Pattern "?") querystring 

    case encodedFilters of
      Just s → case urlDecode s of
        -- Valid filters
        Just af → if hasOnePerCategory af
          then lifeline' af -- Valid filters for each category
          else stripAndDefault window_ location_ -- Missing filters
        -- Invalid filters
        Nothing → stripAndDefault window_ location_
      -- No query string
      Nothing → lifeline' defaultActiveFilters
  where
    stripAndDefault w l = do
      -- Strip query string before continuing
      liftEffect do
        pathname_ ← pathname l
        hash_ ← hash l
        replaceUrl' w (pathname_ <> hash_)

      -- Continue with default
      lifeline' defaultActiveFilters

    defaultActiveFilters =
      (fromArray $ map (fst <<< fst) filtersMaster) -- Super-category tags
      `union`
      (fromArray $ map fst $ filter snd $ concat $ map snd $ concat $ map snd $ filtersMaster) -- Category tags

lifeline' ∷ ∀ a. HashSet Tag → Widget HTML a
lifeline' activeFilters = do
    cb ← lifeline_ false

    -- Update `activeFilters`
    let activeFilters' = if cb `isIn` activeFilters
      then delete cb activeFilters
      else insert cb activeFilters

    if hasOnePerCategory activeFilters' then do
      -- Update url
      liftEffect $ replaceUrl ("?" <> urlFiltersKey <> "=" <> urlEncode activeFilters')

      -- Recurse with given checkbox flipped
      lifeline' activeFilters'
    else do
      -- Indicate error
      _ ← (unit <$ lifeline_ true) <|> liftAff (delay $ fromDuration $ Seconds $ errSeconds)

      -- Do not change filters
      lifeline' activeFilters
  where
    lifeline_ showErr = div'
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
              maxWidth (px 1200.0)
              margin auto auto auto auto
          ]
          [ filterSuperblockContainer $
              map
                (\(Tuple (Tuple t c) cs) → tuple4
                  t
                  c
                  -- Zip filter tags with active boolean
                  (map (over2 (\ts → zip ts $ map (has activeFilters) ts)) cs)
                  (activeFilters `has` t)
                )
                filtersUI
          ]
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
      , div
          [ style do
              animation
                (AnimationName $ fromString "show")
                (sec $ errSeconds / 2.0)
                easeOut
                (sec 0.0)
                (iterationCount 2.0)
                alternate
                backwards
              display if showErr then displayInherit else displayNone
              insetBoxShadow inset (vw 0.0) (vh 0.0) (fromString "calc(10vw + 10vh)") brightRed

              -- Surround view
              position fixed
              top (px 0.0)
              left (px 0.0)
              width (vw 100.0)
              maxWidth (pct 100.0) -- Fix overflow
              height (vh 100.0)
              maxHeight (pct 100.0) -- Fix overflow

              -- Display in front of content
              zIndex (10)
              key (fromString "pointer-events") "none"
          ]
          []
      ]

    errSeconds = 0.5

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

urlFiltersKey ∷ String
urlFiltersKey = "f"

hasOnePerCategory ∷ HashSet Tag → Boolean
hasOnePerCategory ts = foldl conj true $ map
  (\(Tuple _ ctss) → foldl conj true $ map (hasIn ts) ctss)
  filtersByCategory

filtersByCategory ∷ Array (Tuple Tag (Array (HashSet Tag)))
filtersByCategory = map (over1 fst) $ map (over2 (map (fromArray <<< map fst <<< snd))) filtersMaster

over2 ∷ ∀ a b f. Functor f ⇒ (a → b) → f a → f b
over2 = map -- `map` over `Tuple` is like `over2`

replaceUrl ∷ String → Effect Unit
replaceUrl u = do
  window_ ← liftEffect window
  replaceUrl' window_ u

replaceUrl' ∷ Window → String → Effect Unit
replaceUrl' w u = do
  document_ ← liftEffect $ document w
  documentTitle ← liftEffect $ title document_
  history_ ← liftEffect $ history w
  replaceState (unsafeToForeign "") (DocumentTitle documentTitle) (URL $ u) history_

-- | The filters master data structure.
-- | Deconstruct it
-- | for the filter UI,
-- | active filters,
-- | and more.
filtersMaster ∷ Array (Tuple (Tuple Tag Color) (Array (Tuple String (Array (Tuple Tag Boolean)))))
filtersMaster =
  [ Tuple (Tuple Education educationColor)
      [ Tuple "degree"
          [ Tuple Masters true
          , Tuple Bachelors false
          ]
      ]
  , Tuple (Tuple Publication publicationColor)
      [ Tuple "type"
          [ Tuple Thesis true
          , Tuple Journal true
          , Tuple Conference true
          ]
      , Tuple "topic"
          [ Tuple PuMachineLearning true
          , Tuple PuOptimization true
          , Tuple PuIncrementalLearning true
          , Tuple PuSearch true
          , Tuple PuTextSummarization true
          ]
      ]
  , Tuple (Tuple Project projectColor)
      [ Tuple "type"
          [ Tuple Website true
          , Tuple PWA true
          , Tuple Library true
          , Tuple Program true
          , Tuple Template true
          , Tuple Game false
          ]
      , Tuple "topic"
          [ Tuple PrMachineLearning true
          , Tuple PrOptimization true
          , Tuple PrTextSummarization true
          , Tuple PrNoTopics true
          ]
      , Tuple "language"
          [ Tuple Rust true
          , Tuple Haskell true
          , Tuple PureScript true
          , Tuple Nix true
          , Tuple Python true
          , Tuple JavaScript true
          , Tuple CSharp true
          ]
      , Tuple "scope"
          [ Tuple Major true
          , Tuple Medium true
          , Tuple Minor false
          ]
      ]
  ]

timelineItems ∷ ∀ a. Array (TimelineItem a)
timelineItems = sortBy (\a b → compare b.date a.date) $ -- Sorted by descending date
    (map
      (\e → { borderColor : educationColor
            , date : E.graduated e
            , tags : tags e
            , widget : educationToWidget e
            }
      )
      E.education
    )
    <> (map
      (\p → { borderColor : publicationColor
            , date : Pu.published p
            , tags : tags p
            , widget : publicationToWidget p
            }
      )
      Pu.publications
    )
    <> (map
      (\p → { borderColor : projectColor
            , date : Pr.updated p
            , tags : tags p
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
                  show Education
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
              if isJust $ Pu.publicationUrl p
                -- Add extra space
                -- between this link
                -- and the one below,
                -- for easier tapping.
                then marginBottom (px 12.0)
                else pure unit
          ]
          [ a [ href $ unwrap $ Pu.documentUrl p ] [ text $ Pu.name p ] ]
      , subtext' case Pu.publicationUrl p of
          Just (URL url) →
            [ span' [ text $ show Publication <> categorySep ]
            , a
                [ href url ]
                [ text $ show (Pu.type_ p) ]
            , span' [ text $ categorySep <> (showTags $ Pu.topics p) ]
            ]
          Nothing →
            [ text $
                show Publication
                <> categorySep
                <> show (Pu.type_ p)
                <> categorySep
                <> showTags (Pu.topics p)
            ]
      , paragraph [] $
          [ text (Pu.description p <> ".") ]
          <> case Pu.codeUrl p of
            Just (URL url) →
              [ a [ href url ] [ text " Source code is available" ]
              , text "."
              ]
            Nothing → []
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
            show Project
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
    showTags ∷ HashSet Tag → String
    showTags = joinWith ", " <<< sort <<< map show <<< toArray

educationColor ∷ Color
educationColor = brightYellow

publicationColor ∷ Color
publicationColor = brightBlue

projectColor ∷ Color
projectColor = brightGreen
