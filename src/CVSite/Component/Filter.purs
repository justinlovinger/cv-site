module CVSite.Component.Filter
  ( filterSuperblockContainer
  , filterSuperblock
  , filterBlock
  , filter
  ) where

import CSS (Abs, Size, color, display, em, flex, flexBasis, flexGrow, flexWrap, fromString, inlineBlock, margin, marginBottom, marginLeft, marginTop, padding, paddingLeft, px, textWhitespace, whitespaceNoWrap, wrap)
import CSS.Common (none)
import CSS.ListStyle.Type (listStyleType)
import CSS.Render.Concur.React (style)
import CSS.Text.Transform (capitalize, textTransform)
import CSS.TextAlign (center, leftTextAlign, textAlign)
import CVSite.Component.Checkbox (checkbox')
import CVSite.Data.Tags (Tag)
import Color (Color)
import Component.Subsubhead (subsubhead, subsubheadStyle)
import Component.Subsubsubhead (subsubsubhead, subsubsubheadStyle)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, label', li, span, text, ul)
import Concur.React.Props (onChange)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple4, uncurry4)
import Prelude (discard, map, negate, not, show, ($), (/), (<$), (<>))

type FilterBlocks = Array (Tuple String Filters)
type Filters = Array (Tuple Tag Boolean)

filterSuperblockContainer ∷ Array (Tuple4 Tag Color FilterBlocks Boolean) → Widget HTML Tag
filterSuperblockContainer fsbs = div
  [ style do
      display flex
      flexWrap wrap
      padding filterSuperblockVSpace filterSuperblockHSpace filterSuperblockVSpace filterSuperblockHSpace
  ]
  (map (uncurry4 filterSuperblock) fsbs)

filterSuperblock ∷ Tag → Color → FilterBlocks → Boolean → Widget HTML Tag
filterSuperblock t c filters isChecked = div
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
      [ filter t true isChecked ]
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

filterBlock ∷ String → Filters → Boolean → Widget HTML Tag
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
          (\(Tuple tag isChecked) → li
            [ style $ marginBottom (em liMarginEm) ]
            [ filter tag isEnabled isChecked ]
          )
          filters
        )
    ]
  where
    liMarginEm = 0.5

filterBlockSpace ∷ Size Abs
filterBlockSpace = px (10.0 / 2.0) -- Applied twice. Double in practice.

filter ∷ Tag → Boolean → Boolean → Widget HTML Tag
filter tag isEnabled isChecked = label'
  [ checkbox' (not isEnabled) isChecked [ tag <$ onChange ]
  , span
      [ style $ marginLeft (fromString checkboxLabelSpace) ]
      [ text $ (show tag) ]
  ]

checkboxLabelSpace ∷ String
checkboxLabelSpace = "0.5ch"
