module Component.Timeline where

import CSS (absolute, alignItems, background, bold, border, display, em, flex, flexGrow, flexShrink, fontWeight, height, left, marginBottom, padding, paddingBottom, paddingTop, pct, position, px, relative, solid, vw, width, zIndex)
import CSS.Common (center)
import CSS.Render.Concur.React (style)
import CSS.TextAlign as TA
import Color.Scheme.Website as C
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Data.Array (last, reverse, sortWith, tail, zip)
import Data.Date (Date, diff)
import Data.DateTime (DateTime(DateTime))
import Data.Enum (toEnum)
import Data.Formatter.DateTime (FormatterCommand(DayOfMonthTwoDigits,MonthTwoDigits,Placeholder,YearFull), format)
import Data.List ((:), List(Nil))
import Data.Maybe (Maybe(Just,Nothing), fromJust, fromMaybe)
import Data.Time (Time(Time))
import Data.Time.Duration (Days(Days))
import Data.Tuple (Tuple(Tuple), fst)
import Partial.Unsafe (unsafePartial)
import Prelude ((<>), ($), (/), (*), discard, map, negate)
import Math (abs)

timeline ∷ ∀ a. Array (Tuple Date (Widget HTML a)) → Widget HTML a
timeline datedWidgets = div
    [ style $ position relative]
    ([ line ] <>
      (mapPairs
        (\(Tuple (Tuple date widget) (Tuple sndDate _)) →
          timelineItem (marginFromDays $ diff date sndDate) date widget)
        sortedDatedWidgets
      ) <> case last sortedDatedWidgets of
          Just (Tuple date widget) → [ timelineItem (px 0.0) date widget ]
          Nothing → []
    )
  where
    mapPairs f xs = map f (zip xs (fromMaybe [] $ tail xs))
    sortedDatedWidgets = reverse $ sortWith fst datedWidgets

    marginFromDays d = px $ 0.25 * (abs $ fromDays $ d)
    fromDays (Days x) = x

    timelineItem space date widget = div
      [ style $ do
          display flex
          alignItems center
          marginBottom space
      ]
      [ dateWidget date
      , connector
      , div
          [ style $ do
              border solid lineWidth lineColor
              padding (em 1.0) (em 1.0) (em 1.0) (em 1.0)
              flexGrow 1
          ]
          [ widget ]
      ]

    dateWidget date = div
      [ style $ do
          border solid lineWidth lineColor
          paddingTop (em 1.0)
          paddingBottom (em 1.0)
          width (dateWidthUnit dateWidthNum)
          flexShrink 0
          fontWeight bold
          TA.textAlign TA.center
          background (C.background) -- To hide line
      ]
      [ text $ dateFormat $ DateTime date zeroTime ]
    dateWidthUnit = em
    dateWidthNum = 8.0
    dateFormat = format $ YearFull : Placeholder "-" : MonthTwoDigits : Placeholder "-" : DayOfMonthTwoDigits : Nil
    zeroTime = Time (unsafePartial fromJust $ toEnum 0) (unsafePartial fromJust $ toEnum 0) (unsafePartial fromJust $ toEnum 0) (unsafePartial fromJust $ toEnum 0)

    connector = div
      [ style $ do
          background lineColor
          width (vw 4.0)
          flexShrink 0
          height lineWidth
      ]
      []

    line = div
      [ style $ do
          position absolute
          left (dateWidthUnit $ dateWidthNum / 2.0) -- Center of dateWidget
          background lineColor
          width lineWidth
          height (pct 100.0)
          zIndex (-10)
      ]
      []
    lineColor = C.altForeground
    lineWidth = (px 4.0)
