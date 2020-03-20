module Component.Timeline where

import CSS (absolute, alignItems, background, bold, border, display, displayNone, em, flex, flexGrow, flexShrink, fontWeight, height, left, padding, paddingBottom, paddingTop, pct, position, px, relative, solid, vw, width, zIndex)
import CSS.Common (center)
import CSS.Render.Concur.React (style)
import CSS.TextAlign as TA
import Color (Color)
import Color.Scheme.SixteenAnsi (ColorScheme)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Data.Array (concatMap, last, sortBy, tail, zip)
import Data.Date (Date, diff)
import Data.DateTime (DateTime(DateTime))
import Data.Enum (toEnum)
import Data.Formatter.DateTime (FormatterCommand(DayOfMonthTwoDigits, MonthTwoDigits, Placeholder, YearFull), format)
import Data.List ((:), List(Nil))
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe)
import Data.Time (Time(Time))
import Data.Time.Duration (Days(Days))
import Data.Tuple (Tuple(Tuple))
import Math (abs)
import Partial.Unsafe (unsafePartial)
import Prelude (compare, discard, negate, ($), (*), (/), (<>))

type TimelineItem a =
  { borderColor ∷ Maybe Color
  , date ∷ Date
  , hidden ∷ Boolean
  , widget ∷ Widget HTML a
  }

timeline ∷ ∀ a. ColorScheme → Array (TimelineItem a) → Widget HTML a
timeline c items = div
    [ style $ position relative]
    ([ line ] <>
      (concatMap
        (\(Tuple { borderColor, date, hidden, widget } { borderColor : _, date : sndDate, hidden : _, widget : _ }) →
          [ timelineWidget (fromMaybeColor borderColor) date hidden widget
          , div [ style $ height (marginFromDays $ diff date sndDate) ] []
          ]
        )
        (pairs sortedItems)
      ) <> case last sortedItems of
          Just { borderColor, date, hidden, widget } →
            [ timelineWidget (fromMaybeColor borderColor) date hidden widget ]
          Nothing → []
    )
  where
    pairs xs = zip xs (fromMaybe [] $ tail xs)
    sortedItems = sortBy (\a b → compare b.date a.date) items -- Descending by date

    marginFromDays d = px $ 0.25 * (abs $ fromDays $ d)
    fromDays (Days x) = x

    fromMaybeColor = fromMaybe lineColor

    timelineWidget borderColor date hidden widget = div
      [ style $ do
          display if hidden then displayNone else flex
          alignItems center
      ]
      [ dateWidget borderColor date
      , connector borderColor
      , div
          [ style $ do
              border solid lineWidth borderColor
              padding (em 1.0) (em 1.0) (em 1.0) (em 1.0)
              flexGrow 1
          ]
          [ widget ]
      ]

    dateWidget borderColor date = div
      [ style $ do
          border solid lineWidth borderColor
          paddingTop (em 1.0)
          paddingBottom (em 1.0)
          width (dateWidthUnit dateWidthNum)
          flexShrink 0
          fontWeight bold
          TA.textAlign TA.center
          background (c.background) -- To hide line
      ]
      [ text $ dateFormat $ DateTime date zeroTime ]
    dateWidthUnit = em
    dateWidthNum = 8.0
    dateFormat = format $ YearFull : Placeholder "-" : MonthTwoDigits : Placeholder "-" : DayOfMonthTwoDigits : Nil
    zeroTime = Time (unsafePartial fromJust $ toEnum 0) (unsafePartial fromJust $ toEnum 0) (unsafePartial fromJust $ toEnum 0) (unsafePartial fromJust $ toEnum 0)

    connector color = div
      [ style $ do
          background color
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
    lineColor = c.altForeground
    lineWidth = (px 4.0)
