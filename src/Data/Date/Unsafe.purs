module Data.Date.Unsafe (unsafeDate) where

import Prelude

import Data.Date (Date, Month, exactDate)
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

unsafeDate ∷ Int -> Month -> Int -> Date
unsafeDate year month day = unsafePartial fromJust $ exactDate (unsafePartial fromJust $ toEnum year) month (unsafePartial fromJust $ toEnum day)
