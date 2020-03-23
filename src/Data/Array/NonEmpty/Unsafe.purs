module Data.Array.NonEmpty.Unsafe where

import Data.Array.NonEmpty (NonEmptyArray)
import Unsafe.Coerce (unsafeCoerce)

-- From https://github.com/purescript/purescript-arrays/blob/v5.3.1/src/Data/Array/NonEmpty.purs#L140
unsafeFromArray :: ∀ a. Array a → NonEmptyArray a
unsafeFromArray = unsafeCoerce
