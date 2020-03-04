module Data.Tag.Arbitrary where

import Prelude

import Data.Hashable (class Hashable, hash)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tag (Tag)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype ArbitraryTag = ArbitraryTag Tag

derive instance eqArbitraryTag ∷ Eq ArbitraryTag

derive instance newtypeArbitraryTag ∷ Newtype ArbitraryTag _

instance arbArbitraryTag ∷ Arbitrary ArbitraryTag where
  arbitrary = map (ArbitraryTag <<< wrap) arbitrary

instance hashableArbitraryTag ∷ Hashable ArbitraryTag where
  hash = hash <<< unwrap
