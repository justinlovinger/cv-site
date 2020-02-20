module Data.HashSet.Arbitrary where

import Prelude

import Data.Newtype (class Newtype)
import Data.HashSet (HashSet, fromArray)
import Data.Hashable (class Hashable)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype ArbitraryHashSet a = ArbitraryHashSet (HashSet a)

derive instance newtypeArbitraryHashSet ∷ Newtype (ArbitraryHashSet a) _

instance arbArbitraryHashSet ∷ (Arbitrary a, Hashable a) ⇒ Arbitrary (ArbitraryHashSet a) where
  arbitrary = map (ArbitraryHashSet <<< fromArray) arbitrary
