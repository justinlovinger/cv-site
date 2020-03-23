module CVSite.Data.Tags.Arbitrary where

import Prelude

import CVSite.Data.Tags (Tag)
import Data.Hashable (class Hashable, hash)
import Data.Newtype (class Newtype, unwrap, wrap)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

newtype ArbitraryTag = ArbitraryTag Tag

instance arbArbitraryTag ∷ Arbitrary ArbitraryTag where
  arbitrary = wrap <$> genericArbitrary

derive instance eqArbitraryTag ∷ Eq ArbitraryTag

instance hashableArbitraryTag ∷ Hashable ArbitraryTag where
  hash = hash <<< unwrap

derive instance newtypeArbitraryTag ∷ Newtype ArbitraryTag _
