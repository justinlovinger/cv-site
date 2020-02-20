module Data.TagSpec where

import Prelude

import Data.HashSet (fromArray)
import Data.HashSet as HS
import Data.Newtype (unwrap)
import Data.Tag (Tag(Tag), allIn, has, hasIn, isIn)

import Data.HashSet.Arbitrary (ArbitraryHashSet)
import Data.Tag.Arbitrary (ArbitraryTag)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

tagSpec ∷ Spec Unit
tagSpec = describe "Data" $ describe "Tag" do
  describe "allIn" do
    it "is true when all tags from the first set are in the second set" $
      (fromArray [ Tag "a", Tag "b" ] `allIn` fromArray [ Tag "a", Tag "b", Tag "c" ]) `shouldEqual` true
    it "is false when one tag from the first set is not in the given set" $
      (fromArray [ Tag "a", Tag "b", Tag "z" ] `allIn` fromArray [ Tag "a", Tag "b", Tag "c" ]) `shouldEqual` false
    pending "is not commutative"
  describe "has" do
    it "is the flip of isIn" do
      (fromArray [ Tag "foo", Tag "bar" ]) `has` Tag "foo" `shouldEqual` true
      (fromArray [ Tag "bar" ]) `has` Tag "foo" `shouldEqual` false
  describe "hasIn" do
    it "is true when at least one tag from the first set is in the second set" $
      (fromArray [ Tag "a", Tag "b", Tag "z" ] `hasIn` fromArray [ Tag "a", Tag "b", Tag "c" ]) `shouldEqual` true
    it "is false when no tag from the first set is in the second set" $
      (fromArray [ Tag "w", Tag "z" ] `hasIn` fromArray [ Tag "a", Tag "b", Tag "c" ]) `shouldEqual` false
    it "is commutative" $
      quickCheck \(arbA ∷ ArbitraryHashSet ArbitraryTag) (arbB ∷ ArbitraryHashSet ArbitraryTag) →
        let a = HS.map unwrap $ unwrap arbA
            b = HS.map unwrap $ unwrap arbB
        in a `hasIn` b === b `hasIn` a
  describe "isIn" do
    it "is true when the given tag is in the given set" $
      (Tag "foo" `isIn` fromArray [ Tag "foo", Tag "bar" ]) `shouldEqual` true
    it "is false when the given tag is not in the given set" $
      (Tag "foo" `isIn` fromArray [ Tag "bar" ]) `shouldEqual` false
