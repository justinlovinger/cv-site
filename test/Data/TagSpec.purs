module Data.TagSpec where

import Prelude

import Data.HashSet (fromArray)
import Data.HashSet as HS
import Data.HashSet.Arbitrary (ArbitraryHashSet)
import Data.Newtype (unwrap)
import Data.Tag (Tag, allIn, has, hasIn, isIn, namespacedTag, tag)
import Data.Tag.Arbitrary (ArbitraryTag)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.QuickCheck (quickCheck)

tagSpec ∷ Spec Unit
tagSpec = describe "Data" $ describe "Tag" do
  describe "Tag" do
    let ntx = namespacedTag "x" "a"
        nty = namespacedTag "y" "a"
    it "should not be equal if namespace differs" $
      ntx `shouldNotEqual` nty
    it "should show the same String if namespace differs" $
      show ntx `shouldEqual` show nty
  describe "allIn" do
    it "is true when all tags from the first set are in the second set" $
      (fromArray [ tag "a", tag "b" ] `allIn` fromArray [ tag "a", tag "b", tag "c" ]) `shouldEqual` true
    it "is false when one tag from the first set is not in the given set" $
      (fromArray [ tag "a", tag "b", tag "z" ] `allIn` fromArray [ tag "a", tag "b", tag "c" ]) `shouldEqual` false
    pending "is not commutative"
  describe "has" do
    it "is the flip of isIn" do
      (fromArray [ tag "foo", tag "bar" ]) `has` tag "foo" `shouldEqual` true
      (fromArray [ tag "bar" ]) `has` tag "foo" `shouldEqual` false
  describe "hasIn" do
    it "is true when at least one tag from the first set is in the second set" $
      (fromArray [ tag "a", tag "b", tag "z" ] `hasIn` fromArray [ tag "a", tag "b", tag "c" ]) `shouldEqual` true
    it "is false when no tag from the first set is in the second set" $
      (fromArray [ tag "w", tag "z" ] `hasIn` fromArray [ tag "a", tag "b", tag "c" ]) `shouldEqual` false
    it "is commutative" $
      quickCheck \(arbA ∷ ArbitraryHashSet ArbitraryTag) (arbB ∷ ArbitraryHashSet ArbitraryTag) →
        let a = HS.map unwrap $ unwrap arbA
            b = HS.map unwrap $ unwrap arbB
        in a `hasIn` b === b `hasIn` a
  describe "isIn" do
    it "is true when the given tag is in the given set" $
      (tag "foo" `isIn` fromArray [ tag "foo", tag "bar" ]) `shouldEqual` true
    it "is false when the given tag is not in the given set" $
      (tag "foo" `isIn` fromArray [ tag "bar" ]) `shouldEqual` false
