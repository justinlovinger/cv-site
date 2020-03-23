module Data.HashSet.ExtSpec where

import Prelude

import Data.HashSet (fromArray)
import Data.HashSet.Arbitrary (ArbitraryHashSet)
import Data.Newtype (unwrap)
import Data.HashSet.Ext (allIn, has, hasIn, isIn)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

extSpec ∷ Spec Unit
extSpec = describe "Data" $ describe "HashSet" $ describe "Ext" do
  describe "allIn" do
    it "is true when all items from the first set are in the second set" $
      (fromArray [ "a", "b" ] `allIn` fromArray [ "a", "b", "c" ]) `shouldEqual` true
    it "is false when one item from the first set is not in the given set" $
      (fromArray [ "a", "b", "z" ] `allIn` fromArray [ "a", "b", "c" ]) `shouldEqual` false
    pending "is not commutative"
  describe "has" do
    it "is the flip of isIn" do
      (fromArray [ "foo", "bar" ]) `has` "foo" `shouldEqual` true
      (fromArray [ "bar" ]) `has` "foo" `shouldEqual` false
  describe "hasIn" do
    it "is true when at least one from the first set is in the second set" $
      (fromArray [ "a", "b", "z" ] `hasIn` fromArray [ "a", "b", "c" ]) `shouldEqual` true
    it "is false when no item from the first set is in the second set" $
      (fromArray [ "w", "z" ] `hasIn` fromArray [ "a", "b", "c" ]) `shouldEqual` false
    it "is commutative" $
      quickCheck \(arbA ∷ ArbitraryHashSet String) (arbB ∷ ArbitraryHashSet String) →
        let a = unwrap arbA
            b = unwrap arbB
        in a `hasIn` b === b `hasIn` a
  describe "isIn" do
    it "is true when the given item is in the given set" $
      ("foo" `isIn` fromArray [ "foo", "bar" ]) `shouldEqual` true
    it "is false when the given item is not in the given set" $
      ("foo" `isIn` fromArray [ "bar" ]) `shouldEqual` false
