module Data.Tag.EncodeSpec where

import Prelude

import Data.HashSet as HS
import Data.Maybe (Maybe(Just), isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tag.Encode (urlDecode, urlEncode)

import Data.HashSet.Arbitrary (ArbitraryHashSet)
import Data.Tag.Arbitrary (ArbitraryTag)
import Test.QuickCheck ((===), Result(Failed))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

newtype ArbitraryEncodableHashsetTag = ArbitraryEncodableHashsetTag (ArbitraryHashSet ArbitraryTag)

derive instance newtypeArbitraryEncodableHashsetTag ∷ Newtype ArbitraryEncodableHashsetTag _

instance arbArbitraryEncodableHashsetTag ∷ Arbitrary ArbitraryEncodableHashsetTag where
  arbitrary = do
      a ← arbitrary
      if encodable a
        then pure $ ArbitraryEncodableHashsetTag a
        else arbitrary
    where
      encodable ∷ ArbitraryHashSet ArbitraryTag → Boolean
      encodable = isJust <<< urlEncode <<< HS.map unwrap <<< unwrap

encodeSpec ∷ Spec Unit
encodeSpec = describe "Data" $ describe "Tag" $ describe "Encode" do
  describe "urlDecode" do
    it "is the inverse of urlEncode" $
      quickCheck
        \(wrappedArb ∷ ArbitraryEncodableHashsetTag) →
          let a = HS.map unwrap $ unwrap $ unwrap $ wrappedArb
          in maybe
            (Failed "urlEncode returned Nothing. This should not happen")
            (\encodedString → urlDecode encodedString === Just a)
            (urlEncode a)
