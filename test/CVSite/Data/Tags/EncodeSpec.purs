module CVSite.Data.Tags.EncodeSpec where

import Prelude

import CVSite.Data.Tags.Encode (urlDecode, urlEncode)
import Data.HashSet as HS
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap)

import CVSite.Data.Tags.Arbitrary (ArbitraryTag)
import Data.HashSet.Arbitrary (ArbitraryHashSet)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

encodeSpec ∷ Spec Unit
encodeSpec = describe "Data" $ describe "Tag" $ describe "Encode" do
  describe "urlDecode" do
    it "is the inverse of urlEncode" $
      quickCheck
        \(arbHashSetTag ∷ ArbitraryHashSet ArbitraryTag) →
          let a = HS.map unwrap $ unwrap $ arbHashSetTag
          in urlDecode (urlEncode a) === Just a
