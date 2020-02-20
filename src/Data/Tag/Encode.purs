module Data.Tag.Encode (urlDecode, urlEncode) where

import Prelude

import Data.HashSet (HashSet, fromArray, toArray)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)
import Data.String.Common (joinWith, replaceAll, split)
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Data.Tag (Tag)
import Data.Traversable (sequence)
import Global (decodeURIComponent, encodeURIComponent)

urlDecode ∷ String → Maybe (HashSet Tag)
urlDecode = fromArray <$$> sequence <<< map (wrap <$$> decodeURIComponent <<< decodeDash) <<< split' (Pattern "-") where
  -- `split` returns [""] on an empty string,
  -- instead of [].
  -- That is undesirable when decoding.
  split' p s = if s == "" then [] else split p s

urlEncode ∷ HashSet Tag → Maybe String
-- We cannot distinguish between
-- a set of one ""
-- and an empty set.
urlEncode hs = if arr == [ wrap "" ] then Nothing else encodeArray arr where
  arr = toArray hs
  encodeArray = joinWith "-" <$$> sequence <<< map (encodeDash <$$> encodeURIComponent <<< unwrap)

decodeDash ∷ String → String
decodeDash = replaceAll (Pattern "%2D") (Replacement "-")

encodeDash ∷ String → String
encodeDash = replaceAll (Pattern "-") (Replacement "%2D")

infix 2 mapmap as <$$>

mapmap ∷ ∀ f g a b. Functor f ⇒ Functor g ⇒ (a → b) → f (g a) → f (g b)
mapmap = (<$>) <<< (<$>)
