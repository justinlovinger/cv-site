module Data.Tag.Encode (urlDecode, urlEncode) where

import Prelude

import Data.Array (uncons)
import Data.HashSet (HashSet, fromArray, toArray)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String.Common (joinWith, replaceAll, split)
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Data.Tag (Tag, namespacedTag, tag)
import Data.Traversable (sequence)
import Global (decodeURIComponent, encodeURIComponent)

urlDecode ∷ String → Maybe (HashSet Tag)
urlDecode = fromArray <$$> sequence <<< map (decodeTag <<< decodeDash) <<< split' (Pattern "-") where
  -- `split` returns [""] on an empty string,
  -- instead of [].
  -- That is undesirable when decoding.
  split' p s = if s == "" then [] else split p s
  decodeDash = replaceAll (Pattern dashPercent) (Replacement "-")
  decodeTag =
    (maybe
      Nothing
      (\{ head, tail} → if tail == []
                            then maybe Nothing (Just <<< tag) $ decodePart head
                            else mnamespacedTag (decodePart head) (decodePart $ joinWith "" tail))
    ) <<< uncons <<< split (Pattern "_")
  decodePart = decodeURIComponent <<< decodeUnderscore
  decodeUnderscore = replaceAll (Pattern underscorePercent) (Replacement "_")
  mnamespacedTag (Just b) (Just a) = Just $ namespacedTag b a
  mnamespacedTag _ _ = Nothing

urlEncode ∷ HashSet Tag → Maybe String
-- We cannot distinguish between
-- a set of one "" `Tag`
-- and an empty set.
urlEncode hs = if arr == [ tag "" ] then Nothing else encodeArray arr where
  arr = toArray hs
  encodeArray = joinWith "-" <$$> sequence <<< map (encodeDash <$$> encodeTag)
  encodeTag = encodeTagRecord <<< unwrap
  encodeTagRecord { namespace : Nothing, name } = encodePart name
  encodeTagRecord { namespace : Just namespace, name } = (encodePart namespace) `mappend` (Just "_") `mappend` (encodePart name)
  encodePart = encodeUnderscore <$$> encodeURIComponent
  encodeUnderscore = replaceAll (Pattern "_") (Replacement underscorePercent)
  mappend (Just a) (Just b) = Just (a <> b)
  mappend _ _ = Nothing
  encodeDash = replaceAll (Pattern "-") (Replacement dashPercent)

underscorePercent ∷ String
underscorePercent = "%5F"

dashPercent ∷ String
dashPercent = "%2D"

infix 2 mapmap as <$$>

mapmap ∷ ∀ f g a b. Functor f ⇒ Functor g ⇒ (a → b) → f (g a) → f (g b)
mapmap = (<$>) <<< (<$>)
