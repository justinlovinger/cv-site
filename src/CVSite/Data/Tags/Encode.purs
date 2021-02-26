module CVSite.Data.Tags.Encode (urlDecode, urlEncode) where

import Prelude

import CVSite.Data.Tags (Tag(..))
import Data.Array (sort)
import Data.HashSet (HashSet, fromArray, toArray)
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith, split)
import Data.String.Pattern (Pattern(Pattern))
import Data.Traversable (sequence)

urlDecode ∷ String → Maybe (HashSet Tag)
urlDecode = fromArray <$$> sequence <<< map decodeTag <<< split' (Pattern "") where
  -- `split` returns [""] on an empty string,
  -- instead of [].
  -- That is undesirable when decoding.
  split' p s = if s == "" then [] else split p s

  decodeTag ∷ String → Maybe Tag
  -- Super-categories
  decodeTag "e" = Just Education
  decodeTag "p" = Just Project
  decodeTag "P" = Just Publication
  -- Education
  -- Degree
  decodeTag "b" = Just Bachelors
  decodeTag "m" = Just Masters
  -- Project
  -- Type
  decodeTag "g" = Just Game
  decodeTag "l" = Just Library
  decodeTag "w" = Just PWA
  decodeTag "r" = Just Program
  decodeTag "t" = Just Template
  decodeTag "W" = Just Website
  -- Topic
  decodeTag "M" = Just PrMachineLearning
  decodeTag "n" = Just PrNoTopics
  decodeTag "o" = Just PrOptimization
  decodeTag "x" = Just PrTextSummarization
  -- Language
  decodeTag "s" = Just CSharp
  decodeTag "h" = Just Haskell
  decodeTag "j" = Just JavaScript
  decodeTag "N" = Just Nix
  decodeTag "u" = Just PureScript
  decodeTag "y" = Just Python
  -- Scope
  decodeTag "a" = Just Major
  decodeTag "E" = Just Medium
  decodeTag "i" = Just Minor
  -- Publications
  -- Type
  decodeTag "f" = Just Conference
  decodeTag "J" = Just Journal
  decodeTag "T" = Just Thesis
  -- Topic
  decodeTag "I" = Just PuIncrementalLearning
  decodeTag "A" = Just PuMachineLearning
  decodeTag "O" = Just PuOptimization
  decodeTag "S" = Just PuSearch
  decodeTag "X" = Just PuTextSummarization
  -- Invalid
  decodeTag _ = Nothing

-- NOTE: Without `sort`, order may change greatly with small changes.
-- This doesn't affect functionality, but it looks odd to users.
urlEncode ∷ HashSet Tag → String
urlEncode = joinWith "" <<< map encodeTag <<< sort <<< toArray where
  -- Assign each tag the first character in its name.
  -- If that character is taken,
  -- assign that character capitalized.
  -- If that capitalized character is taken,
  -- move to the second character, and repeat.
  -- NOTE: A few exceptions are made for future proofing.
  -- If all characters in the alphabet are taken,
  -- prepend the character with 0.
  -- If all characters prepended with 0 are taken,
  -- prepend with 1,
  -- etc.
  -- NOTE: Prepended characters are not currently supported by `decodeUrl`.
  encodeTag ∷ Tag → String
  -- Super-categories
  encodeTag Education = "e"
  encodeTag Project = "p"
  encodeTag Publication = "P"
  -- Education
  -- Degree
  encodeTag Bachelors = "b"
  encodeTag Masters = "m"
  -- Project
  -- Type
  encodeTag Game = "g"
  encodeTag Library = "l"
  encodeTag PWA = "w"
  encodeTag Program = "r"
  encodeTag Template = "t"
  encodeTag Website = "W"
  -- Topic
  encodeTag PrMachineLearning = "M"
  encodeTag PrNoTopics = "n"
  encodeTag PrOptimization = "o"
  encodeTag PrTextSummarization = "x"
  -- Language
  {-- encodeTag C = "c" --}
  {-- encodeTag Cpp = "C" --}
  encodeTag CSharp = "s"
  encodeTag Haskell = "h"
  encodeTag JavaScript = "j"
  encodeTag Nix = "N"
  encodeTag PureScript = "u"
  encodeTag Python = "y"
  -- Scope
  encodeTag Major = "a"
  encodeTag Medium = "E"
  encodeTag Minor = "i"
  -- Publications
  -- Type
  encodeTag Conference = "f"
  encodeTag Journal = "J"
  encodeTag Thesis = "T"
  -- Topic
  encodeTag PuIncrementalLearning = "I"
  encodeTag PuMachineLearning = "A"
  encodeTag PuOptimization = "O"
  encodeTag PuSearch = "S"
  encodeTag PuTextSummarization = "X"

infix 2 mapmap as <$$>

mapmap ∷ ∀ f g a b. Functor f ⇒ Functor g ⇒ (a → b) → f (g a) → f (g b)
mapmap = (<$>) <<< (<$>)
