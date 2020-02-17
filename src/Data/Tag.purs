module Data.Tag where

import Prelude

import Data.HashSet (HashSet, difference, intersection, isEmpty, member)
import Data.Hashable (class Hashable, hash)

class Tagged a where
  tags ∷ a → HashSet Tag

class TagLike a where
  toTag ∷ a → Tag

newtype Tag = Tag String
instance hashableTag ∷ Hashable Tag where hash (Tag t) = hash t
instance eqTag ∷ Eq Tag where eq (Tag a) (Tag b) = eq a b
instance showTag ∷ Show Tag where show (Tag t) = t

allIn ∷ HashSet Tag → HashSet Tag → Boolean
allIn a b = isEmpty $ difference a b

has ∷ HashSet Tag → Tag → Boolean
has = (flip member)

hasIn ∷ HashSet Tag → HashSet Tag → Boolean
hasIn a b = not $ isEmpty $ intersection a b

isIn ∷ Tag → HashSet Tag → Boolean
isIn = member
