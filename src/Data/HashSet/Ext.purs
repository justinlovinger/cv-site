module Data.HashSet.Ext where

import Prelude

import Data.HashSet (HashSet, difference, intersection, isEmpty, member)
import Data.Hashable (class Hashable)

allIn ∷ ∀ a. Hashable a ⇒ HashSet a → HashSet a → Boolean
allIn a b = isEmpty $ difference a b

has ∷ ∀ a. Hashable a ⇒ HashSet a → a → Boolean
has = (flip member)

hasIn ∷ ∀ a. Hashable a ⇒ HashSet a → HashSet a → Boolean
hasIn a b = not $ isEmpty $ intersection a b

isIn ∷ ∀ a. Hashable a ⇒ a → HashSet a → Boolean
isIn = member
