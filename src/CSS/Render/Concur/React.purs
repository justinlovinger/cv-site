-- | Render CSS as a Concur React prop
module CSS.Render.Concur.React (style, styledEl) where

import Prelude

import Concur.React.DOM (El)
import CSS.Property (Key, Value)
import CSS.Render (collect)
import CSS.Stylesheet (CSS, Rule(..), runS)
import Concur.React.Props as P
import Data.Array (mapMaybe, concatMap, singleton)
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)

-- | Render a set of css rules as an inline style prop
-- |
-- | For example:
-- |
-- | ```purescript
-- | div [ style do color red
-- |                display block ]
-- |     [ ... ]
-- | ```
style ∷ ∀ a. CSS → P.ReactProps a
style = P.style <<< unsafeCoerce <<< rules <<< runS where
  rules ∷ Array Rule → Object.Object String
  rules rs = Object.fromFoldable properties where
    properties ∷ Array (Tuple String String)
    properties = mapMaybe property rs >>= collect >>> rights

  property ∷ Rule → Maybe (Tuple (Key Unit) Value)
  property (Property k v) = Just (Tuple k v)
  property _              = Nothing

  rights ∷ ∀ l r. Array (Either l r) → Array r
  rights = concatMap $ foldMap singleton

-- | Embed css in an element
-- |
-- | For example:
-- |
-- | ```purescript
-- | redDiv = styledEl div $ do
-- |   color red
-- |   display block
-- | ```
styledEl ∷ El → CSS → El
styledEl el css props = el $ [ style css ] <> props
