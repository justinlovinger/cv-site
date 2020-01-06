module CSS.Render.Concur.ReactSpec where

import Prelude

import CSS (block, display)
import CSS.Render.Concur.React (style)
import Concur.Core.Props as CP
import Concur.React.Props as P
import Global.Unsafe (unsafeStringify)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data ComparableStyle a = ComparableStyle (P.ReactProps a)

instance showComparableStyle ∷ Show (ComparableStyle a) where
  show (ComparableStyle (CP.PrimProp x)) = unsafeStringify x
  show (ComparableStyle (CP.Handler x)) = unsafeStringify x

instance eqComparableStyle ∷ Eq (ComparableStyle a) where
  eq x y = (show x) == (show y)

reactSpec ∷ Spec Unit
reactSpec =
  describe "CSS" $ describe "Render" $ describe "Concur" $ describe "React" do
     describe "style" do
        it "returns the same css it is given" $
          (ComparableStyle $ style $ display block) `shouldEqual` (ComparableStyle $ P.style { "display" : "block" })
