module Concur.React.Widgetable where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)

class Widgetable a where
  toWidget ∷ ∀ b. a → Widget HTML b
