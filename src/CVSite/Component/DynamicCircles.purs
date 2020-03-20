module CVSite.Component.DynamicCircles  where

import CVSite.Color.Scheme (colorScheme)
import Component.DynamicCircles as DC
import Concur.Core (Widget)
import Concur.React (HTML)

dynamicCircles ∷ ∀ a. Number → Number → Widget HTML a
dynamicCircles = DC.dynamicCircles colorScheme
