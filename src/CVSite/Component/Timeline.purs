module CVSite.Component.Timeline where

import CVSite.Color.Scheme (colorScheme)
import Component.Timeline as T
import Concur.Core (Widget)
import Concur.React (HTML)

type TimelineItem a = T.TimelineItem a

timeline ∷ ∀ a. Array (TimelineItem a) → Widget HTML a
timeline = T.timeline colorScheme
