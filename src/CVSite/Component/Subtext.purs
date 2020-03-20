module CVSite.Component.Subtext where

import CSS (CSS)
import CVSite.Color.Scheme (colorScheme)
import Component.Subtext as ST
import Concur.React.DOM (El)

subtextStyle ∷ CSS
subtextStyle = ST.subtextStyle colorScheme

subtext ∷ El
subtext = ST.subtext colorScheme
