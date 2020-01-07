module Component.Indicator (bouncePauseStylesheet, indicator, indicatorStyle) where

import CSS (CSS, fromString, key, keyframes, marginBottom, marginTop, px)
import CSS.Render.Concur.React (style)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Prelude (($), discard)

-- No support for multiple percents per frame, such as
-- 0%, 10%, 20%, 30% {
--   transform: translateY(0) translateX(-50%);
-- }
-- 5%, 15%, 25% {
--   transform: translateY(-50%) translateX(-50%);
-- }
bouncePauseStylesheet ∷ CSS
bouncePauseStylesheet = keyframes "bounce-pause" $
    0.0  /\ key (fromString "transform") "translateY(0)" :|
  [ 5.0  /\ key (fromString "transform") "translateY(-50%)"
  , 10.0 /\ key (fromString "transform") "translateY(0)"
  , 15.0 /\ key (fromString "transform") "translateY(-50%)"
  , 20.0 /\ key (fromString "transform") "translateY(0)"
  , 25.0 /\ key (fromString "transform") "translateY(-50%)"
  , 30.0 /\ key (fromString "transform") "translateY(0)" ]

indicatorStyle ∷ CSS
indicatorStyle = do
  marginTop $ px 8.0
  marginBottom $ px 8.0

  -- animation is missing support for 'ease' timing function and 'none' fill mode
  -- animation (fromString "bounce-pause") (sec 10.0) ease (sec 0.0) infinite normalAnimationDirection none
  key (fromString "animation") "bounce-pause 10s ease infinite"

  -- Missing support for following. Using escape hatch.
  key (fromString "cursor") "default"
  key (fromString "user-select") "none"
  key (fromString "-webkit-touch-callout") "none"

indicator ∷ ∀ a. Widget HTML a
indicator = div [ style $ indicatorStyle ] [ text "╲╱" ]
