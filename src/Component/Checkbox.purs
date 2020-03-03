module Component.Checkbox
  ( checkbox'
  , checkbox
  , checkboxStyle
  , checkedCheckboxStyle
  , disabledCheckboxStyle
  , checkedDisabledCheckboxStyle
  ) where

import Prelude

import CSS (BackgroundImage, CSS, absolute, backgroundColor, backgroundImage, border, borderBox, borderColor, boxSizing, display, ex, fromString, height, inlineBlock, key, position, px, solid, url, width)
import CSS.Overflow (hidden, overflow)
import CSS.Render.Concur.React (style)
import CSS.VerticalAlign (textBottom, verticalAlign)
import Color (Color, toHexString)
import Color.Scheme.Website (altBackground, altForeground, foreground)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (input, span)
import Concur.React.Props (ReactProps, _type, checked, disabled)
import Control.MultiAlternative (orr)
import Data.Either (fromRight)
import Data.String.Base64 (btoa)
import Partial.Unsafe (unsafePartial)

checkbox' ∷ ∀ a. Boolean → Boolean → Array (ReactProps a) → Widget HTML a
checkbox' = checkbox checkboxStyle checkedCheckboxStyle disabledCheckboxStyle checkedDisabledCheckboxStyle

checkbox ∷ ∀ a. CSS → CSS → CSS → CSS → Boolean → Boolean → Array (ReactProps a) → Widget HTML a
checkbox sty cSty dSty cdSty d c props = orr
    [ input ([ _type "checkbox", style hiddenCheckboxStyle, disabled d, checked c ] <> props)
    , span ([ style $ sty *> conditionalStyle ] <> props) []
    ]
  where
    hiddenCheckboxStyle = do
      key (fromString "border") "0"
      key (fromString "clip") "rect(0 0 0 0)"
      height (px 1.0)
      key (fromString "margin") "-1.0"
      overflow hidden
      key (fromString "padding") "0"
      position absolute
      width (px 1.0)
    conditionalStyle =
      if c && d then cdSty else
        if c then cSty else
          if d then dSty else
            pure unit
  
checkboxStyle ∷ CSS
checkboxStyle = do
  boxSizing borderBox
  display inlineBlock
  width (ex 2.0)
  height (ex 2.0)
  border solid (px 2.0) foreground
  verticalAlign textBottom
  key (fromString "cursor") "pointer"

checkedCheckboxStyle ∷ CSS
checkedCheckboxStyle = do
  backgroundImage $ checkmarkBgSvg foreground

disabledCheckboxStyle ∷ CSS
disabledCheckboxStyle = do
  backgroundColor altBackground
  borderColor altForeground
  key (fromString "cursor") "default"

checkedDisabledCheckboxStyle ∷ CSS
checkedDisabledCheckboxStyle = checkedCheckboxStyle *> disabledCheckboxStyle *> do
  backgroundImage $ checkmarkBgSvg altForeground

checkmarkBgSvg ∷ Color → BackgroundImage
checkmarkBgSvg c = url $ "data:image/svg+xml;base64," <> checkmarkB64Svg where
  checkmarkB64Svg = unsafePartial fromRight $ btoa checkmarkSvg
  -- Checkmark, originally:
  -- <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24"><path d="M20.285 2l-11.285 11.567-5.286-5.011-3.714 3.716 9 8.728 15-15.285z"/></svg>
  -- is from
  -- https://iconmonstr.com/check-mark-1-svg/
  checkmarkSvg =  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100%\" height=\"100%\" viewBox=\"0 0 24 24\"><path fill=\"" <> toHexString c <> "\" d=\"M20.285 2l-11.285 11.567-5.286-5.011-3.714 3.716 9 8.728 15-15.285z\"/></svg>"
