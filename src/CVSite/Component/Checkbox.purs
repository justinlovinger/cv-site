module CVSite.Component.Checkbox where

import CSS (CSS)
import CVSite.Color.Scheme (colorScheme)
import Component.Checkbox as C
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.Props (ReactProps)

checkbox' ∷ ∀ a. Boolean → Boolean → Array (ReactProps a) → Widget HTML a
checkbox' = C.checkbox' colorScheme

checkbox ∷ ∀ a. CSS → CSS → CSS → CSS → Boolean → Boolean → Array (ReactProps a) → Widget HTML a
checkbox = C.checkbox
  
checkboxStyle ∷ CSS
checkboxStyle = C.checkboxStyle colorScheme

checkedCheckboxStyle ∷ CSS
checkedCheckboxStyle = C.checkedCheckboxStyle colorScheme

disabledCheckboxStyle ∷ CSS
disabledCheckboxStyle = C.disabledCheckboxStyle colorScheme

checkedDisabledCheckboxStyle ∷ CSS
checkedDisabledCheckboxStyle = C.checkedDisabledCheckboxStyle colorScheme
