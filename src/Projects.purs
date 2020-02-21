module Projects (Language(..), Project, Scope(..), Type_(..), projects, published, updated) where

import Prelude

import CSS (display, em, inlineBlock, marginRight)
import CSS.Render.Concur.React (style)
import CSS.Text.Transform (textTransform, lowercase)
import CSS.TextAlign (textAlign, leftTextAlign)
import Component.Paragraph (paragraph)
import Component.Subsubhead (subsubhead, subsubheadStyle)
import Component.Subtext (subtext, subtextStyle)
import Concur.React.DOM (a, text)
import Concur.React.Props (href)
import Concur.React.Widgetable (class Widgetable)
import Control.MultiAlternative (orr)
import Data.Array ((:), sort)
import Data.Date (Date, Month(January,February,March,April,June,July,September,October,November,December))
import Data.Date.Unsafe (unsafeDate)
import Data.HashSet (HashSet, empty, fromArray, singleton, toArray, union)
import Data.HashSet as HS
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(Just,Nothing))
import Data.String (joinWith)
import Data.Tag (class Tagged, class TagLike, Tag(Tag), toTag)
import Generated.Files (files)
import Web.HTML.History (URL(URL))

newtype Project = Project
  { name ∷ String
  , tags ∷ { firstType ∷ Type_, otherTypes ∷ HashSet Type_, firstLanguage ∷ Language, otherLanguages ∷ HashSet Language, scope ∷ Scope }
  , published ∷ Date
  , updated ∷ Date -- Last major update
  , description ∷ String -- No punctuation
  , longDescription ∷ Maybe String
  , teamRole ∷ Maybe String
  , url ∷ Maybe URL
  }
instance taggedProject ∷ Tagged Project where
  tags (Project p) =
    fromArray [ toTag p.tags.firstType, toTag p.tags.firstLanguage, toTag p.tags.scope ]
    `union` HS.map toTag p.tags.otherTypes
    `union` HS.map toTag p.tags.otherLanguages
instance widgetableProject ∷ Widgetable Project where
  toWidget (Project p ) = orr
      [ subsubhead
          [ style $ subsubheadStyle *> textAlign leftTextAlign ]
          [ case p.url of
              Just (URL url) → a [ href url ] [ text p.name ]
              Nothing → text p.name
          ]
      -- Note: We sort tags
      -- because `HashSet` has nondeterministic ordering.
      , subtext' [ text $ joinWith ", " $ sort $ map show $ p.tags.firstType : (toArray p.tags.otherTypes) ]
      , subtext' [ text $ joinWith ", " $ sort $ map show $ p.tags.firstLanguage : (toArray p.tags.otherLanguages) ]
      , subtext' [ text $ show p.tags.scope ]
      , paragraph [] [ text (p.description <> ".") ]
      ]
    where
      subtext' = subtext
        [ style do
            subtextStyle
            display inlineBlock
            marginRight (em 1.0)
            textTransform lowercase
        ]

data Type_ = Website | PWA | Library | Template | Game
instance tagLikeType_ ∷ TagLike Type_ where toTag = Tag <<< show
instance hashableType_ ∷ Hashable Type_ where hash = hashShow
derive instance eqType_ ∷ Eq Type_
instance showType_ ∷ Show Type_ where
  show Website = "website"
  show PWA = "PWA"
  show Library = "library"
  show Template = "template"
  show Game = "game"

data Language = Nix | PureScript | Python | Javascript | CSharp
instance tagLikeLanguage ∷ TagLike Language where toTag = Tag <<< show
instance hashableLanguage ∷ Hashable Language where hash = hashShow
derive instance eqLanguage ∷ Eq Language
instance showLanguage ∷ Show Language where
  show Nix = "Nix"
  show PureScript = "PureScript"
  show Python = "Python"
  show Javascript = "JavaScript"
  show CSharp = "C#"

data Scope = Major | Medium | Minor
instance tagLikeScope ∷ TagLike Scope where toTag = Tag <<< show
instance hashScope ∷ Hashable Scope where hash = hashShow
derive instance eqScope ∷ Eq Scope
instance showScope ∷ Show Scope where
  show Major = "major"
  show Medium = "medium"
  show Minor = "minor"

-- Note: Manually assigning a number
-- to each data entry
-- would be more efficient,
-- but harder to maintain.
hashShow ∷ ∀ a. Show a ⇒ a → Int
hashShow = hash <<< show

-- Note: dates are constructed
-- with an unsafe `unsafeDate`
-- because these dates are manually written
-- and will not change.
projects ∷ Array Project 
projects =
  [ Project
      { name : "Labyrinth RL"
      , tags : { firstType : Game, otherTypes : empty, firstLanguage : Python, otherLanguages : empty, scope : Medium }
      , published : unsafeDate 2014 October 20 -- Date of last commit
      , updated : unsafeDate 2014 October 20 -- Never updated
      , description : "Escape a labyrinth with traditional roguelike mechanics and a unique victory condition"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL files.projects."labyrinth-rl".url
      }
  , Project
      { name : "Gladiator Manager"
      , tags : { firstType : Game, otherTypes : empty, firstLanguage : CSharp, otherLanguages : empty, scope : Medium }
      , published : unsafeDate 2014 December 10 -- Date of last modified file
      , updated : unsafeDate 2014 December 10 -- Never updated
      , description : "Manage gladiators in physics driven combat"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL files.projects."gladiator-manager".url
      }
  , Project
      { name : "Gist"
      , tags : { firstType : Website, otherTypes : empty, firstLanguage : Python, otherLanguages : singleton Javascript, scope : Major }
      , published : unsafeDate 2016 February 12 -- Date of first App Engine version
      , updated : unsafeDate 2017 March 27 -- Date of second to last commit. Last notes that change was made earlier, but not committed.
      , description : "Automatically summarize text"
      , longDescription : Nothing
      , teamRole : Just "Team Lead and AI"
      , url : Just $ URL "https://gist.justinlovinger.com/"
      }
  , Project
      { name : "Clever Surveys"
      , tags : { firstType : Website, otherTypes : empty, firstLanguage : Python, otherLanguages : singleton Javascript, scope : Major }
      , published : unsafeDate 2016 July 26 -- Date of v1.0.0 in Clever Surveys repo
      , updated : unsafeDate 2017 March 7 -- Date of last non-hotfix patch (v1.1.0)
      , description : "Make surveys with predictive machine learning trained by survey responses"
      , longDescription : Just "Website with predictive machine learning trained by survey responses. Users can answer surveys and receive predictions. Users can create surveys with built-in survey builder."
      , teamRole : Nothing
      , url : Just $ URL "https://cleversurveys.com/"
      }
  , Project
      { name : "Optimal"
      , tags : { firstType : Library, otherTypes : empty, firstLanguage : Python, otherLanguages : empty, scope : Major }
      , published : unsafeDate 2016 November 1 -- Date of v0.1.0
      , updated : unsafeDate 2017 September 4 -- Date of v0.2.0
      , description : "Perform metaheuristic optimization with Genetic Algorithms, Gravitational Search, Cross Entropy, and PBIL"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/optimal"
      }
  , Project
      { name : "Learning"
      , tags : { firstType : Library, otherTypes : empty, firstLanguage : Python, otherLanguages : empty, scope : Major }
      , published : unsafeDate 2017 September 14 -- Date readme was added. Approximate date of public GitHub.
      , updated : unsafeDate 2018 April 20 -- Date of last commit, before recent minor commits
      , description : "Perform machine learning with several models and powerful numerical optimization methods"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/learning"
      }
  , Project
      { name : "Reader"
      , tags : { firstType : Website, otherTypes : singleton PWA, firstLanguage : Python, otherLanguages : singleton Javascript, scope : Medium }
      , published : unsafeDate 2018 June 5 -- Date of first App Engine version
      , updated : unsafeDate 2018 June 9 -- Date of last tagged version (v0.2.0)
      , description : "Read pdf ebooks online or offline with cloud sync"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://reader.justinlovinger.com/"
      }
  , Project
      { name: "timeandreturn"
      , tags : { firstType : Library, otherTypes : empty, firstLanguage : Javascript, otherLanguages : empty, scope : Minor }
      , published : unsafeDate 2018 October 5 -- Date of v1.0.0
      , updated : unsafeDate 2018 October 5
      , description : "Easily time blocks of code for performance profiling"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/timeandreturn"
      }
  , Project
      { name: "chai-return-bool"
      , tags : { firstType : Library, otherTypes : empty, firstLanguage : Javascript, otherLanguages : empty, scope : Minor }
      , published : unsafeDate 2018 October 13 -- Date of v1.0.0
      , updated : unsafeDate 2018 October 13 -- Date of v1.0.1
      , description : "Compose chai.js assertions with libraries expecting boolean"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/chai-return-bool"
      }
  , Project
      { name : "nix-purescript-concur-frontend-starter"
      , tags : { firstType : Template, otherTypes : empty, firstLanguage : Nix, otherLanguages : singleton PureScript, scope : Minor }
      , published : unsafeDate 2020 January 8 -- Date of v1.0.0
      , updated : unsafeDate 2020 January 27 -- Date of v1.0.2
      , description : "Build a modern frontend with Nix, PureScript, and Concur"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/nix-purescript-concur-frontend-starter"
      }
  ]

published ∷ Project → Date
published (Project p) = p.published

updated ∷ Project → Date
updated (Project p) = p.updated
