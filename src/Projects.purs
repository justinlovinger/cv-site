module Projects
  ( Language(..)
  , Project
  , Scope(..)
  , Topic(..)
  , Type_(..)
  , description
  , languages
  , longDescription
  , name
  , noTopics
  , projects
  , published
  , scope
  , teamRole
  , topics
  , types
  , updated
  , url
  ) where

import Prelude

import Data.Date (Date, Month(January, February, March, April, June, July, September, October, November, December))
import Data.Date.Unsafe (unsafeDate)
import Data.HashSet (HashSet, empty, insert, singleton, union)
import Data.HashSet as HS
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Tag (class TagLike, class Tagged, Tag, namespacedTag, tag, toTag)
import Generated.Files (files)
import Web.HTML.History (URL(URL))

newtype Project = Project
  { name ∷ String
  , tags ∷ { firstType ∷ Type_, otherTypes ∷ HashSet Type_
           , topics ∷ Maybe { firstTopic ∷ Topic, otherTopics ∷ HashSet Topic }
           , firstLanguage ∷ Language, otherLanguages ∷ HashSet Language
           , scope ∷ Scope
           }
  , published ∷ Date
  , updated ∷ Date -- Last major update
  , description ∷ String -- No punctuation
  , longDescription ∷ Maybe String
  , teamRole ∷ Maybe String
  , url ∷ Maybe URL
  }

instance taggedProject ∷ Tagged Project where
  tags p = scope p `insert` types p `union` topics p `union` languages p

data Type_ = Website | PWA | Library | Template | Game
instance tagLikeType_ ∷ TagLike Type_ where toTag = tag <<< show
instance hashableType_ ∷ Hashable Type_ where hash = hashShow
derive instance eqType_ ∷ Eq Type_
instance showType_ ∷ Show Type_ where
  show Website = "website"
  show PWA = "PWA"
  show Library = "library"
  show Template = "template"
  show Game = "game"

data Topic = MachineLearning
           | Optimization
           | TextSummarization
instance tagLikeTopic ∷ TagLike Topic where toTag = namespacedTag "pr" <<< show
instance hashableTopic ∷ Hashable Topic where hash = hashShow
derive instance eqTopic ∷ Eq Topic
instance showTopic ∷ Show Topic where
  show MachineLearning = "machine learning"
  show Optimization = "optimization"
  show TextSummarization = "text summarization"

data Language = Nix | PureScript | Python | Javascript | CSharp
instance tagLikeLanguage ∷ TagLike Language where toTag = tag <<< show
instance hashableLanguage ∷ Hashable Language where hash = hashShow
derive instance eqLanguage ∷ Eq Language
instance showLanguage ∷ Show Language where
  show Nix = "Nix"
  show PureScript = "PureScript"
  show Python = "Python"
  show Javascript = "JavaScript"
  show CSharp = "C#"

data Scope = Major | Medium | Minor
instance tagLikeScope ∷ TagLike Scope where toTag = tag <<< show
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
      , tags : { firstType : Game, otherTypes : empty
               , topics : Nothing
               , firstLanguage : Python, otherLanguages : empty
               , scope : Medium
               }
      , published : unsafeDate 2014 October 20 -- Date of last commit
      , updated : unsafeDate 2014 October 20 -- Never updated
      , description : "Escape a labyrinth with traditional roguelike mechanics and a unique victory condition"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL files.projects."labyrinth-rl".url
      }
  , Project
      { name : "Gladiator Manager"
      , tags : { firstType : Game, otherTypes : empty
               , topics : Nothing
               , firstLanguage : CSharp, otherLanguages : empty
               , scope : Medium
               }
      , published : unsafeDate 2014 December 10 -- Date of last modified file
      , updated : unsafeDate 2014 December 10 -- Never updated
      , description : "Manage gladiators in physics driven combat"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL files.projects."gladiator-manager".url
      }
  , Project
      { name : "Gist"
      , tags : { firstType : Website, otherTypes : empty
               , topics : Just { firstTopic : TextSummarization, otherTopics : singleton Optimization }
               , firstLanguage : Python, otherLanguages : singleton Javascript
               , scope : Major
               }
      , published : unsafeDate 2016 February 12 -- Date of first App Engine version
      , updated : unsafeDate 2017 March 27 -- Date of second to last commit. Last notes that change was made earlier, but not committed.
      , description : "Automatically summarize text"
      , longDescription : Nothing
      , teamRole : Just "Team Lead and AI"
      , url : Just $ URL "https://gist.justinlovinger.com/"
      }
  , Project
      { name : "Clever Surveys"
      , tags : { firstType : Website, otherTypes : empty
               , topics : Just { firstTopic : MachineLearning, otherTopics : singleton Optimization }
               , firstLanguage : Python, otherLanguages : singleton Javascript
               , scope : Major
               }
      , published : unsafeDate 2016 July 26 -- Date of v1.0.0 in Clever Surveys repo
      , updated : unsafeDate 2017 March 7 -- Date of last non-hotfix patch (v1.1.0)
      , description : "Make surveys with predictive machine learning trained by survey responses"
      , longDescription : Just "Website with predictive machine learning trained by survey responses. Users can answer surveys and receive predictions. Users can create surveys with built-in survey builder."
      , teamRole : Nothing
      , url : Just $ URL "https://cleversurveys.com/"
      }
  , Project
      { name : "Optimal"
      , tags : { firstType : Library, otherTypes : empty
               , topics : Just { firstTopic : Optimization, otherTopics : singleton MachineLearning }
               , firstLanguage : Python, otherLanguages : empty
               , scope : Major
               }
      , published : unsafeDate 2016 November 1 -- Date of v0.1.0
      , updated : unsafeDate 2017 September 4 -- Date of v0.2.0
      , description : "Perform metaheuristic optimization with Genetic Algorithms, Gravitational Search, Cross Entropy, and PBIL"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/optimal"
      }
  , Project
      { name : "Learning"
      , tags : { firstType : Library, otherTypes : empty
               , topics : Just { firstTopic : MachineLearning, otherTopics : singleton Optimization }
               , firstLanguage : Python, otherLanguages : empty
               , scope : Major
               }
      , published : unsafeDate 2017 September 14 -- Date readme was added. Approximate date of public GitHub.
      , updated : unsafeDate 2018 April 20 -- Date of last commit, before recent minor commits
      , description : "Perform machine learning with several models and powerful numerical optimization methods"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/learning"
      }
  , Project
      { name : "Reader"
      , tags : { firstType : Website, otherTypes : singleton PWA
               , topics : Nothing
               , firstLanguage : Python, otherLanguages : singleton Javascript
               , scope : Medium
               }
      , published : unsafeDate 2018 June 5 -- Date of first App Engine version
      , updated : unsafeDate 2018 June 9 -- Date of last tagged version (v0.2.0)
      , description : "Read pdf ebooks online or offline with cloud sync"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://reader.justinlovinger.com/"
      }
  , Project
      { name: "timeandreturn"
      , tags : { firstType : Library, otherTypes : empty
               , topics : Nothing
               , firstLanguage : Javascript, otherLanguages : empty
               , scope : Minor
               }
      , published : unsafeDate 2018 October 5 -- Date of v1.0.0
      , updated : unsafeDate 2018 October 5
      , description : "Easily time blocks of code for performance profiling"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/timeandreturn"
      }
  , Project
      { name: "chai-return-bool"
      , tags : { firstType : Library, otherTypes : empty
               , topics : Nothing
               , firstLanguage : Javascript, otherLanguages : empty
               , scope : Minor
               }
      , published : unsafeDate 2018 October 13 -- Date of v1.0.0
      , updated : unsafeDate 2018 October 13 -- Date of v1.0.1
      , description : "Compose chai.js assertions with libraries expecting boolean"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/chai-return-bool"
      }
  , Project
      { name : "nix-purescript-concur-frontend-starter"
      , tags : { firstType : Template, otherTypes : empty
               , topics : Nothing
               , firstLanguage : Nix, otherLanguages : singleton PureScript
               , scope : Minor
               }
      , published : unsafeDate 2020 January 8 -- Date of v1.0.0
      , updated : unsafeDate 2020 January 27 -- Date of v1.0.2
      , description : "Build a modern frontend with Nix, PureScript, and Concur"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/nix-purescript-concur-frontend-starter"
      }
  , Project
      { name : "My CV Site"
      , tags : { firstType : Website, otherTypes : empty
               , topics : Nothing
               , firstLanguage : PureScript, otherLanguages : singleton Nix
               , scope : Medium
               }
      , published : unsafeDate 2020 March 10
      , updated : unsafeDate 2020 March 10
      , description : "View the source code for this website"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/JustinLovinger/cv-site"
      }
  ]

name ∷ Project → String
name (Project p) = p.name

types ∷ Project → HashSet Tag
types (Project p) = insert (toTag p.tags.firstType) (HS.map toTag p.tags.otherTypes)

topics ∷ Project → HashSet Tag
topics (Project p) = maybe
  (singleton noTopics)
  (\{ firstTopic, otherTopics } → insert (toTag firstTopic) (HS.map toTag otherTopics))
  p.tags.topics

noTopics ∷ Tag
noTopics = tag "other"

languages ∷ Project → HashSet Tag
languages (Project p) = insert (toTag p.tags.firstLanguage) (HS.map toTag p.tags.otherLanguages)

scope ∷ Project → Tag
scope (Project p) = toTag p.tags.scope

published ∷ Project → Date
published (Project p) = p.published

updated ∷ Project → Date
updated (Project p) = p.updated

description ∷ Project → String
description (Project p) = p.description

longDescription ∷ Project → Maybe String
longDescription (Project p) = p.longDescription

teamRole ∷ Project → Maybe String
teamRole (Project p) = p.teamRole

url ∷ Project → Maybe URL
url (Project p) = p.url
