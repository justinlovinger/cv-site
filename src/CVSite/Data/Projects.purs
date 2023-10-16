module CVSite.Data.Projects
  ( Project
  , description
  , languages
  , longDescription
  , name
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

import CVSite.Data.Tags (class TagLike, class Tagged, Tag, toTag)
import CVSite.Data.Tags as T
import Data.Array.NonEmpty (NonEmptyArray, singleton)
import Data.Array.NonEmpty.Unsafe (unsafeFromArray)
import Data.Date (Date, Month(January, February, March, April, May, June, July, August, September, October, November, December))
import Data.Date.Unsafe (unsafeDate)
import Data.HashSet (HashSet, fromArray, fromFoldable, union)
import Data.HashSet as HS
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Generated.Files (files)
import Web.HTML.History (URL(URL))

newtype Project = Project
  { name ∷ String
  , tags ∷ { types ∷ NonEmptyArray Type_
           , topics ∷ Maybe (NonEmptyArray Topic)
           , languages ∷ NonEmptyArray Language
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
  tags p = fromArray [ T.Project, scope p ] `union` types p `union` topics p `union` languages p

data Type_ = Website | PWA | Library | Program | Template | Game

instance tagLikeType_ ∷ TagLike Type_ where
  toTag Website = T.Website
  toTag PWA = T.PWA
  toTag Library = T.Library
  toTag Program = T.Program
  toTag Template = T.Template
  toTag Game = T.Game

data Topic = MachineLearning
           | Optimization
           | TextSummarization

instance tagLikeTopic ∷ TagLike Topic where
  toTag MachineLearning = T.PrMachineLearning
  toTag Optimization = T.PrOptimization
  toTag TextSummarization = T.PrTextSummarization

data Language = Rust | Haskell | PureScript | Nix | Python | JavaScript | CSharp

instance tagLikeLanguage ∷ TagLike Language where
  toTag Rust = T.Rust
  toTag Haskell = T.Haskell
  toTag PureScript = T.PureScript
  toTag Nix = T.Nix
  toTag Python = T.Python
  toTag JavaScript = T.JavaScript
  toTag CSharp = T.CSharp

data Scope = Major | Medium | Minor

instance tagLikeScope ∷ TagLike Scope where
  toTag Major = T.Major
  toTag Medium = T.Medium
  toTag Minor = T.Minor

-- Note: unsafe operations are used
-- because these values are manually written
-- and will not change at runtime.
projects ∷ Array Project 
projects =
  [ Project
      { name : "Labyrinth RL"
      , tags : { types : singleton Game
               , topics : Nothing
               , languages : singleton Python
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
      , tags : { types : singleton Game
               , topics : Nothing
               , languages : singleton CSharp
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
      , tags : { types : singleton Website
               , topics : Just $ unsafeFromArray [ TextSummarization, Optimization ]
               , languages : unsafeFromArray [ Python, JavaScript]
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
      , tags : { types : singleton Website
               , topics : Just $ unsafeFromArray [ MachineLearning, Optimization]
               , languages : unsafeFromArray [ Python, JavaScript ]
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
      { name : "optimal-py"
      , tags : { types : singleton Library
               , topics : Just $ unsafeFromArray [ Optimization,  MachineLearning ]
               , languages : singleton Python
               , scope : Major
               }
      , published : unsafeDate 2016 November 1 -- Date of v0.1.0
      , updated : unsafeDate 2017 September 4 -- Date of v0.2.0
      , description : "Perform metaheuristic optimization with Genetic Algorithms, Gravitational Search, Cross Entropy, and PBIL"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/optimal-py"
      }
  , Project
      { name : "optimal-py-learning"
      , tags : { types : singleton Library
               , topics : Just $ unsafeFromArray [ MachineLearning, Optimization ]
               , languages : singleton Python
               , scope : Major
               }
      , published : unsafeDate 2017 September 14 -- Date readme was added. Approximate date of public GitHub.
      , updated : unsafeDate 2018 April 20 -- Date of last commit, before recent minor commits
      , description : "Perform machine learning with several models and powerful numerical optimization methods"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/optimal-py-learning"
      }
  , Project
      { name : "Reader"
      , tags : { types : unsafeFromArray [ Website, PWA ]
               , topics : Nothing
               , languages : unsafeFromArray [ Python, JavaScript ]
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
      , tags : { types : singleton Library
               , topics : Nothing
               , languages : singleton JavaScript
               , scope : Minor
               }
      , published : unsafeDate 2018 October 5 -- Date of v1.0.0
      , updated : unsafeDate 2018 October 5
      , description : "Easily time blocks of code for performance profiling"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/timeandreturn"
      }
  , Project
      { name: "chai-return-bool"
      , tags : { types : singleton Library
               , topics : Nothing
               , languages : singleton JavaScript
               , scope : Minor
               }
      , published : unsafeDate 2018 October 13 -- Date of v1.0.0
      , updated : unsafeDate 2018 October 13 -- Date of v1.0.1
      , description : "Compose chai.js assertions with libraries expecting boolean"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/chai-return-bool"
      }
  , Project
      { name : "nix-purescript-concur-frontend-starter"
      , tags : { types : singleton Template
               , topics : Nothing
               , languages : unsafeFromArray [ Nix, PureScript ]
               , scope : Minor
               }
      , published : unsafeDate 2020 January 8 -- Date of v1.0.0
      , updated : unsafeDate 2020 January 27 -- Date of v1.0.2
      , description : "Build a modern frontend with Nix, PureScript, and Concur"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/nix-purescript-concur-frontend-starter"
      }
  , Project
      { name : "My CV Site"
      , tags : { types : singleton Website
               , topics : Nothing
               , languages : unsafeFromArray [ PureScript, Nix ]
               , scope : Medium
               }
      , published : unsafeDate 2020 March 10
      , updated : unsafeDate 2023 May 1
      , description : "View the source code for this website"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/cv-site"
      }
  , Project
      { name : "Collage"
      , tags : { types : singleton Program
               , topics : Nothing
               , languages : unsafeFromArray [ Haskell, Nix ]
               , scope : Minor
               }
      , published : unsafeDate 2020 August 30
      , updated : unsafeDate 2020 August 30
      , description : "Create a collage of semi-random images"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/collage"
      }
  , Project
      { name : "webgrep"
      , tags : { types : singleton Program
               , topics : Nothing
               , languages : unsafeFromArray [ Rust, Nix ]
               , scope : Medium
               }
      , published : unsafeDate 2022 June 13
      , updated : unsafeDate 2022 June 13
      , description : "Recursively search the web"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/webgrep"
      }
  , Project
      { name : "optimal-rs"
      , tags : { types : singleton Library
               , topics : Just $ unsafeFromArray [ Optimization,  MachineLearning ]
               , languages : unsafeFromArray [ Rust, Nix ]
               , scope : Major
               }
      , published : unsafeDate 2023 September 6
      , updated : unsafeDate 2023 September 12
      , description : "Perform mathematical optimization and machine learning with an intuitive and transparent iterator-based API"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/optimal-rs"
      }
  , Project
      { name : "owm"
      , tags : { types : singleton Program
               , topics : Just $ singleton Optimization
               , languages : unsafeFromArray [ Rust, Nix ]
               , scope : Medium
               }
      , published : unsafeDate 2023 September 6
      , updated : unsafeDate 2023 September 19
      , description : "Use mathematical optimization to invent window-layouts on-the-fly"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/owm"
      }
  , Project
      { name : "tag"
      , tags : { types : singleton Program
               , topics : Nothing
               , languages : unsafeFromArray [ Rust, Nix ]
               , scope : Medium
               }
      , published : unsafeDate 2023 October 16
      , updated : unsafeDate 2023 October 16
      , description : "Automatically organize files with a simple tagging system"
      , longDescription : Nothing
      , teamRole : Nothing
      , url : Just $ URL "https://github.com/justinlovinger/tag"
      }
  ]

name ∷ Project → String
name (Project p) = p.name

types ∷ Project → HashSet Tag
types (Project p) = fromFoldable $ map toTag p.tags.types

topics ∷ Project → HashSet Tag
topics (Project p) = maybe
  (HS.singleton T.PrNoTopics)
  (\ts → fromFoldable $ map toTag ts)
  p.tags.topics

languages ∷ Project → HashSet Tag
languages (Project p) = fromFoldable $ map toTag p.tags.languages

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
