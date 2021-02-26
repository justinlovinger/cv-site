module CVSite.Data.Tags where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.HashSet (HashSet)
import Data.Hashable (class Hashable)

class Tagged a where
  tags ∷ a → HashSet Tag

class TagLike a where
  toTag ∷ a → Tag

data Tag
  -- Super-categories
  = Education
  | Project
  | Publication
  -- Education
  -- Degree
  | Bachelors
  | Masters
  -- Project
  -- Type
  | Game
  | Library
  | PWA
  | Program
  | Template
  | Website
  -- Topic
  | PrMachineLearning
  | PrNoTopics
  | PrOptimization
  | PrTextSummarization
  -- Language
  | CSharp
  | Haskell
  | JavaScript
  | Nix
  | PureScript
  | Python
  -- Scope
  | Major
  | Medium
  | Minor
  -- Publications
  -- Type
  | Conference
  | Journal
  | Thesis
  -- Topic
  | PuIncrementalLearning
  | PuMachineLearning
  | PuOptimization
  | PuSearch
  | PuTextSummarization

derive instance eqTag ∷ Eq Tag

derive instance genericTag ∷ Generic Tag _

instance hashableTag ∷ Hashable Tag where
  -- Super-categories
  hash Education = 1
  hash Project = 2
  hash Publication = 3
  -- Education
  -- Degree
  hash Bachelors = 11
  hash Masters = 12
  -- Project
  -- Type
  hash Game = 211
  hash Library = 212
  hash PWA = 213
  hash Program = 2135
  hash Template = 214
  hash Website = 215
  -- Topic
  hash PrMachineLearning = 221
  hash PrNoTopics = 222
  hash PrOptimization = 223
  hash PrTextSummarization = 224
  -- Language
  hash CSharp = 231
  hash Haskell = 2315
  hash JavaScript = 232
  hash Nix = 233
  hash PureScript = 234
  hash Python = 235
  -- Scope
  hash Major = 241
  hash Medium = 242
  hash Minor = 243
  -- Publications
  -- Type
  hash Conference = 311
  hash Journal = 312
  hash Thesis = 313
  -- Topic
  hash PuIncrementalLearning = 321
  hash PuMachineLearning = 322
  hash PuOptimization = 323
  hash PuSearch = 324
  hash PuTextSummarization = 325

derive instance ordTag ∷ Ord Tag

instance showTag ∷ Show Tag where
  -- Super-categories
  show Education = "education"
  show Project = "project"
  show Publication = "publication"
  -- Education
  -- Degree
  show Bachelors = "bachelors"
  show Masters = "masters"
  -- Project
  -- Type
  show Game = "game"
  show Library = "library"
  show PWA = "PWA"
  show Program = "program"
  show Template = "template"
  show Website = "website"
  -- Topic
  show PrMachineLearning = "machine learning"
  show PrNoTopics = "other"
  show PrOptimization = "optimization"
  show PrTextSummarization = "text summarization"
  -- Language
  show CSharp = "C#"
  show Haskell = "Haskell"
  show JavaScript = "JavaScript"
  show Nix = "Nix"
  show PureScript = "PureScript"
  show Python = "Python"
  -- Scope
  show Major = "major"
  show Medium = "medium"
  show Minor = "minor"
  -- Publications
  -- Type
  show Conference = "conference"
  show Journal = "journal"
  show Thesis = "thesis"
  -- Topic
  show PuIncrementalLearning = "incremental learning"
  show PuMachineLearning = "machine learning"
  show PuOptimization = "optimization"
  show PuSearch = "search"
  show PuTextSummarization = "text summarization"
