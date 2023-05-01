module CVSite.Data.Publications
  ( Publication
  , codeUrl
  , description
  , documentUrl
  , name
  , publicationUrl
  , publications
  , published
  , topics
  , type_
  ) where

import Prelude

import CVSite.Data.Tags (class TagLike, class Tagged, Tag, toTag)
import CVSite.Data.Tags as T
import Data.Array.NonEmpty (NonEmptyArray, singleton)
import Data.Array.NonEmpty.Unsafe (unsafeFromArray)
import Data.Date (Date, Month(January, February, May, July, September, October, November, December))
import Data.Date.Unsafe (unsafeDate)
import Data.HashSet (HashSet, fromArray, fromFoldable, union)
import Data.Maybe (Maybe(Just, Nothing))
import Generated.Files (files)
import Web.HTML.History (URL(URL))

newtype Publication = Publication
  { name ∷ String
  , tags ∷ { type_ ∷ Type_, topics ∷ NonEmptyArray Topic }
  , published ∷ Date
  , description ∷ String -- No punctuation
  , documentUrl ∷ URL
  , codeUrl :: Maybe URL
  , publicationUrl ∷ Maybe URL
  }
instance taggedPublication ∷ Tagged Publication where
  tags p = fromArray [ T.Publication, type_ p ] `union` (topics p)

data Type_ = Conference | Journal | Thesis

instance tagLikeType_ ∷ TagLike Type_ where
  toTag Conference = T.Conference
  toTag Journal = T.Journal
  toTag Thesis = T.Thesis

data Topic = IncrementalLearning
           | MachineLearning
           | Optimization
           | Search
           | TextSummarization

instance tagLikeTopic ∷ TagLike Topic where
  toTag IncrementalLearning = T.PuIncrementalLearning
  toTag MachineLearning = T.PuMachineLearning
  toTag Optimization = T.PuOptimization
  toTag Search = T.PuSearch
  toTag TextSummarization = T.PuTextSummarization

-- Note: unsafe operations are used
-- because these values are manually written
-- and will not change at runtime.
publications ∷ Array Publication
publications =
  [ Publication
      { name : "Harnessing Mother Nature: Optimizing Genetic Algorithms for Adaptive Systems"
      , tags : { type_ : Conference, topics : singleton Optimization }
      , published : unsafeDate 2014 November 3 -- Date of conference
      , description : "Automatically optimize genetic algorithm hyperparameters"
      , documentUrl : URL files.publications."harnessing-mother-nature".url
      , codeUrl : Nothing
      , publicationUrl : Just $ URL "https://www.sciencedirect.com/science/article/pii/S1877050914012824"
      }
  , Publication
      { name : "Scrubbing the web for association rules: An application in predictive text"
      , tags : { type_ : Conference, topics : singleton MachineLearning }
      , published : unsafeDate 2015 December 9 -- Date of conference
      , description : "Data mine social media to improve smartphone keyboard predictive text"
      , documentUrl : URL files.publications."scrubbing-the-web-for-association-rules".url
      , codeUrl : Nothing
      , publicationUrl : Just $ URL "https://ieeexplore.ieee.org/abstract/document/7424353"
      }
  , Publication
      { name : "Neural Field: Supervised apportioned incremental learning (SAIL)"
      , tags : { type_ : Conference, topics : unsafeFromArray [ MachineLearning, IncrementalLearning ] }
      , published : unsafeDate 2016 July 24 -- Date of conference
      , description : "Learn in real-time with a novel incremental supervised learning model"
      , documentUrl : URL files.publications."neural-field".url
      , codeUrl : Nothing
      , publicationUrl : Just $ URL "https://ieeexplore.ieee.org/abstract/document/7727510"
      }
  , Publication
      { name : "The effect of human thought on data: an analysis of self-reported data in supervised learning and neural networks"
      , tags : { type_ : Journal, topics : singleton MachineLearning }
      , published : unsafeDate 2017 February 16 -- Date published
      , description : "Examine the difficulty of supervised learning with data directly reported by people"
      , documentUrl : URL files.publications."the-effect-of-human-thought-on-data".url
      , codeUrl : Nothing
      , publicationUrl : Just $ URL "https://link.springer.com/article/10.1007/s13748-017-0118-4"
      }
  , Publication
      { name : "Gist: general integrated summarization of text and reviews"
      , tags : { type_ : Journal, topics : unsafeFromArray [ TextSummarization, Optimization ] }
      , published : unsafeDate 2017 October 10 -- Date published. Issued much later.
      , description : "Automatically summarize text and reviews"
      , documentUrl : URL files.publications.gist.url
      , codeUrl : Nothing
      , publicationUrl : Just $ URL "https://link.springer.com/article/10.1007/s00500-017-2882-2"
      }
  , Publication
      { name : "Enhanced Simplified Memory-bounded A Star (SMA*+)"
      , tags : { type_ : Conference, topics : singleton Search }
      , published : unsafeDate 2017 October 19 -- Date of conference
      , description : "Search in low memory environments with a memory-efficient optimal search algorithm"
      , documentUrl : URL files.publications."sma-star-plus".url
      , codeUrl : Nothing
      , publicationUrl : Just $ URL "https://easychair.org/publications/paper/TL2M"
      }
  , Publication
      { name : "A Tutorial on Supervised Learning from the Perspective of Mathematical Optimization"
      , tags : { type_ : Thesis, topics : unsafeFromArray [ MachineLearning, Optimization ] }
      , published : unsafeDate 2018 May 22 -- Date of thesis defense
      , description : "Master supervised machine learning at a fundamental level"
      , documentUrl : URL files.publications."supervised-learning-as-mathematical-optimization-tutorial".url
      , codeUrl : Nothing
      , publicationUrl : Nothing
      }
  , Publication
      { name : "Infinite Lattice Learner: an ensemble for incremental learning"
      , tags : { type_ : Journal, topics : unsafeFromArray [ MachineLearning, IncrementalLearning ] }
      , published : unsafeDate 2019 September 6 -- Date published
      , description : "Learn in real-time with a novel incremental supervised learning ensemble"
      , documentUrl : URL files.publications."infinite-lattice-learner".url
      , codeUrl : Just $ URL "https://github.com/JustinLovinger/ill"
      , publicationUrl : Just $ URL "https://link.springer.com/article/10.1007/s00500-019-04330-7"
      }
  , Publication
      { name : "AUTO: Supervised Learning With Full Model Search and Global Optimization"
      , tags : { type_ : Journal, topics : unsafeFromArray [ MachineLearning, Optimization ] }
      , published : unsafeDate 2023 January 11 -- Date published
      , description : "Incrementally build models with derivative-free optimization and reinforcement learning to solve supervised learning problems"
      , documentUrl : URL files.publications."auto".url
      , codeUrl : Nothing
      , publicationUrl : Just $ URL "https://www.tandfonline.com/doi/full/10.1080/0952813X.2023.2165717"
      }
  ]

name ∷ Publication → String
name (Publication p) = p.name

type_ ∷ Publication → Tag
type_ (Publication p) = (toTag p.tags.type_)

topics ∷ Publication → HashSet Tag
topics (Publication p) = fromFoldable $ map toTag p.tags.topics

published ∷ Publication → Date
published (Publication p) = p.published

description ∷ Publication → String
description (Publication p) = p.description

documentUrl ∷ Publication → URL
documentUrl (Publication p) = p.documentUrl

codeUrl ∷ Publication → Maybe URL
codeUrl (Publication p) = p.codeUrl

publicationUrl ∷ Publication → Maybe URL
publicationUrl (Publication p) = p.publicationUrl
