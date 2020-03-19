module Publications (Publication, Topic(..), Type_(..), description, documentUrl, name, publications, published, topics, type_, url) where

import Prelude

import Data.Date (Date, Month(February, May, July, September, October, November, December))
import Data.Date.Unsafe (unsafeDate)
import Data.HashSet (HashSet, empty, fromArray, insert, singleton, union)
import Data.HashSet as HS
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tag (class TagLike, class Tagged, Tag, namespacedTag, tag, toTag)
import Generated.Files (files)
import Web.HTML.History (URL(URL))

newtype Publication = Publication
  { name ∷ String
  , tags ∷ { type_ ∷ Type_, firstTopic ∷ Topic, otherTopics ∷ HashSet Topic }
  , published ∷ Date
  , description ∷ String -- No punctuation
  , documentUrl ∷ URL
  , url ∷ Maybe URL
  }
instance taggedPublication ∷ Tagged Publication where
  tags (Publication p) = fromArray [ toTag p.tags.type_, toTag p.tags.firstTopic ]
                   `union` (HS.map toTag p.tags.otherTopics)

data Type_ = Conference | Journal | Thesis
instance tagLikeType_ ∷ TagLike Type_ where toTag = tag <<< show
instance hashableType_ ∷ Hashable Type_ where hash = hash <<< show
derive instance eqType_ ∷ Eq Type_
instance showType_ ∷ Show Type_ where
  show Conference = "conference"
  show Journal = "journal"
  show Thesis = "thesis"

data Topic = IncrementalLearning
           | MachineLearning
           | Optimization
           | Search
           | TextSummarization
instance tagLikeTopic ∷ TagLike Topic where toTag = namespacedTag "pa" <<< show
instance hashableTopic ∷ Hashable Topic where hash = hash <<< show
derive instance eqTopic ∷ Eq Topic
instance showTopic ∷ Show Topic where
  show IncrementalLearning = "incremental learning"
  show MachineLearning = "machine learning"
  show Optimization = "optimization"
  show Search = "search"
  show TextSummarization = "text summarization"

-- Note: dates are constructed
-- with an unsafe `unsafeDate`
-- because these dates are manually written
-- and will not change.
publications ∷ Array Publication
publications =
  [ Publication
      { name : "Harnessing Mother Nature: Optimizing Genetic Algorithms for Adaptive Systems"
      , tags : { type_ : Conference, firstTopic : Optimization, otherTopics : empty }
      , published : unsafeDate 2014 November 3 -- Date of conference
      , description : "Automatically optimize genetic algorithm hyperparameters"
      , documentUrl : URL files.publications."harnessing-mother-nature".url
      , url : Just $ URL "https://www.sciencedirect.com/science/article/pii/S1877050914012824"
      }
  , Publication
      { name : "Scrubbing the web for association rules: An application in predictive text"
      , tags : { type_ : Conference, firstTopic : MachineLearning, otherTopics : empty }
      , published : unsafeDate 2015 December 9 -- Date of conference
      , description : "Data mine social media to improve smartphone keyboard predictive text"
      , documentUrl : URL files.publications."scrubbing-the-web-for-association-rules".url
      , url : Just $ URL "https://ieeexplore.ieee.org/abstract/document/7424353"
      }
  , Publication
      { name : "Neural Field: Supervised apportioned incremental learning (SAIL)"
      , tags : { type_ : Conference, firstTopic : MachineLearning, otherTopics : singleton IncrementalLearning }
      , published : unsafeDate 2016 July 24 -- Date of conference
      , description : "Learn in real-time with a novel incremental supervised learning model"
      , documentUrl : URL files.publications."neural-field".url
      , url : Just $ URL "https://ieeexplore.ieee.org/abstract/document/7727510"
      }
  , Publication
      { name : "The effect of human thought on data: an analysis of self-reported data in supervised learning and neural networks"
      , tags : { type_ : Journal, firstTopic : MachineLearning, otherTopics : empty }
      , published : unsafeDate 2017 February 16 -- Date published
      , description : "Examine the difficulty of supervised learning with data directly reported by people"
      , documentUrl : URL files.publications."the-effect-of-human-thought-on-data".url
      , url : Just $ URL "https://link.springer.com/article/10.1007/s13748-017-0118-4"
      }
  , Publication
      { name : "Gist: general integrated summarization of text and reviews"
      , tags : { type_ : Journal, firstTopic : TextSummarization, otherTopics : singleton Optimization }
      , published : unsafeDate 2017 October 10 -- Date published. Issued much later.
      , description : "Automatically summarize text and reviews"
      , documentUrl : URL files.publications.gist.url
      , url : Just $ URL "https://link.springer.com/article/10.1007/s00500-017-2882-2"
      }
  , Publication
      { name : "Enhanced Simplified Memory-bounded A Star (SMA*+)"
      , tags : { type_ : Conference, firstTopic : Search, otherTopics : empty }
      , published : unsafeDate 2017 October 19 -- Date of conference
      , description : "Search in low memory environments with a memory-efficient optimal search algorithm"
      , documentUrl : URL files.publications."sma-star-plus".url
      , url : Just $ URL "https://easychair.org/publications/paper/TL2M"
      }
  , Publication
      { name : "A Tutorial on Supervised Learning from the Perspective of Mathematical Optimization"
      , tags : { type_ : Thesis, firstTopic : MachineLearning, otherTopics : singleton Optimization }
      , published : unsafeDate 2018 May 22 -- Date of thesis defense
      , description : "Master supervised machine learning at a fundamental level"
      , documentUrl : URL files.publications."supervised-learning-as-mathematical-optimization-tutorial".url
      , url : Nothing
      }
  , Publication
      { name : "Infinite Lattice Learner: an ensemble for incremental learning"
      , tags : { type_ : Journal, firstTopic : MachineLearning, otherTopics : singleton IncrementalLearning }
      , published : unsafeDate 2019 September 6 -- Date published
      , description : "Learn in real-time with a novel incremental supervised learning ensemble"
      , documentUrl : URL files.publications."infinite-lattice-learner".url
      , url : Just $ URL "https://link.springer.com/article/10.1007/s00500-019-04330-7"
      }
  ]

name ∷ Publication → String
name (Publication p) = p.name

type_ ∷ Publication → Tag
type_ (Publication p) = (toTag p.tags.type_)

topics ∷ Publication → HashSet Tag
topics (Publication p) = insert (toTag p.tags.firstTopic) (HS.map toTag p.tags.otherTopics)

published ∷ Publication → Date
published (Publication p) = p.published

description ∷ Publication → String
description (Publication p) = p.description

documentUrl ∷ Publication → URL
documentUrl (Publication p) = p.documentUrl

url ∷ Publication → Maybe URL
url (Publication p) = p.url
