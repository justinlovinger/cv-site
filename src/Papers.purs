module Papers (Paper, Topic(..), Type_(..), description, documentUrl, name, papers, published, topics, type_, url) where

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

newtype Paper = Paper
  { name ∷ String
  , tags ∷ { type_ ∷ Type_, firstTopic ∷ Topic, otherTopics ∷ HashSet Topic }
  , published ∷ Date
  , description ∷ String -- No punctuation
  , documentUrl ∷ URL
  , url ∷ Maybe URL
  }
instance taggedPaper ∷ Tagged Paper where
  tags (Paper p) = fromArray [ toTag p.tags.type_, toTag p.tags.firstTopic ]
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
papers ∷ Array Paper 
papers =
  [ Paper
      { name : "Harnessing Mother Nature: Optimizing Genetic Algorithms for Adaptive Systems"
      , tags : { type_ : Conference, firstTopic : Optimization, otherTopics : empty }
      , published : unsafeDate 2014 November 3 -- Date of conference
      , description : "Automatic optimization of genetic algorithm hyperparameters"
      , documentUrl : URL files.papers."harnessing-mother-nature".url
      , url : Just $ URL "https://www.sciencedirect.com/science/article/pii/S1877050914012824"
      }
  , Paper
      { name : "Scrubbing the web for association rules: An application in predictive text"
      , tags : { type_ : Conference, firstTopic : MachineLearning, otherTopics : empty }
      , published : unsafeDate 2015 December 9 -- Date of conference
      , description : "Data mine social media to improve smartphone keyboard predictive text"
      , documentUrl : URL files.papers."scrubbing-the-web-for-association-rules".url
      , url : Just $ URL "https://ieeexplore.ieee.org/abstract/document/7424353"
      }
  , Paper
      { name : "Neural Field: Supervised apportioned incremental learning (SAIL)"
      , tags : { type_ : Conference, firstTopic : MachineLearning, otherTopics : singleton IncrementalLearning }
      , published : unsafeDate 2016 July 24 -- Date of conference
      , description : "Real-time incremental supervised learning model"
      , documentUrl : URL files.papers."neural-field".url
      , url : Just $ URL "https://ieeexplore.ieee.org/abstract/document/7727510"
      }
  , Paper
      { name : "The effect of human thought on data: an analysis of self-reported data in supervised learning and neural networks"
      , tags : { type_ : Journal, firstTopic : MachineLearning, otherTopics : empty }
      , published : unsafeDate 2017 February 16 -- Date published
      , description : "Examine the difficulty of supervised learning with data directly reported by people"
      , documentUrl : URL files.papers."the-effect-of-human-thought-on-data".url
      , url : Just $ URL "https://link.springer.com/article/10.1007/s13748-017-0118-4"
      }
  , Paper
      { name : "Gist: general integrated summarization of text and reviews"
      , tags : { type_ : Journal, firstTopic : TextSummarization, otherTopics : singleton Optimization }
      , published : unsafeDate 2017 October 10 -- Date published. Issued much later.
      , description : "Automatically summarize text and reviews"
      , documentUrl : URL files.papers.gist.url
      , url : Just $ URL "https://link.springer.com/article/10.1007/s00500-017-2882-2"
      }
  , Paper
      { name : "Enhanced Simplified Memory-bounded A Star (SMA*+)"
      , tags : { type_ : Conference, firstTopic : Search, otherTopics : empty }
      , published : unsafeDate 2017 October 19 -- Date of conference
      , description : "Improved memory-efficient optimal search algorithm"
      , documentUrl : URL files.papers."sma-star-plus".url
      , url : Just $ URL "https://easychair.org/publications/paper/TL2M"
      }
  , Paper
      { name : "A Tutorial on Supervised Learning from the Perspective of Mathematical Optimization"
      , tags : { type_ : Thesis, firstTopic : MachineLearning, otherTopics : singleton Optimization }
      , published : unsafeDate 2018 May 22 -- Date of thesis defense
      , description : "Master supervised machine learning at a fundamental level"
      , documentUrl : URL files.papers."supervised-learning-as-mathematical-optimization-tutorial".url
      , url : Nothing
      }
  , Paper
      { name : "Infinite Lattice Learner: an ensemble for incremental learning"
      , tags : { type_ : Journal, firstTopic : MachineLearning, otherTopics : singleton IncrementalLearning }
      , published : unsafeDate 2019 September 6 -- Date published
      , description : "Real-time incremental supervised learning ensemble"
      , documentUrl : URL files.papers."infinite-lattice-learner".url
      , url : Just $ URL "https://link.springer.com/article/10.1007/s00500-019-04330-7"
      }
  ]

name ∷ Paper → String
name (Paper p) = p.name

type_ ∷ Paper → Tag
type_ (Paper p) = (toTag p.tags.type_)

topics ∷ Paper → HashSet Tag
topics (Paper p) = insert (toTag p.tags.firstTopic) (HS.map toTag p.tags.otherTopics)

published ∷ Paper → Date
published (Paper p) = p.published

description ∷ Paper → String
description (Paper p) = p.description

documentUrl ∷ Paper → URL
documentUrl (Paper p) = p.documentUrl

url ∷ Paper → Maybe URL
url (Paper p) = p.url
