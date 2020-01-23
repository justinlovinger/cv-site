module Papers (Paper, PaperType, papers) where

import Prelude

import Data.Date (Date, Month(December,February,July,May,November,October,September))
import Data.Maybe (Maybe(Just,Nothing))
import Data.Date.Unsafe (unsafeDate)
import Generated.Files (files)
import Web.HTML.History (URL(URL))

type Paper =
  { date ∷ Date
  , description ∷ String -- No punctuation
  , documentUrl ∷ URL
  , name ∷ String
  , type ∷ PaperType
  , url ∷ Maybe URL
  }

data PaperType = Conference | Journal | Thesis

-- Note: dates are constructed
-- with an unsafe `unsafeDate`
-- because these dates are manually written
-- and will not change.
papers ∷ Array Paper 
papers =
  [ { date : unsafeDate 2014 November 3 -- Date of conference
    , description : "Automatic optimization of genetic algorithm hyperparameters"
    , documentUrl : URL files.papers."harnessing-mother-nature".url
    , name : "Harnessing Mother Nature: Optimizing Genetic Algorithms for Adaptive Systems"
    , type : Conference
    , url : Just $ URL "https://www.sciencedirect.com/science/article/pii/S1877050914012824"
    }
  , { date : unsafeDate 2015 December 9 -- Date of conference
    , description : "Data mine social media to improve smartphone keyboard predictive text"
    , documentUrl : URL files.papers."scrubbing-the-web-for-association-rules".url
    , name : "Scrubbing the web for association rules: An application in predictive text"
    , type : Conference
    , url : Just $ URL "https://ieeexplore.ieee.org/abstract/document/7424353"
    }
  , { date : unsafeDate 2016 July 24 -- Date of conference
    , description : "Real-time incremental supervised learning model"
    , documentUrl : URL files.papers."neural-field".url
    , name : "Neural Field: Supervised apportioned incremental learning (SAIL)"
    , type : Conference
    , url : Just $ URL "https://ieeexplore.ieee.org/abstract/document/7727510"
    }
  , { date : unsafeDate 2017 February 16 -- Date published
    , description : "Examine the difficulty of supervised learning with data directly reported by people"
    , documentUrl : URL files.papers."the-effect-of-human-thought-on-data".url
    , name : "The effect of human thought on data: an analysis of self-reported data in supervised learning and neural networks"
    , type : Journal
    , url : Just $ URL "https://link.springer.com/article/10.1007/s13748-017-0118-4"
    }
  , { date : unsafeDate 2017 October 10 -- Date published. Issued much later.
    , description : "Automatically summarize text and reviews"
    , documentUrl : URL files.papers.gist.url
    , name : "Gist: general integrated summarization of text and reviews"
    , type : Journal
    , url : Just $ URL "https://link.springer.com/article/10.1007/s00500-017-2882-2"
    }
  , { date : unsafeDate 2017 October 19 -- Date of conference
    , description : "Improved memory-efficient optimal search algorithm"
    , documentUrl : URL files.papers."sma-star-plus".url
    , name : "Enhanced Simplified Memory-bounded A Star (SMA*+)"
    , type : Conference
    , url : Just $ URL "https://easychair.org/publications/paper/TL2M"
    }
  , { date : unsafeDate 2018 May 22 -- Date of thesis defense
    , description : "Master supervised machine learning at a fundamental level"
    , documentUrl : URL files.papers."supervised-learning-as-mathematical-optimization-tutorial".url
    , name : "A Tutorial on Supervised Learning from the Perspective of Mathematical Optimization"
    , type : Thesis
    , url : Nothing
    }
  , { date : unsafeDate 2019 September 6 -- Date published
    , description : "Real-time incremental supervised learning ensemble"
    , documentUrl : URL files.papers."infinite-lattice-learner".url
    , name : "Infinite Lattice Learner: an ensemble for incremental learning"
    , type : Journal
    , url : Just $ URL "https://link.springer.com/article/10.1007/s00500-019-04330-7"
    }
  ]
