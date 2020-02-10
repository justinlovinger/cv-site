module Projects (Project, projects) where

import Prelude

import Data.Date (Date, Month(December,February,July,June,March,October))
import Data.Date.Unsafe (unsafeDate)
import Data.Maybe (Maybe(Just,Nothing))
import Generated.Files (files)
import Web.HTML.History (URL(URL))

type Project =
  { name ∷ String
  , published ∷ Date
  , updated ∷ Date
  , description ∷ String -- No punctuation
  , longDescription ∷ Maybe String
  , teamRole ∷ Maybe String
  , url ∷ Maybe URL
  }

-- Note: dates are constructed
-- with an unsafe `unsafeDate`
-- because these dates are manually written
-- and will not change.
projects ∷ Array Project 
projects =
  [ { name : "Labyrinth RL"
    , published : unsafeDate 2014 October 20 -- Date of last commit
    , updated : unsafeDate 2014 October 20 -- Never updated
    , description : "Traditional roguelike game with unique victory condition"
    , longDescription : Nothing
    , teamRole : Nothing
    , url : Just $ URL files.projects."labyrinth-rl".url
    }
  , { name : "Gladiator Manager"
    , published : unsafeDate 2014 December 10 -- Date of last modified file
    , updated : unsafeDate 2014 December 10 -- Never updated
    , description : "Management game with physics driven combat"
    , longDescription : Nothing
    , teamRole : Nothing
    , url : Just $ URL files.projects."gladiator-manager".url
    }
  , { name : "Gist"
    , published : unsafeDate 2016 February 12 -- Date of first App Engine version
    , updated : unsafeDate 2017 March 27 -- Date of second to last commit. Last notes that change was made earlier, but not committed.
    , description : "Website to automatically summarize text"
    , longDescription : Nothing
    , teamRole : Just "Team Lead and AI"
    , url : Just $ URL "https://gist.justinlovinger.com/"
    }
  , { name : "Clever Surveys"
    , published : unsafeDate 2016 July 26 -- Date of v1.0.0 in Clever Surveys repo
    , updated : unsafeDate 2017 March 7 -- Date of last non-hotfix patch (v1.1.0)
    , description : "Website with predictive machine learning trained by survey responses"
    , longDescription : Just "Website with predictive machine learning trained by survey responses. Users can answer surveys and receive predictions. Users can create surveys with built-in survey builder."
    , teamRole : Nothing
    , url : Just $ URL "https://cleversurveys.com/"
    }
  , { name : "Reader"
    , published : unsafeDate 2018 June 5 -- Date of first App Engine version
    , updated : unsafeDate 2018 June 9 -- Date of last tagged version (v0.2.0)
    , description : "Progressive web app for reading pdf ebooks"
    , longDescription : Nothing
    , teamRole : Nothing
    , url : Just $ URL "https://reader.justinlovinger.com/"
    }
  ]
