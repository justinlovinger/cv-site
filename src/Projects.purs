module Projects (Project, projects) where

import Prelude

import Data.Date (Date, Month(December,February,July,June,October), exactDate)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(Just,Nothing), fromJust)
import Partial.Unsafe (unsafePartial)
import Web.HTML.History (URL(URL))

type Project =
  { date ∷ Date
  , description ∷ String -- No punctuation
  , longDescription ∷ Maybe String
  , name ∷ String
  , teamRole ∷ Maybe String
  , url ∷ Maybe URL
  }

-- Note: dates are constructed
-- with an unsafe `fromJust`
-- because these dates are manually written
-- and will not change.
projects ∷ Array Project 
projects =
  [ { date : unsafePartial fromJust $ exactDate (unsafePartial fromJust $ toEnum 2014) October (unsafePartial fromJust $ toEnum 20) -- Date of last commit
    , description : "Traditional roguelike game with unique victory condition"
    , longDescription : Nothing
    , name: "Labyrinth RL"
    , teamRole : Nothing
    , url : Just $ URL "/file/labyrinth-rl.zip"
    }
  , { date : unsafePartial fromJust $ exactDate (unsafePartial fromJust $ toEnum 2014) December (unsafePartial fromJust $ toEnum 10) -- Date of last modified file
    , description : "Management game with physics driven combat"
    , longDescription : Nothing
    , name : "Gladiator Manager"
    , teamRole : Nothing
    , url : Just $ URL "/file/gladiator-manager.zip"
    }
  , { date : unsafePartial fromJust $ exactDate (unsafePartial fromJust $ toEnum 2016) February (unsafePartial fromJust $ toEnum 12) -- Date of first App Engine version
    , description : "Website to automatically summarize text"
    , longDescription : Nothing
    , name : "Gist"
    , teamRole : Just "Team Lead and AI"
    , url : Just $ URL "https://gist.justinlovinger.com/"
    }
  , { date : unsafePartial fromJust $ exactDate (unsafePartial fromJust $ toEnum 2016) July (unsafePartial fromJust $ toEnum 26) -- Date of v1.0.0 in Clever Surveys repo
    , description : "Website with predictive machine learning trained by survey responses"
    , longDescription : Just "Website with predictive machine learning trained by survey responses. Users can answer surveys and receive predictions. Users can create surveys with built-in survey builder."
    , name : "Clever Surveys"
    , teamRole : Nothing
    , url : Just $ URL "https://cleversurveys.com/"
    }
  , { date : unsafePartial fromJust $ exactDate (unsafePartial fromJust $ toEnum 2018) June (unsafePartial fromJust $ toEnum 5) -- Date of first App Engine version
    , description : "Progressive web app for reading pdf ebooks"
    , longDescription : Nothing
    , name : "Reader"
    , teamRole : Nothing
    , url : Just $ URL "https://reader.justinlovinger.com/"
    }
  ]
