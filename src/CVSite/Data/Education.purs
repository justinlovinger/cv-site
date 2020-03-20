module CVSite.Data.Education
  ( DegreeType(..)
  , Education
  , accolades
  , degreeProgram
  , degreeType
  , education
  , gpa
  , graduated
  , schoolAddress
  , schoolName
  ) where

import Prelude

import Data.Date (Date, Month(..))
import Data.Date.Unsafe (unsafeDate)
import Data.HashSet (singleton)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Tag (class TagLike, class Tagged, Tag, tag, toTag)

newtype Education = Education
  { school ∷ School
  , degree ∷ { degreeType ∷ DegreeType, program ∷ String }
  , graduated ∷ Date
  , gpa ∷ Number
  , accolades ∷ Maybe (Array String)
  }

instance taggedEducation ∷ Tagged Education where
  tags e = singleton $ degreeType e

type School = { name ∷ String, address ∷ Address }

type Address =
  { street ∷ String
  , district ∷ String
  , state ∷ String
  , country ∷ String
  , postalCode ∷ String
  }

data DegreeType = PhD | Masters | Bachelors
instance tagLikeDegreeType ∷ TagLike DegreeType where toTag = tag <<< show
instance hashableDegreeType ∷ Hashable DegreeType where hash = hash <<< show
derive instance eqDegreeType ∷ Eq DegreeType
instance showDegreeType ∷ Show DegreeType where
  show PhD = "PhD"
  show Masters = "masters"
  show Bachelors = "bachelors"

education ∷ Array Education
education =
  [ Education
      { school : umassd
      , degree : { degreeType : Bachelors, program : "computer science" }
      , graduated : unsafeDate 2015 May 15 -- Date of graduation ceremony
      , gpa : 3.7 -- GPA for major
      , accolades : Nothing
      }
  , Education
      { school : umassd
      , degree : { degreeType : Masters, program : "computer science" }
      , graduated : unsafeDate 2018 May 12 -- Date of graduation ceremony
      , gpa : 4.0
      , accolades : Just
          [ "Graduate research award"
          , "Graduate faculty award"
          ]
      }
  ]

umassd ∷ School
umassd =
  { name : "UMass Dartmouth"
  , address :
    { street : "285 Old Westport Road"
    , district : "Dartmouth"
    , state : "MA"
    , country : "USA"
    , postalCode : "02747-2300"
    }
  }

schoolName ∷ Education → String
schoolName (Education e) = e.school.name

schoolAddress ∷ Education → Address
schoolAddress (Education e) = e.school.address

degreeType ∷ Education → Tag
degreeType (Education e) = toTag e.degree.degreeType

degreeProgram ∷ Education → String
degreeProgram (Education e) = e.degree.program

graduated ∷ Education → Date
graduated (Education e) = e.graduated

gpa ∷ Education → Number
gpa (Education e) = e.gpa

accolades ∷ Education → Maybe (Array String)
accolades (Education e) = e.accolades
