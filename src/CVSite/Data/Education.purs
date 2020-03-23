module CVSite.Data.Education
  ( Education
  , accolades
  , degreeProgram
  , degreeType
  , education
  , gpa
  , graduated
  , schoolAddress
  , schoolName
  ) where

import CVSite.Data.Tags (class TagLike, class Tagged, Tag, toTag)
import CVSite.Data.Tags as T
import Data.Date (Date, Month(..))
import Data.Date.Unsafe (unsafeDate)
import Data.HashSet (fromArray)
import Data.Maybe (Maybe(..))

newtype Education = Education
  { school ∷ School
  , degree ∷ { degreeType ∷ DegreeType, program ∷ String }
  , graduated ∷ Date
  , gpa ∷ Number
  , accolades ∷ Maybe (Array String)
  }

instance taggedEducation ∷ Tagged Education where
  tags e = fromArray [ T.Education, degreeType e ]

type School = { name ∷ String, address ∷ Address }

type Address =
  { street ∷ String
  , district ∷ String
  , state ∷ String
  , country ∷ String
  , postalCode ∷ String
  }

data DegreeType = Masters | Bachelors

instance tagLikeDegreeType ∷ TagLike DegreeType where
  toTag Masters = T.Masters
  toTag Bachelors = T.Bachelors

-- Note: unsafe operations are used
-- because these values are manually written
-- and will not change at runtime.
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
