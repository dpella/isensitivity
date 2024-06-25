
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult where

import Dataset.Adult.Age
import Dataset.Adult.Workclass
import Dataset.Adult.Race
import Dataset.Adult.Sex
import Dataset.Adult.HoursPerWeek
import Dataset.Adult.NativeCountry

data Adult = Adult
    { ssn           :: !Int
    , age           :: !Age
    , workclass     :: !Workclass
    , fnlwgt        :: !Int
    , education     :: !String        -- TODO: Enum, check csv.
    , educationNum  :: !Int           -- TODO: Enum, check csv.
    , maritalStatus :: !String        -- TODO: Enum, check csv.
    , occupation    :: !String
    , relationship  :: !String        -- TODO: Enum, check csv.
    , race          :: !Race
    , sex           :: !Sex
    , captialGain   :: !Int
    , capitalLoss   :: !Int
    , hoursPerWeek  :: !HoursPerWeek
    , nativeCountry :: !NativeCountry -- TODO: Enum, check csv.
    , salarys       :: !String        -- TODO: Enum, check csv.
    }
  deriving (Eq, Ord, Show)
