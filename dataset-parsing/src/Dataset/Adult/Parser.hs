
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

-- | A simple csv parser for the 'adult' data set.

module Dataset.Adult.Parser where

import Prelude
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

import Dataset.Adult
import Dataset.Adult.Age.Parser
import Dataset.Adult.Workclass.Parser
import Dataset.Adult.Race.Parser
import Dataset.Adult.Sex.Parser
import Dataset.Adult.HoursPerWeek.Parser
import Dataset.Adult.NativeCountry.Parser

-- * Define parser for the Adult data model.

instance FromNamedRecord Adult where
    parseNamedRecord r = Adult
      <$> r .: "SSN"
      <*> r .: "Age"
      <*> r .: "workclass"
      <*> r .: "fnlwgt"
      <*> r .: "Education"
      <*> r .: "education-num"
      <*> r .: "marital-status"
      <*> r .: "Occupation"
      <*> r .: "relationship"
      <*> r .: "race"
      <*> r .: "sex"
      <*> r .: "capital-gain"
      <*> r .: "capital-loss"
      <*> r .: "hours-per-week"
      <*> r .: "native-country"
      <*> r .: "Salarys"

-- * Functions for parsing Adult data from csv files.

parseAdultsV :: String -> IO (Either String (V.Vector Adult))
parseAdultsV file = do
  csvData <- BL.readFile file
  pure $ snd <$> (decodeByName csvData)

parseAdultsV' :: String -> IO (V.Vector Adult)
parseAdultsV' file = do
  adults <- parseAdultsV file
  pure $ either error id adults

-- * List variants of the above

parseAdults :: String -> IO (Either String [Adult])
parseAdults = parseAdultsV >=> (pure . fmap V.toList)

parseAdult' :: String -> IO [Adult]
parseAdult' = parseAdultsV' >=> (pure . V.toList)
