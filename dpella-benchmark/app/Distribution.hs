
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

-- | a pipeline for automatically producing results by spitting out
-- the distribution of the 4 relevant parameters in a CSV file

module Distribution (mkDomain, Domain, encodeDomain, parseDomain) where

import Workloads
import Text.Read (readMaybe)

import Prelude
import Util
import CompressedAge
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Dataset.Adult.Age
import Dataset.Adult.Sex
import Dataset.Adult.Race
import Dataset.Adult.HoursPerWeek
import Dataset.Adult.Workclass

import Dataset.Adult.Age.Parser
import Dataset.Adult.Sex.Parser
import Dataset.Adult.Race.Parser
import Dataset.Adult.HoursPerWeek.Parser
import Dataset.Adult.Workclass.Parser

data Domain = Domain
  { query        :: Int
  , distribution :: Double
  }
  deriving Show

instance ToNamedRecord Domain where
  toNamedRecord (Domain query distribution) =
    namedRecord [
    "query"          .= toField query
    , "distribution" .= toField distribution
    ]

instance FromNamedRecord Domain where
    parseNamedRecord r = Domain
      <$> r .: "query"
      <*> r .: "distribution"

parseDomain :: FilePath -> IO [Domain]
parseDomain file = do
  csvData <- decodeByName <$> BL.readFile file
  let domain = snd $ either error id csvData
  pure $ (V.toList domain)

encodeDomain :: [Domain] -> BL.ByteString
encodeDomain = encodeByName domainHeader

domainHeader :: Header
domainHeader = header ["query", "distribution"]
 
-- | Encode the output distribution from MWEM into a CSV
mkDomain :: (Int, Double) -> Domain
mkDomain (query, distribution) =
  Domain query distribution

-- * Auxiallary

instance ToField Sex where
  toField = \case
    Male -> "Male"
    Female -> "Female"

instance ToField Race where
  toField = \case
    White   -> "White"
    Black   -> "Black"
    Asian   -> "Asian-Pac-Islander"
    Eskimo  -> "Amer-Indian-Eskimo"
    Other   -> "Other"

instance ToField Workclass where
  toField = \case
    Private                  -> "Private"
    SelfEmployed             -> "Self-emp-not-inc"
    SelfEmployedIncorporated -> "Self-emp-inc"
    LocalGov                 -> "Local-gov"
    FederalGov               -> "Federal-gov"
    StateGov                 -> "State-gov"
    Never                    -> "Never-worked"
    WithoutPay               -> "Without-pay"
    Unknown                  -> "?"

instance ToField HoursPerWeek where
  toField = toField . fromEnum

instance ToField Age where
  toField = toField . fromEnum

instance ToField CompressedAge where
  toField = toField . show

instance FromField CompressedAge where
  parseField f = do
      fs <- parseField f
      case readMaybe fs of
        Just i  -> pure (compress  ((toEnum i) :: Age))
        Nothing -> mempty
