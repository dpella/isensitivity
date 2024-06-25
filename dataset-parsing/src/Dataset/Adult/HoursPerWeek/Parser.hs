
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.HoursPerWeek.Parser where

import Dataset.Adult.HoursPerWeek

import Data.Csv (Field, FromField (..), Parser)
import Control.Applicative (pure)
import Data.Monoid (mempty)
import Prelude (Maybe (..), toEnum)
import Text.Read (readMaybe)

instance FromField HoursPerWeek where
    parseField :: Field -> Parser HoursPerWeek
    parseField f = do
      fs <- parseField f
      case readMaybe fs of
        Just i  -> pure (toEnum i)
        Nothing -> mempty
