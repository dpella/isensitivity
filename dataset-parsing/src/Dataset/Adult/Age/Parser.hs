
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.Age.Parser where

import Dataset.Adult.Age

import Data.Csv (Field, FromField (..), Parser)
import Control.Applicative (pure)
import Data.Monoid (mempty)
import Prelude (Maybe (..), toEnum)
import Text.Read (readMaybe)

instance FromField Age where
    parseField :: Field -> Parser Age
    parseField f = do
      fs <- parseField f
      case readMaybe fs of
        Just i  -> pure (toEnum i)
        Nothing -> mempty
