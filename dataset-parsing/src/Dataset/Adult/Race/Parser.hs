
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.Race.Parser where

import Dataset.Adult.Race

import Data.Csv (Field, FromField (..), Parser)
import Control.Applicative (pure)
import Data.Monoid (mempty)

instance FromField Race where
    parseField :: Field -> Parser Race
    parseField = \case
      "White"              -> pure White
      "Black"              -> pure Black
      "Asian-Pac-Islander" -> pure Asian
      "Amer-Indian-Eskimo" -> pure Eskimo
      "Other"              -> pure Other
      _                    -> mempty
