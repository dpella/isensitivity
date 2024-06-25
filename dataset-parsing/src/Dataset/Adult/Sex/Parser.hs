
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.Sex.Parser where

import Dataset.Adult.Sex

import Data.Csv (Field, FromField (..), Parser)
import Control.Applicative (pure)
import Data.Monoid (mempty)

instance FromField Sex where
    parseField :: Field -> Parser Sex
    parseField = \case
      "Male"   -> pure Male
      "Female" -> pure Female
      _        -> mempty
