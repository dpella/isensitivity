
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.Workclass.Parser where

import Dataset.Adult.Workclass

import Data.Csv (Field, FromField (..), Parser)
import Control.Applicative (pure)
import Data.Monoid (mempty)

instance FromField Workclass where
    parseField :: Field -> Parser Workclass
    parseField = \case
      "Private"          -> pure Private
      "Self-emp-not-inc" -> pure SelfEmployed
      "Self-emp-inc"     -> pure SelfEmployedIncorporated
      "Local-gov"        -> pure LocalGov
      "Federal-gov"      -> pure FederalGov
      "State-gov"        -> pure StateGov
      "Never-worked"     -> pure Never
      "Without-pay"      -> pure WithoutPay
      "?"                -> pure Unknown
      _                  -> mempty
