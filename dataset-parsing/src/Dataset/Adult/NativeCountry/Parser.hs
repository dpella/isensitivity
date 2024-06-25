
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.NativeCountry.Parser where

import Dataset.Adult.NativeCountry

import Data.Csv (Field, FromField (..), Parser)
import Control.Applicative (pure)
import Data.Monoid (mempty)

instance FromField NativeCountry where
    parseField :: Field -> Parser NativeCountry
    parseField = \case
      "United-States"              -> pure USA
      "Mexico"                     -> pure Mexico
      "Puerto-Rico"                -> pure PuertoRico
      "Cuba"                       -> pure Cuba
      "Jamaica"                    -> pure Jamaica
      "India"                      -> pure India
      "South"                      -> pure South
      "Honduras"                   -> pure Honduras
      "England"                    -> pure England
      "Canada"                     -> pure Canada
      "Germany"                    -> pure Germany
      "Iran"                       -> pure Iran
      "Philippines"                -> pure Philippines
      "Italy"                      -> pure Italy
      "Poland"                     -> pure Poland
      "Columbia"                   -> pure Columbia
      "Cambodia"                   -> pure Cambodia
      "Thailand"                   -> pure Thailand
      "Ecuador"                    -> pure Ecuador
      "Laos"                       -> pure Laos
      "Taiwan"                     -> pure Taiwan
      "Haiti"                      -> pure Haiti
      "Portugal"                   -> pure Portugal
      "Dominican-Republic"         -> pure DominicanRepublic
      "El-Salvador"                -> pure ElSalvador
      "France"                     -> pure France
      "Guatemala"                  -> pure Guatemala
      "China"                      -> pure China
      "Japan"                      -> pure Japan
      "Yugoslavia"                 -> pure Yugoslavia
      "Peru"                       -> pure Peru
      "Outlying-US(Guam-USVI-etc)" -> pure OutlyingUS
      "Scotland"                   -> pure Scotland
      "Trinadad&Tobago"            -> pure TrinadadTobago
      "Greece"                     -> pure Greece
      "Nicaragua"                  -> pure Nicaragua
      "Vietnam"                    -> pure Vietnam
      "Hong"                       -> pure HongKong
      "Ireland"                    -> pure Ireland
      "Hungary"                    -> pure Hungary
      "Holand-Netherlands"         -> pure Netherlands
      "?"                          -> pure Unknown
      _                            -> mempty
