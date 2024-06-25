
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.NativeCountry where

import Prelude (Eq, Ord, Enum, Bounded, Show)

data NativeCountry
  = USA
  | Mexico
  | PuertoRico
  | Cuba
  | Jamaica
  | India
  | South
  | Honduras
  | England
  | Canada
  | Germany
  | Iran
  | Philippines
  | Italy
  | Poland
  | Columbia
  | Cambodia
  | Thailand
  | Ecuador
  | Laos
  | Taiwan
  | Haiti
  | Portugal
  | DominicanRepublic
  | ElSalvador
  | France
  | Guatemala
  | China
  | Japan
  | Yugoslavia
  | Peru
  | OutlyingUS
  | Scotland
  | TrinadadTobago
  | Greece
  | Nicaragua
  | Vietnam
  | HongKong
  | Ireland
  | Hungary
  | Netherlands
  | Unknown
  deriving (Eq, Ord, Enum, Bounded, Show)
