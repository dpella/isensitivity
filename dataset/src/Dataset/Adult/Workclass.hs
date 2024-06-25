
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.Workclass where

import Prelude (Eq, Ord, Enum, Bounded, Show)

data Workclass
  = Private
  | SelfEmployed
  | SelfEmployedIncorporated
  | LocalGov
  | FederalGov
  | StateGov
  | WithoutPay
  | Never
  | Unknown
  deriving (Eq, Ord, Enum, Bounded, Show)
