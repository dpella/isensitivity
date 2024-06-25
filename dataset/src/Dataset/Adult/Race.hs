
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.Race where

import Prelude (Eq, Ord, Enum, Bounded, Show)

data Race = White | Black | Asian | Eskimo | Other
  deriving (Eq, Ord, Enum, Bounded, Show)
