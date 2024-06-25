
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE TemplateHaskell #-}
-- | Split Dataset.Adult.Age into bins of 10.

module CompressedAge where

import Prelude
import DSL.Deep

import Dataset.Adult.Age

data CompressedAge
  = Tens
  | Twenties
  | Thirties
  | Fortys
  | Fiftys
  | Sixtys
  | Seventees
  | Eighties
  | Nineties
  deriving (Enum, Bounded, Eq, Ord)

instance Show CompressedAge where
  show = \case
    Tens      -> "<20"
    Twenties  -> "<30"
    Thirties  -> "<40"
    Fortys    -> "<50"
    Fiftys    -> "<60"
    Sixtys    -> "<70"
    Seventees -> "<80"
    Eighties  -> "<90"
    Nineties  -> "<100"

compress :: Age -> CompressedAge
compress a
  | a' < 20  = Tens
  | a' < 30  = Twenties
  | a' < 40  = Thirties
  | a' < 50  = Fortys
  | a' < 60  = Fiftys
  | a' < 70  = Sixtys
  | a' < 80  = Seventees
  | a' < 90  = Eighties
  | a' < 100 = Nineties
  where a' = (fromEnum a)

$(mkTCBoiler ''CompressedAge)
