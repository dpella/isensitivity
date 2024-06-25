
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

-- | Important functions and types.

module Util
  (
  -- * Functions
  createWorkload
  , universe1, universe2, universe3, universe4
  , rangeComputation
  -- * Types
  , QueryT
  -- * Re-exports
  , Workload
  , getWorkload
  , getSensitivity
  , mwem
  , lit
  , oneAttr
  , twoAttr'
  , fourAttr'
  ) where

import Prelude
import Data.Bifunctor (first)
import DSL.Deep

import DPella.Algorithm.MWEM ( mwem , Workload (..), Epsilon)

type QueryT row = Query row Double
type Weights row = [(row,Double)]
type Distribution column = [column]
type Universe t = [t]

-- | Build a workload
mkLoad :: QueryT row -> Workload row
mkLoad = uncurry Workload . first pure . rangeComputation

-- | Combine workloads
createWorkload :: [QueryT row] -> Workload row
createWorkload = foldMap mkLoad

-- | Extract the underlying function and the maximum sensitivity from a query.
rangeComputation :: QueryT row -> (row -> Double, Double)
rangeComputation = (,) <$> runQuery <*> rangeQuery

-- * Functions for creating queries
twoAttr' f = twoAttr (curry f) 

-- >>> :t query2'

fourAttr' f = fourAttr (curry4 f)

-- * Auxillary functions

curry4 :: ((a,b,c,d) -> r) -> (a -> b -> c -> d -> r)
curry4 f = \a -> \b -> \c -> \d -> f (a,b,c,d)

universe1 :: (Enum a, Bounded a) => [a]
universe1 = enumFrom minBound

universe2 :: (Enum a, Enum b, Bounded a, Bounded b) => [(a, b)]
universe2 = (,) <$> universe1 <*> universe1

universe3 :: (Enum a, Enum b, Enum c , Bounded a, Bounded b, Bounded c) => [(a, b, c)]
universe3 = (,,) <$> universe1 <*> universe1 <*> universe1

universe4 :: (Enum a, Enum b, Enum c, Enum d , Bounded a, Bounded b, Bounded c, Bounded d) => [(a, b, c, d)]
universe4 = (,,,) <$> universe1 <*> universe1 <*> universe1 <*> universe1

