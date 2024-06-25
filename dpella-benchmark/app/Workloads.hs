
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

-- | Queries on `adult.csv`

module Workloads where

import Prelude
import Util
import CompressedAge
import DPella.Algorithm.MWEM
import Data.Bifunctor (first, second)
import Data.Foldable (fold)

-- * import data model for `adult` dataset.
import Dataset.Adult
import Dataset.Adult.Age
import Dataset.Adult.Sex
import Dataset.Adult.Race
--import Dataset.Adult.HoursPerWeek
import Dataset.Adult.Workclass

-- * import underscore constructors.
import Dataset.Adult.Age.TH
import Dataset.Adult.Sex.TH
import Dataset.Adult.Race.TH
--import Dataset.Adult.HoursPerWeek.TH
import Dataset.Adult.Workclass.TH

-- Note: If someone where to calculate the sensitivity of this query using the
-- traditional version of the trick, one would need to evaluate the query on
-- D(Sex) * D(Race) * D(Workclass) * D(HoursPerWeek) inputs. In our case, that
-- would be equal to evaluating the query 9000 times, instead of just 2^4 times
-- (each element in the tuple contributes with two functions: One on the
-- explicit pattern, and one wildcard. See definition of intervalSem.). This
-- complexity of course grows linearly in the number of queries.
-- whiteMalesWorkingFortyHoursInPrivate :: QueryT (Sex, Race, Workclass, Age)
-- whiteMalesWorkingFortyHoursInPrivate = fourAttr' $ \case
--   (Male_, White_, Private_, Forty_) -> lit 1.0
--   _ -> lit 0.0

whiteMalesWorkingThirtyHoursInPrivate :: QueryT (Sex, Race, Workclass, Age)
whiteMalesWorkingThirtyHoursInPrivate = fourAttr' $ \case
  (Female_, White_, Private_, Zero_) -> lit 1.0
  _ -> lit 0

-- | Generate counting queries (+ complement) for every combination of attributes
queries :: (Enum a, Enum b, Bounded a, Bounded b, Eq a, Eq b) => [(a, b) -> Double]
queries = [(\x -> if x == attr then 1.0 else 0.0) | attr <- universe2 ] ++
          [(\x -> if x /= attr then 1.0 else 0.0) | attr <- universe2 ]

demoWorkload :: Workload (Sex, Race, Workclass, CompressedAge)
demoWorkload = Workload (concat
  [ [ f . a2mfa  | f <- queries :: [(Sex, CompressedAge)       -> Double] ]
  , [ f . a2mfwc | f <- queries :: [(Sex, Workclass)           -> Double] ]
  , [ f . a2awc  | f <- queries :: [(CompressedAge, Workclass) -> Double] ]
  , [ f . a2ar   | f <- queries :: [(CompressedAge, Race)      -> Double] ]
  , [ f . a2mfr  | f <- queries :: [(Sex, Race)                -> Double] ]
  , [ f . a2wcr  | f <- queries :: [(Workclass, Race)          -> Double] ]
  ]) 1.0

a2mfa :: (Sex, Race, Workclass, CompressedAge) -> (Sex, CompressedAge)
a2mfa = (\(s,_,_,a) -> (s, a))

a2mfwc :: (Sex, Race, Workclass, CompressedAge) -> (Sex, Workclass)
a2mfwc = (\(s,_,wc,_) -> (s, wc))

a2awc :: (Sex, Race, Workclass, CompressedAge) -> (CompressedAge, Workclass)
a2awc = (\(_,_,wc,a) -> (a, wc))

a2ar :: (Sex, Race, Workclass, CompressedAge) -> (CompressedAge, Race)
a2ar = (\(_,r,_,a) -> (a, r))

a2mfr :: (Sex, Race, Workclass, CompressedAge) -> (Sex, Race)
a2mfr = (\(s,r,_,_) -> (s, r))

a2wcr :: (Sex, Race, Workclass, CompressedAge) -> (Workclass, Race)
a2wcr = (\(_,r,wc,_) -> (wc, r))

fiftyWorkload :: Workload (Sex, Race, Workclass, CompressedAge)
fiftyWorkload = Workload (getWorkload fiftyWorkload') 1.0

-- * 1. Write 50 queries

fiftyWorkload' :: Workload (Sex, Race, Workclass, CompressedAge)
fiftyWorkload' = fold $
     [ uncurry Workload . first pure $ first (\f -> f . a2mfa  ) (rangeComputation f) | f <- saQueries  ] -- :: Workload (Sex, Race, Workclass, CompressedAge)
  <> [ uncurry Workload . first pure $ first (\f -> f . a2mfwc ) (rangeComputation f) | f <- swcQueries ]
  <> [ uncurry Workload . first pure $ first (\f -> f . a2mfr  ) (rangeComputation f) | f <- srQueries  ]
  <> [ uncurry Workload . first pure $ first (\f -> f . a2awc  ) (rangeComputation f) | f <- awcQueries ]
  <> [ uncurry Workload . first pure $ first (\f -> f . a2ar   ) (rangeComputation f) | f <- arQueries  ]
  <> [ uncurry Workload . first pure $ first (\f -> f . a2wcr  ) (rangeComputation f) | f <- wcrQueries ]

-- | 4
saQueries = map twoAttr'
  [ (\case
        (Male_, Twenties_)   -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Male_, Fiftys_)     -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Female_, Fortys_)   -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Female_, Thirties_) -> lit 1.0
        _                    -> lit 0.0)
  ]

-- | 4
swcQueries = map twoAttr'
  [ (\case
        (Male_, LocalGov_)    -> lit 1.0
        _                     -> lit 0.0)
  , (\case
        (Female_, LocalGov_)  -> lit 1.0
        _                     -> lit 0.0)
  , (\case
        (Male_, SelfEmployed_) -> lit 1.0
        _                     -> lit 0.0)
  , (\case
        (Female_, StateGov_)  -> lit 1.0
        _                     -> lit 0.0)
  ]

-- | 2
srQueries = map twoAttr'
  [ (\case
        (Male_, Black_)   -> lit 1.0
        _                 -> lit 0.0)
  , (\case
        (Female_, White_) -> lit 1.0
        _                 -> lit 0.0)
  ]

-- | 20
awcQueries = map twoAttr'
  [ (\case
        (Tens_, Private_)                    -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Twenties_,Private_ )                -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Thirties_,Private_ )                -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Fortys_, Private_)                  -> lit 1.0
        _                                    -> lit 0.0)
  , (\case                      -- 5
        (Fiftys_,Private_ )                  -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Sixtys_, Private_)                  -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Tens_, SelfEmployed_)               -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Thirties_, SelfEmployed_)           -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Fiftys_, SelfEmployed_)             -> lit 1.0
        _                                    -> lit 0.0)
  , (\case                     -- 10
        (Seventees_, SelfEmployed_)          -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Tens_, FederalGov_)                 -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Twenties_,LocalGov_ )               -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Thirties_,StateGov_ )               -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Fortys_, WithoutPay_)               -> lit 1.0
        _                                    -> lit 0.0)
  , (\case                      -- 15
        (Fiftys_,Unknown_ )                  -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Sixtys_, SelfEmployedIncorporated_) -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Tens_, WithoutPay_)                 -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Twenties_, StateGov_)               -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Fiftys_, Unknown_)                  -> lit 1.0
        _                                    -> lit 0.0)
  , (\case                     -- 20
        (Seventees_, FederalGov_)            -> lit 1.0
        _                                    -> lit 0.0)
  ]

-- | 10
arQueries = map twoAttr'
  [ (\case
        (Tens_, White_)      -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Twenties_,Black_ )  -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Thirties_,Eskimo_ ) -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Fortys_, Asian_)    -> lit 1.0
        _                    -> lit 0.0)
  , (\case                      -- 5
        (Fiftys_,Other_ )    -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Sixtys_, White_)    -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Seventees_, Black_) -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Eighties_, Eskimo_) -> lit 1.0
        _                    -> lit 0.0)
  , (\case
        (Nineties_, Asian_)  -> lit 1.0
        _                    -> lit 0.0)
  , (\case                     -- 10
        (Tens_, Other_)      -> lit 1.0
        _                    -> lit 0.0)
  ]


-- | 10
wcrQueries = map twoAttr'
  [ (\case
        (Private_, White_)                   -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (SelfEmployed_,Black_ )              -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (SelfEmployedIncorporated_,Eskimo_ ) -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (LocalGov_, Asian_)                  -> lit 1.0
        _                                    -> lit 0.0)
  , (\case                      -- 5
        (FederalGov_,Other_ )                -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (StateGov_, White_)                  -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (WithoutPay_, Black_)                -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Unknown_, Eskimo_)                  -> lit 1.0
        _                                    -> lit 0.0)
  , (\case
        (Private_, Asian_)                   -> lit 1.0
        _                                    -> lit 0.0)
  , (\case                     -- 10
        (Never_, Other_)                     -> lit 1.0
        _                                    -> lit 0.0)
  ]
