
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module DPella.Algorithm.Weights where

import Data.List
import Control.Arrow ((&&&))

-- import DPella.Imports

import Prelude

----------------------------------------
-- Dataset as list of weights
----------------------------------------

type Weights row = [(row, Double)]

rowsToWeights :: Ord row => [row] -> Weights row
rowsToWeights = fmap (head &&& (fromIntegral . length)) . group . sort

----------------------------------------
-- Multiplicative weights
----------------------------------------

type Eta = Double
type Linear row = row -> Double

uniformWeights :: Ord row => [row] -> Weights row
uniformWeights univ = [ let weight = 1 / fromIntegral (length univ) in (row, weight)
                      | row <- univ ]

multiplyWeights :: Eta -> Linear row -> Weights row -> Weights row
multiplyWeights eta linear weights =
  [ (row, w * exp (eta * linear row))
  | (row, w) <- weights ]

sumWeights :: Weights row -> Double
sumWeights weights = sum (snd <$> weights)

(.+.) :: Weights row -> Weights row -> Weights row
(.+.) ws1 ws2 = [ (row, w1 + w2) | ((row, w1), (_, w2)) <- zip ws1 ws2 ]

scalar :: Double -> Weights row -> Weights row
scalar n weights = [ (row, w * n) | (row, w) <- weights ]

liftLinear :: Linear row -> Weights row -> Double
liftLinear q ws = sum [ q d * w | (d, w) <- ws ]
