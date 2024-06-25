
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

-- | Test queries on `adult.csv`
module Main where

import Prelude
import qualified Data.Map as M
import Data.List (sort, group)
import Data.Bifunctor (second)

import Util
import Workloads
import Distribution
import CompressedAge
import qualified Data.ByteString.Lazy as BSL

-- * import data model for `adult` dataset.
import Dataset.Adult
import Dataset.Adult.Age
import Dataset.Adult.Sex
import Dataset.Adult.Race
--import Dataset.Adult.HoursPerWeek
import Dataset.Adult.Workclass
-- * import parser for `adult` dataset.
import Dataset.Adult.Parser (parseAdult')

main :: IO ()
main = do
  -- * Parse csv
  adults <- parseAdult' "adult.csv"
  -- * Setup domain and run function
  let rows = map attributes adults
  let produce dataset_name wl eps iter = do
        putStrLn $ "producing " ++ show (eps, iter) ++ " .."
        result <- mwem eps iter wl rows universe4
        let
          scale :: Double -> [(a, Double)] -> [(a, Double)]
          scale s = map (second (*s))
        let last = fst result
        let result = queryResults
              (concat [ replicate (floor freq) element
                      | (element, freq) <- scale (fromIntegral $ (length rows)) last ])
              wl
        let domain = map mkDomain result
        BSL.writeFile ("results/" ++ dataset_name ++ "-" ++ show eps ++ "e-" ++ show iter ++ "i.csv") $ encodeDomain domain

  -- * Produce reference data
  produceReference "reference" (queryResults rows fiftyWorkload)
  -- * Run tests
  -- Run mwem and produce results.
  produce "adult" fiftyWorkload' 0.01 10
  produce "adult" fiftyWorkload' 0.1  10
  produce "adult" fiftyWorkload' 1    10

  -- * Multi results
  -- produce "old/adult" fiftyWorkload 0.01  50
  -- produce "old/adult" fiftyWorkload 0.1   50
  -- produce "old/adult" fiftyWorkload 1     50

  -- produce "new/adult" fiftyWorkload' 0.01  50
  -- produce "new/adult" fiftyWorkload' 0.1   50
  -- produce "new/adult" fiftyWorkload' 1     50

  -- * Done
  putStrLn "ByeBye"

-- * Auxillary functions

attributes :: Adult -> (Sex, Race, Workclass, CompressedAge)
attributes = (,,,) <$> sex <*> race <*> workclass <*> (compress . age)

produceReference :: FilePath -> [(Int, Double)] -> IO ()
produceReference f res = do
  let domain = map mkDomain res
  BSL.writeFile (f ++ ".csv") (encodeDomain domain)
  where
    g :: (Eq a, Ord a) => [a] -> [(a,Int)]
    g s = map (\x -> (head x, length x)) . group . sort $ s

queryResults :: [(Sex, Race, Workclass, CompressedAge)] -> Workload (Sex, Race, Workclass, CompressedAge) -> [(Int, Double)]
queryResults a = M.toList . queryResults' a

queryResults' :: [(Sex, Race, Workclass, CompressedAge)] -> Workload (Sex, Race, Workclass, CompressedAge) -> M.Map Int Double
queryResults' rs qs = iter rs queries M.empty
  where
    queries = zip (getWorkload qs) [1..]
    iter [] qs m = m
    iter (r:rs) qs m = iter rs qs (update r qs m)
    update _ [] m = m
    update r ((q,i):qs) m = update r qs (M.insertWith (+) i (q r) m)
