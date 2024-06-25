
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module DPella.Algorithm.MWEM
  ( theoreticalError
  , mwem
  , Epsilon
  , Iterations
  , Workload (..)
  ) where

import Data.List
import Control.Monad.Reader

-- import DPella.Imports
import Prelude
import DPella.Noise
import DPella.Algorithm.Weights

import Numeric.Natural (Natural)

import Statistics.Distribution.Laplace (laplace)
import qualified Numeric.Probability.Distribution as Dist

type Epsilon = Double
type Iterations = Natural

data Workload row = Workload
  { getWorkload    :: [row -> Double]
  , getSensitivity :: Sensitivity
  }

instance Semigroup (Workload row) where
  (Workload w1 s1) <> (Workload w2 s2) =
    Workload { getWorkload    = w1 <> w2
             , getSensitivity = (max s1 s2)
             }

instance Monoid (Workload row) where
  mempty = Workload [] 0

----------------------------------------
-- Theoretical error
----------------------------------------

theoreticalError :: Epsilon
                 -> Iterations
                 -> Workload row
                 -> [row]  -- Dataset rows
                 -> [row]  -- Dataset universe
                 -> Double
theoreticalError eps iter ws rows univ =
  2 * fromIntegral (length rows)
    * sqrt (log (fromIntegral (length univ)) / fromIntegral iter)
  + 10 * fromIntegral iter
       * log (fromIntegral (length (getWorkload ws)))
       / eps

----------------------------------------
-- Exponential mechanism
----------------------------------------

type Score = Double
type Sensitivity = Double

expMechanism :: (MonadNoise m, Show response, Ord response)
             => Epsilon
             -> Sensitivity
             -> dataset -- Weights row
             -> [response]
             -> (dataset -> response -> Score) -- (Weights row -> resp -> Score)
             -> m response
expMechanism eps sens weights resps scoring = do
  let scores   = [ scoring weights resp | resp  <- resps ]
  let probs    = [ exp ((eps * score) / (2 * sens)) | score <- scores ]
  let expDistr = Dist.fromList (zip resps probs)
  let err      = error "expMechanism: could not sample"
  maybe err id <$> Dist.sample expDistr

----------------------------------------
-- MWEM
----------------------------------------

data MWEMEnv row =
  MWEMEnv
  -- Input dataset
  { dataset     :: Weights row             -- ^ The original dataset
  , size        :: Natural                 -- ^ The size of the dataset
  -- Workload
  , numQueries  :: Natural                 -- ^ Size of the workload
  , queriesR    :: Workload row
  , queriesW    :: [Weights row -> Double] -- ^ Derived workload over weights
  , wlOnDataset :: [Double]                -- ^ Workload applied to the input dataset
  -- DP parameters
  , epsExpM     :: Double                  -- ^ Epsilon used for the exponential mechanism
  , lapScale    :: Double                  -- ^ Laplace scale
  , noiseGen    :: NoiseGen                -- ^ Cprypto noise generator
  }

-- Lift a noise generating computation to the top-level reader monad
withNoiseGen :: ReaderT NoiseGen m a -> ReaderT (MWEMEnv row) m a
withNoiseGen = withReaderT noiseGen

mkMWEMEnv :: Ord row => Epsilon -> Iterations -> [row] -> Workload row -> IO (MWEMEnv row)
mkMWEMEnv eps iter rows wl = do
  gen <- newNoiseGen
  let qs = getWorkload wl
  let dataset'  = rowsToWeights rows
  let queriesW' = fmap (\q -> \ws -> sum [ q d * w | (d, w) <- ws ]) qs
  return (MWEMEnv
          { dataset     = dataset'
          , size        = fromIntegral (length rows)
          , numQueries  = fromIntegral (length qs)
          , queriesR    = wl
          , queriesW    = queriesW'
          , wlOnDataset = [ query dataset' | query <- queriesW' ]
          , epsExpM     = eps / (2 * fromIntegral (iter + 1))
          , lapScale    = 2 * fromIntegral (iter + 1) / eps
          , noiseGen    = gen
          })

mwem :: Ord row
     => Epsilon
     -> Iterations
     -> Workload row
     -> [row]           -- ^ Dataset rows
     -> [row]           -- ^ Dataset universe
     -> IO (Weights row, Weights row)  -- (last step, avg steps)
mwem eps iter workload rows univ = do
  let weights = uniformWeights univ
  env <- mkMWEMEnv eps iter rows workload
  flip runReaderT env $ do
    (lastSynthetic, sumSynthetic) <- loop stepMWEM (.+.) weights iter
    let avgSynthetic = scalar (1 / fromIntegral (iter + 1)) sumSynthetic
    --                                                ^ (+1) because of the
    --                                                extra initial `weights`
    --                                                after `iter` iterations
    return (lastSynthetic, avgSynthetic)

loop :: Monad m => (a -> m a) -> (a -> a -> a) -> a -> Iterations -> m (a, a)
loop step f a = loop' a a
  where
    loop' x acc 0 = return (x, acc)
    loop' x acc n = step x >>= \x' -> loop' x' (f x' acc) (n-1)

stepMWEM :: Weights row -> ReaderT (MWEMEnv row) IO (Weights row)
stepMWEM synthetic = do
  idx <- expM
  m <- laplaceM idx
  update idx m

  where
    -- Exponential mechanism
    expM = do
      budget  <- asks epsExpM
      ds      <- asks dataset
      qs      <- asks queriesW
      maxIdxQ <- asks (subtract 1 . numQueries)
      qsOnDs  <- asks wlOnDataset
      sens    <- asks (getSensitivity . queriesR) -- List of corresponding sensitivities

      idx <- withNoiseGen $ do
               expMechanism
                 budget
                 sens
                 ds
                 [ 0 .. fromIntegral maxIdxQ ]
                 (\_ idxQ -> abs ((qs !! idxQ) synthetic - qsOnDs !! idxQ))
                 -- ^ We ignore the input dataset here because the workload has been
                 -- precomputed and store in the environment beforehand.
      return idx

    -- Laplace noise
    laplaceM idx = do
      scale <- asks lapScale
      m     <- asks wlOnDataset
      noise <- withNoiseGen (genCryptoContVar (laplace 0 scale))
      return (m !! idx + noise)

    -- Updating weights
    update idx m = do
      qsW <- asks queriesW
      qsR <- asks (getWorkload . queriesR)
      n   <- asks size
      let eta = (m - (qsW !! idx) synthetic) / (2 * fromIntegral n)
      -- New synthetic data
      let synthetic' = multiplyWeights eta (qsR !! idx) synthetic
      let n'         = sumWeights synthetic'
      let normalized = scalar (1 / n') synthetic'
      return normalized
