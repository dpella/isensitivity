
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module DPella.Noise where

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT, get)
-- import Control.Monad ( Monad(return) )
import Control.Monad.IO.Class ( MonadIO(..) ) 
-- import GHC.Types ( Double, IO ) 
-- import GHC.Real ( fromIntegral, Fractional((/)) )  
import Foreign.Storable (peek)

import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Tuple (swap)
import Data.ByteArray (withByteArray, Bytes)
import Data.Bits (shiftR, shiftL)
import Data.Word (Word64)

import Crypto.Random (ChaChaDRG, drgNewSeed, seedNew, randomBytesGenerate)

import Statistics.Distribution (Distribution, quantile, mean, stdDev)
import Statistics.Distribution.Laplace (LaplaceDistribution)
import Statistics.Distribution.Normal (NormalDistribution)
import Statistics.Distribution.Exponential (ExponentialDistribution)

import Prelude 

----------------------------------------
-- Noise generators

newtype NoiseGen = NoiseGen (IORef ChaChaDRG)

newNoiseGen :: Prelude.IO NoiseGen
newNoiseGen = do
  seed <- seedNew
  drgIO <- newIORef (drgNewSeed seed)
  Prelude.return (NoiseGen drgIO)

-- | Create new 'ChaChaDRGIO' using system entropy to seed it.

----------------------------------------
-- The MonadNoise class

class MonadIO m => MonadNoise m where
  getNoiseGen :: m NoiseGen

-- We might need more instances in the future but it is unclear now

instance MonadIO m => MonadNoise (ReaderT NoiseGen m) where
  getNoiseGen = ask

instance MonadIO m => MonadNoise (StateT NoiseGen m) where
  getNoiseGen = get

-- Generating noise

-- Uniform noise in the range [0,1]
uniform :: MonadNoise m => m Prelude.Double
uniform = do
  NoiseGen genRef <- getNoiseGen
  -- generate a bytearray with 8 bytes.
  bytes <- liftIO Prelude.$ atomicModifyIORef' genRef Prelude.$ \gen ->
    swap (randomBytesGenerate 8 gen)
  -- convert it into Word64
  w64 <- liftIO Prelude.$ withByteArray (bytes :: Bytes) peek
  Prelude.return Prelude.$! word64Uniform w64

-- Uniform noise in a given range
uniformInRange :: MonadNoise m => Prelude.Double -> Prelude.Double -> m Prelude.Double
uniformInRange a b = do
  u <- uniform
  Prelude.return (u Prelude.* (b Prelude.-a) Prelude.+ a)

-- Normal noise with mean and stdev
normal :: MonadNoise m => Prelude.Double -> Prelude.Double -> m Prelude.Double
normal m d = do
  -- Box-Muller, fast and simple, and good?
  u1 <- uniform
  u2 <- uniform
  -- we don't use second number we could, make impl simpler.
  let x = Prelude.sqrt (-2 Prelude.* Prelude.log u1) Prelude.* Prelude.cos (2 Prelude.* Prelude.pi Prelude.* u2)
  --  y = sqrt (-2 * log u1) * sin (2 * pi * u2)
  Prelude.return Prelude.$! m Prelude.+ x Prelude.* d

----------------------------------------
-- The CryptoContGen class

class (MonadNoise m, Distribution d) => CryptoContGen m d where
  genCryptoContVar :: d -> m Prelude.Double

instance MonadNoise m => CryptoContGen m LaplaceDistribution where
  genCryptoContVar d = do
    -- "text book" laplace distribution generation.
    x <- uniform
    Prelude.return Prelude.$! quantile d x

instance MonadNoise m => CryptoContGen m NormalDistribution where
  genCryptoContVar d = normal (mean d) (stdDev d)

instance MonadNoise m => CryptoContGen m ExponentialDistribution where
  genCryptoContVar d = do
    x <- uniform
    Prelude.return Prelude.$! quantile d x

----------------------------------------
-- Helpers

-- "Knuth" method, uniform [0, 1) multiplies of 2^-53
word64Uniform :: Word64 -> Prelude.Double
word64Uniform w64 = Prelude.fromIntegral (w64 `shiftR` 11) Prelude.* doubleUlp

doubleUlp :: Prelude.Double
doubleUlp =  1.0 Prelude./ Prelude.fromIntegral (1 `shiftL` 53 :: Word64)
