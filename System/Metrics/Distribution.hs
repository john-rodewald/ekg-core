module System.Metrics.Distribution
    (
      -- * Warning
      -- $warning

      -- * Distributions
      Distribution
    , new
    , add
    , addN
    , read

      -- * Gathered statistics
    , Internal.Stats
    , Internal.mean
    , Internal.variance
    , Internal.count
    , Internal.sum
    , Internal.min
    , Internal.max
    ) where

import Prelude hiding (max, min, read, sum)

import qualified Data.Array as A
import qualified System.Metrics.Distribution.Internal as Internal
import System.Metrics.ThreadId

import qualified Prelude

import Control.Monad ((<$!>), forM_, replicateM, when)
import Data.Atomics (atomicModifyIORefCAS_)
import Data.IORef
import Data.Int (Int64)

------------------------------------------------------------------------

-- | An metric for tracking events.
newtype Distribution = Distribution { unD :: A.Array Stripe }

-- | Number of lock stripes. Should be greater or equal to the number
-- of HECs.
numStripes :: Int
numStripes = 8

-- | Get the stripe to use for this thread.
myStripe :: Distribution -> IO Stripe
myStripe distrib = do
    tid <- myCapability
    return $! unD distrib `A.index` (tid `mod` numStripes)

------------------------------------------------------------------------

-- We want at least 64 bits in order to avoid overflow of the count of
-- samples added to the distribution.

newtype Stripe = Stripe (IORef Distrib)

data Distrib = Distrib
  { dMean :: !Double
  , dSumSqDelta :: !Double
  , dCount :: !Int64
  , dSum   :: !Double
  , dMin   :: !Double
  , dMax   :: !Double
  }

newStripe :: IO Stripe
newStripe = Stripe <$!> newIORef Distrib
  { dMean  = 0
  , dSumSqDelta = 0
  , dCount = 0
  , dSum   = 0
  , dMin   = inf
  , dMax   = -inf
  }
  where
    inf :: Double
    inf = 1/0

-- | Mean and variance are computed according to
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
stripeAddN :: Stripe -> Double -> Int -> IO ()
stripeAddN (Stripe ref) val n = atomicModifyIORefCAS_ ref $ \dist ->
  let n' = fromIntegral n
      newCount = fromIntegral n + dCount dist
      newCount' = fromIntegral newCount
      delta = val - dMean dist
  in  Distrib
        { dMean = dMean dist + n' * delta / newCount'
        , dSumSqDelta = dSumSqDelta dist +
            delta * delta * (n' * fromIntegral (dCount dist)) / newCount'
        , dCount = newCount
        , dSum   = dSum dist + n' * val
        , dMin   = Prelude.min val (dMin dist)
        , dMax   = Prelude.max val (dMax dist)
        }

-- | Adds the data of the left distribution to that of the right
-- distribution using
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
stripeCombine :: Stripe -> Stripe -> IO ()
stripeCombine (Stripe ref) (Stripe accRef) = do
  dist <- readIORef ref
  -- If the left stripe has no data, do not combine its data with that of
  -- the right stripe. This is to avoid `NaN`s from divisons by zero when
  -- the right stripe also has no data.
  when (dCount dist > 0) $
    modifyIORef' accRef $ \accDist ->
      let count = dCount dist
          mean = dMean dist
          accCount = dCount accDist
          accMean = dMean accDist
          newCount = count + accCount
          delta = mean - accMean
          count' = fromIntegral count
          accCount' = fromIntegral accCount
          newCount' = fromIntegral newCount
      in  Distrib
            { dMean = (accCount' * accMean + count' * mean) / newCount'
            , dSumSqDelta = dSumSqDelta accDist + dSumSqDelta dist +
                delta * delta * (accCount' * count') / newCount'
            , dCount = newCount
            , dSum   = dSum accDist + dSum dist
            , dMin   = Prelude.min (dMin accDist) (dMin dist)
            , dMax   = Prelude.max (dMax accDist) (dMax dist)
            }

readStripe :: Stripe -> IO Internal.Stats
readStripe (Stripe ref) = do
  dist <- readIORef ref
  let count = dCount dist
  pure $! Internal.Stats
    { Internal.mean  = if count == 0 then 0.0 else dMean dist
    , Internal.variance = if count == 0 then 0.0
                            else dSumSqDelta dist / fromIntegral count
    , Internal.count = dCount dist
    , Internal.sum   = dSum dist
    , Internal.min   = if count == 0 then 0.0 else dMin dist
    , Internal.max   = if count == 0 then 0.0 else dMax dist
    }

------------------------------------------------------------------------
-- * Distributions

-- Exposed API

-- | Create a new distribution.
new :: IO Distribution
new = (Distribution . A.fromList numStripes) `fmap`
      replicateM numStripes newStripe

-- | Add a value to the distribution.
add :: Distribution -> Double -> IO ()
add distrib val = addN distrib val 1

-- | Add the same value to the distribution N times.
addN :: Distribution -> Double -> Int -> IO ()
addN distribution val n = do
  stripe <- myStripe distribution
  stripeAddN stripe val n

-- | Get the current statistical summary for the event being tracked.
read :: Distribution -> IO Internal.Stats
read distribution = do
  result <- newStripe
  forM_ (A.toList $ unD distribution) $ \stripe ->
    stripeCombine stripe result
  readStripe result
