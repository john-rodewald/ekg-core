-- | Perform 100,000 atomic sample additions using 100 concurrent
-- writers.
module Main where

import Prelude hiding (read)

import Criterion
import Criterion.Main (defaultMain)
import Tasks

main :: IO ()
main = defaultMain
  [ bench "Add to distribution with multiple writers" $
      whnfIO addToDistributionWithMultipleWriters
  ]

