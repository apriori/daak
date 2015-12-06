-----------------------------------------------------------------------------
--
-- Module      : MT
-- Copyright   : Armin Kazmi (2015)
-- License     : MIT
--
-- Maintainer  : Armin Kazmi
-- Stability   : experimental
-- Portability : portable
--
-- |
--
-----------------------------------------------------------------------------
module DAAK.Core.Random.MT where

import           Data.Word
import           System.CPUTime
import           System.Random.Mersenne.Pure64
import           System.Time

-- | Create a new PureMT generator, using the clocktime as the base for the seed.
newPureMTGetSeed :: IO (PureMT, Word64)
newPureMTGetSeed = do
  ct             <- getCPUTime
  (TOD sec psec) <- getClockTime
  let seed = fromIntegral $ sec * 1013904242 + psec + ct
  return (pureMT seed, seed)
