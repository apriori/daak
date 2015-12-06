-----------------------------------------------------------------------------
--
-- Module      : Encoding
-- Copyright   : Armin Kazmi (2015)
-- License     : MIT
--
-- Maintainer  : Armin Kazmi
-- Stability   : experimental
-- Portability : GHC only or compatible
--
-- | This module contains all functions and types to describe the genetic
-- encoding for the packing algorithm.
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module DAAK.Algorithms.Gamo.Encoding where

import           Test.Framework

import           Control.DeepSeq
import           Control.Monad
import           Control.Parallel.Strategies
import           DAAK.Core.ResidualSpace
import           DAAK.Core.Space3D                   as S
import           Data.Function
import           Data.List                           as L
import           Data.Map                            as M
import           Data.Maybe
import           GHC.Float
import           Moo.GeneticAlgorithm.Continuous
import           Moo.GeneticAlgorithm.Multiobjective hiding (stepNSGA2)
import           Moo.GeneticAlgorithm.Random
import           Moo.GeneticAlgorithm.Statistics

import           DAAK.Algorithms.NSGA2Mod
import           DAAK.Core.Debug
import           DAAK.Core.Utilities
import           Data.Vect
import           Debug.Trace

-- | Flat genomes
-- prop> all in [0.0, 1.0]
type ChromosomeType = Double
type OrderPermutationGenome = [ChromosomeType]
type SelectorGenome = [ChromosomeType]
type PackingSelectorGenome = [ChromosomeType]
type OrientationGenome = [ChromosomeType]
type FillOrderGenome = [ChromosomeType]

-- | Tupelization of all flat genomes
type RandomSelectors = ( SelectorGenome
                       , OrderPermutationGenome
                       , OrientationGenome
                       , FillOrderGenome
                       , PackingSelectorGenome
                       )

-- | A single chromosome package
type PackingFullChromosome = (ChromosomeType, ChromosomeType, ChromosomeType, ChromosomeType, ChromosomeType)

selectorTupleSize :: Int
selectorTupleSize = 5

-- | A reduced chromosome package only containg orientation, fill order and candidate selector
type PackingSelector = (ChromosomeType, ChromosomeType, ChromosomeType)
-- | Tupelization of orientation, fill order and candidate selector genomes
type PackingSelectors = ( OrientationGenome
                        , FillOrderGenome
                        , PackingSelectorGenome
                        )

-- | Return selector chromosome, aka first of provided tupel
selectorChromosome :: PackingFullChromosome -> ChromosomeType
selectorChromosome (a, _, _, _, _) = a

-- | Return order chromosome, aka second of provided tupel
orderChromosome :: PackingFullChromosome -> ChromosomeType
orderChromosome (_, a, _, _, _) = a

-- | Return orientation chromosome, aka third of provided tupel
orientationChromosome :: PackingFullChromosome -> ChromosomeType
orientationChromosome(_, _, a, _, _) = a

-- | Return fill order chromosome, aka fourth of provided tupel
fillOrderChromosome :: PackingFullChromosome -> ChromosomeType
fillOrderChromosome (_, _, _, a, _) = a

-- | Return candidate chromosome, aka fifth of provided tupel
packingChromosome :: PackingFullChromosome -> ChromosomeType
packingChromosome (_, _, _, _, a) = a


-- | Defines selection for a value of >= 0.5
useSelector :: Double -> Bool
useSelector = (>= 0.5)
--useSelector _ = True

-- | For a given selector chromosome and an item,
-- return either 'Just' in case it is to be selected or 'Nothing'.
selector :: (ChromosomeType, a) -> Maybe a
selector (s, a)
  | useSelector s = Just a
  | otherwise = Nothing

-- | For a given limit, a list of values and a list of items
-- return all items that are to be selected and whos values exceed the limit.
-- __Note__: It is totally fine for both lists to be of different length. The behavior
-- in such a case is defined by that of 'zip'.
selectLimit :: Double -> [Double] -> [a] -> [a]
selectLimit p ds as = snd <$> L.filter (flip (>=) p. fst) (zip ds as)



-- five lists (see 'RandomSelectors').
-- | Decompose a flat list of chromosomes into an unzipped tuple of
extractSelectors :: Genome ChromosomeType -> RandomSelectors
extractSelectors = deconstruct5Var

-- | Decompose 'RandomSelectors' into list of chromosome packages
extractPackingFullChromoSomes :: RandomSelectors -> [PackingFullChromosome]
extractPackingFullChromoSomes (a, b, c, d, e) = zip5 a b c d e

-- | Remove all chromosome packages that do not encode a selection with their
-- selection chromosome. Also strip all selection chromosome.
filterSelectors :: RandomSelectors ->
                   (OrderPermutationGenome, OrientationGenome, FillOrderGenome, PackingSelectorGenome)
filterSelectors (a, b, c, d, e) = unzip4 $
                                  catMaybes $
                                  zipWith5 (\r o or lt p -> selector (r, (o, or, lt, p)) ) a b c d e



getRandomR01 :: (Num a, Random a) => Rand a
getRandomR01 = getRandomR (0, 1)

getRandomLazy01Genome :: (Num a, Random a) => Int -> Rand (Genome a)
getRandomLazy01Genome n = replicateM n getRandomR01

splitSuffixFactor :: Genome a -> Double -> (Genome a, Genome a)
splitSuffixFactor as l = splitAt location as
  where
    location = floor $ l * fromIntegral (length as)

flattenPackingFullChromosomes :: [PackingFullChromosome] -> Genome Double
flattenPackingFullChromosomes = concatMap (\(a, b, c, d, e) -> [a, b, c, d, e])

modifyOrderGeneBy :: (ChromosomeType -> ChromosomeType) ->
                     Genome ChromosomeType ->
                     Genome ChromosomeType
modifyOrderGeneBy modify g = flattenPackingFullChromosomes $ zip5 a (modify <$> b) c d e
  where
    (a, b, c, d, e) = unzip5 $ extractPackingFullChromoSomes $ extractSelectors g

keepOrderReplace :: PackingFullChromosome ->
                    PackingFullChromosome ->
                    PackingFullChromosome
keepOrderReplace a b =
  ( selectorChromosome b
  , orderChromosome a
  , orientationChromosome b
  , fillOrderChromosome b
  , packingChromosome b
  )

clampOffsetOrderReplace :: Double ->
                           PackingFullChromosome ->
                           PackingFullChromosome ->
                           PackingFullChromosome
clampOffsetOrderReplace offset _ b =
  ( selectorChromosome b
  , minmax 0.0 1.0 $ orderChromosome b + offset
  , orientationChromosome b
  , fillOrderChromosome b
  , packingChromosome b
  )
