-----------------------------------------------------------------------------
--
-- Module      : Gamo
-- Copyright   : Armin Kazmi (2015)
-- License     : MIT
--
-- Maintainer  : Armin Kazmi
-- Stability   : experimental
-- Portability : GHC only or compatible
--
-- |
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module DAAK.Algorithms.Gamo where

import           Test.Framework

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Control.Parallel.Strategies
import           DAAK.Core.ResidualSpace
import           DAAK.Core.Space3D                   as S
import           Data.Function
import           Data.List                           as L
import           Data.Map                            as M
import           Data.Maybe
import           Data.Vect
import           GHC.Float
import           Moo.GeneticAlgorithm.Continuous
import           Moo.GeneticAlgorithm.Multiobjective hiding (stepNSGA2)
import           Moo.GeneticAlgorithm.Random
import           Moo.GeneticAlgorithm.Statistics

import           DAAK.Algorithms.Gamo.Criteria
import           DAAK.Algorithms.Gamo.Packing
import           DAAK.Algorithms.NSGA2Mod
import           DAAK.Core.Debug
import           DAAK.Core.Utilities

import           DAAK.Algorithms.Gamo.Encoding
import           Debug.Trace

-- | NFData instances for EuclideanSpace3D, flat parallel evaluation
instance NFData EuclideanSpace3D where
   rnf a = a `seq` ()

-- | NFData instances for ResidualSpace, flat parallel evaluation
instance NFData ResidualSpace where
   rnf a = a `seq` ()

-- | NFData instance for parallel calculation of a packing.
-- | Evaluate it flat.
instance NFData Packing where
  rnf a = a `seq` ()

-- | Run the packing algorithm on a given problem with the given genome.
-- The algorithm returns 'Nothing', if there was no way to place a package
-- as defined by 'placeAll'.
packing :: ProblemDescription -> Genome Double -> Maybe Packing
packing (ProblemDescription _ [] _) _  = Nothing
packing (ProblemDescription load ipackages iquantities) !genome
  | length genome < itemsCount iquantities * selectorTupleSize
  = error $  "Insufficent random data, the size of the initial random population is too small "
          ++ "to feed the packing process, is "
          ++ show (length genome)
          ++ " should be "
          ++ show (itemsCount iquantities * selectorTupleSize)
  | Just (es, rs, used) <- placeAll load chromosomePackages sortedPackages recIqs
  = mkPackingMaybe load es rs used
  | otherwise
  = Nothing
  where
    -- TODO: double selector ... wasted performance
    selectedChromosomes@(sgs, _, _, _, _) = extractSelectors genome
    ( orderChromosomes, orientationChromosomes, fillOrderChromosomes , candidateSelectChromosome) = filterSelectors selectedChromosomes
    chromosomePackages = (orientationChromosomes, fillOrderChromosomes, candidateSelectChromosome)
    -- select spaces
    repss = qutantityReplicateItems ipackages iquantities
    selectedPackages = catMaybes $ selector <$> zip sgs repss
    sortedPackages = sortByKeys orderChromosomes selectedPackages
    -- readjust quantities due to selection
    recIqs = (\a -> (fst $ head a, length a)) <$> groupBy ((==) `on` fst) selectedPackages


allSizes :: EuclideanSpace3D -> [Float]
allSizes e = nub [sizex e, sizey e, sizez e]

accCenterDist :: (EuclideanSpace3D, EuclideanSpace3D) -> Float
accCenterDist = uncurry centerDistSqr

groupIndexedSpace :: [IndexedSpace] -> [[IndexedSpace]]
groupIndexedSpace = groupBy ((==) `on` fst) . sortBy (compare `on` fst)

groupByEz :: [EuclideanSpace3D] -> [[EuclideanSpace3D]]
groupByEz = groupBy ((==) `on` ez) . sortBy (compare `on` ez)

popsize :: Int
popsize = 1

initialize :: (Ord a, Random a, Fractional a) => Int -> ProblemDescription -> Rand [Genome a]
initialize s (ProblemDescription _ _ iqs) = getRandomGenomes (s * is) $ replicate l (0.0, 1.0)
  where
   is = itemsCount iqs
   l = is * selectorTupleSize

--tournament = tournamentSelect Minimizing 2 30
--tournament = stochasticUniversalSampling 12
tournament = rouletteSelect 20
--tournament = unimodalCrossoverRP
--tournament = topBotSelect


modifyOrderGeneBy :: (Double -> Double) ->
                     Genome Double ->
                     Genome Double
modifyOrderGeneBy modify g = flattenPackingFullChromosomes $ zip5 a (modify <$> b) c d e
  where
    (a, b, c, d, e) = unzip5 $ extractPackingFullChromoSomes $ extractSelectors g

topBotSelect :: Population Double -> Rand (Population Double)
topBotSelect ps =
  do
    let (tops, rest) = splitSuffixFactor (bestFirst Minimizing ps) 0.15
        flattenCombine a b = concatMap (\(a, b) -> [a, b]) $ zip a b
    restSamples <- randomSample (length tops) rest
    return $ flattenCombine tops restSamples

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

solutionMergeCrossover :: ProblemDescription ->
                          Double ->
                          [Genome Double] ->
                          Rand ([Genome Double], [Genome Double])
solutionMergeCrossover _ _ [] = return ([], [])
solutionMergeCrossover pd p parents@(a:b:rs) =
  do
    keepSplit <- getRandomR (0.0, 1.0)
    let
      extract = L.partition (useSelector . orderChromosome . snd)
      rawGenes = zip [0..] . extractPackingFullChromoSomes . extractSelectors
      comp = compare `on` (orderChromosome . snd)
      determineUsed = genesUsed . packing pd
      (rawA, rawB) = (rawGenes a, rawGenes b)
      (packSelectedA, packUnselectedA) = extract rawA
      (packSelectedB, packUnselectedB) = extract rawB
      (sortedA, sortedB) = (sortBy comp packSelectedA, sortBy comp packSelectedB)
      (packedA, packedB) = (determineUsed a, determineUsed b)
      (packedObjA, packedObjB) = (take packedA sortedA, take packedB sortedB)
      ((untouchedA, removedA), (untouchedB, removedB)) =
       ( splitSuffixFactor packedObjA keepSplit
       , splitSuffixFactor packedObjB keepSplit
       )
      maxKeyA = if L.null untouchedA then 0.0
                else maximum $ orderChromosome . snd <$> untouchedA
      maxKeyB = if L.null untouchedB then 0.0
                else maximum $ orderChromosome . snd <$> untouchedB
      lenRemA = length removedA
      lenRemB = length removedB
      minLen = min lenRemA lenRemB
    crossRands <- getRandomLazy01Genome minLen
    let
      cA = selectLimit p crossRands removedB
      cB = selectLimit p crossRands removedA
      geneA = offsetReplaceFlatten maxKeyA rawA cA
      geneB = offsetReplaceFlatten maxKeyB rawB cB
    uniformCrosses <- doCrossovers parents (uniformCrossover p)
    return ([geneA, geneB] ++ uniformCrosses, rs)
solutionMergeCrossover _ _ gs = fail $ "Invalid crossover attempt with list of len " ++ show (length gs)

offsetReplaceFlatten :: Double -> [(Int, PackingFullChromosome)] -> [(Int, PackingFullChromosome)] -> Genome Double
offsetReplaceFlatten offset base replace =
  flattenPackingFullChromosomes $ replaceAllElem (clampOffsetOrderReplace offset) (snd <$> base) replace




mutate :: Double -> Genome Double -> Rand (Genome Double)
mutate _ [] = return []
mutate p gs = mapM (withProbability p (const getRandomR01)) gs


step :: ProblemDescription -> (ProblemDescription -> MultiObjectiveProblem (Maybe Packing -> Double)) -> StepGA Rand Double
step p mop = stepNSGA2 (packing p) (mop p) tournament (solutionMergeCrossover p 0.7) (mutate 0.4)
