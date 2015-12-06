-----------------------------------------------------------------------------
--
-- Module      : DAAK.Algorithms.Gamo.Criteria
-- Copyright   : Armin Kazmi (2015)
-- License     : MIT
--
-- Maintainer  : Armin Kazmi
-- Stability   : experimental
-- Portability : portable
--
-- | The criteria module. This modules contains all criteria evalation functions, with
-- the notion whether they should be maximized or minimized. This decision is up to the user
-- when defining a 'MultiObjectiveProblem'.
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DAAK.Algorithms.Gamo.Criteria where

import           Test.Framework

import           DAAK.Algorithms.Gamo.Packing
import           DAAK.Core.ResidualSpace
import           DAAK.Core.Space3D               as S
import           Data.Function
import           Data.List
import           Data.Vect
import           GHC.Float
import           Moo.GeneticAlgorithm.Statistics
import           Moo.GeneticAlgorithm.Types

instance ObjectiveFunction (Maybe Packing -> Double) (Maybe Packing) where
  evalObjective f gs = zip gs $ f <$> concat gs

-- | Calculate the absolute 3D-filling rate of a packing.
fillingRate :: Packing -> Double
fillingRate (EmptyPacking _) = 0.0
fillingRate (Packing l es _ _) = float2Double (sum $ fmap (volume . snd) es) /
                                 float2Double (volume l)

-- | Calculate the absolute 2D-filling rate of a packing.
filling2DRate :: Packing -> Double
filling2DRate (EmptyPacking _) = 0.0
filling2DRate (Packing l es _ _) = float2Double (sum $ fmap (surfaceArea . snd) es) /
                                   float2Double (surfaceArea l)

-- | Calclulate the volume of the 3D-boundingBox of all packages in a packing
boundingBoxVolume :: Packing -> Double
boundingBoxVolume (EmptyPacking _) = 0
boundingBoxVolume (Packing l es _ _) = float2Double $ volume $ boundingBox $ fmap snd es


-- | Find the highest end-Z-coordinate of a packing or zero in case of an empty packing.
packingMaxZ :: Packing -> Double
packingMaxZ (EmptyPacking _) = 0
packingMaxZ (Packing l [] _ _) = 0
packingMaxZ (Packing l es _ _) = float2Double $ maximum $ (ez . snd) <$> es

-- | Return the amount of packed items.
packedItems :: Packing -> Double
packedItems (EmptyPacking _) = 0
packedItems (Packing l es _ _) = fromIntegral $ length es

-- | Calculate the free relative surface area.
freeSurfaceArea :: Packing -> Double
freeSurfaceArea (EmptyPacking _) = 1.0
freeSurfaceArea (Packing _ [] _ _) = 1.0
freeSurfaceArea (Packing l es _ _ ) = (float2Double (surfaceArea l) - float2Double (sum $ fmap (surfaceArea . snd) es)) /
                                float2Double  (surfaceArea l)

-- | The accumulated density of a packing defined as relative usage of the
-- 3D-boundingbox volume.
accDensity :: Packing -> Double
accDensity (EmptyPacking _) = 0
accDensity (Packing l [] _ _) = 0
accDensity p@(Packing l es _ _) = float2Double (sum $ fmap (volume . snd) es) / boundingBoxVolume p

-- | The fragmentation of a packing, defined as the number of residual spaces.
fragmentation :: Packing -> Double
fragmentation (EmptyPacking _) = 0
fragmentation (Packing l _ rs _) = fromIntegral $ length rs

-- | Returns the reduced list of sizes. E.g. for a 'EuclideanSpace3D' of
-- size (l = 3, d = 3, h = 5) this function will return [3, 5]. The result will
-- be of the type as requested by the caller site.
allSizes :: (Eq a, Fractional a, Floating a) => EuclideanSpace3D -> [a]
allSizes e = nub $ (fromRational . toRational) <$> [sizex e, sizey e, sizez e]

-- | Calculate the height variance of a 'Packing' or zero in case of an 'EmptyPacking'.
-- The variance is relative to a maximum estimation based on the smallest and biggest size coordinate.
heightVariance :: ProblemDescription -> Packing -> Double
heightVariance _ (EmptyPacking _) = 0
heightVariance (ProblemDescription _ allpacks _) (Packing l es _ _) = variance samples / limit
  where
    samples = (float2Double . ez . snd) <$> es
    allNubedSizes = nub $ concat $ (allSizes . snd) <$> allpacks
    maxH = float2Double $ maximum allNubedSizes
    minH = float2Double $ minimum allNubedSizes
    limit = ((maxH - minH) ^ 2)/4.0

-- | Return the squared center distance between two spaces.
accCenterDist :: (EuclideanSpace3D, EuclideanSpace3D) -> Float
accCenterDist = uncurry centerDistSqr

-- | Using `groupBy` on the package index determine a list of list of indexed spaces.
-- The order of the groups in the result list depends on the order in the input list.
groupIndexedSpace :: [IndexedSpace] -> [[IndexedSpace]]
groupIndexedSpace  = groupBy ((==) `on` fst) . sortBy (compare `on` fst)

-- | Group spaces by their end-Z-coordinate. Return the groups in ascending order.
groupByEz :: [EuclideanSpace3D] -> [[EuclideanSpace3D]]
groupByEz = groupBy ((==) `on` ez) . sortBy (compare `on` ez)

supportUnionIntersect :: (ResidualSpace, [EuclideanSpace3D]) -> EuclideanSpace3D
supportUnionIntersect (r, es) = rspace r `S.intersect` bbes `translate` (vec3Z &* ez bbes)
  where bbes = boundingBox es

supportGroupIntersectionSpaces :: [EuclideanSpace3D] -> [EuclideanSpace3D]
supportGroupIntersectionSpaces = fmap supportUnionIntersect . heightLayerUnionSplit . groupByEz

heightGroupBiggestSupportSpace :: [EuclideanSpace3D] -> EuclideanSpace3D
heightGroupBiggestSupportSpace = maximumBy (compare `on` surfaceArea) . supportGroupIntersectionSpaces

heightGroupBiggestSupport :: [EuclideanSpace3D] -> Float
heightGroupBiggestSupport = sum . fmap surfaceArea . supportGroupIntersectionSpaces

heightGroupBiggestSupportCrit :: Packing -> Double
heightGroupBiggestSupportCrit (EmptyPacking _) = 0.0
--heightGroupBiggestSupportCrit (Packing _ es _ _) = float2Double $ heightGroupBiggestSupport $ fmap snd es
heightGroupBiggestSupportCrit (Packing _ es _ _) = float2Double $ surfaceArea $ heightGroupBiggestSupportSpace $ fmap snd es

groupingDistanceAverage :: Packing -> Double
groupingDistanceAverage (EmptyPacking _) = 0.0
groupingDistanceAverage (Packing _ es _ _) = float2Double $ average $ fmap (sum . fmap accCenterDist) crosses
  where
    grouped = fmap snd <$> groupIndexedSpace es
    crosses = fmap (\k -> crossProdX k k) grouped
