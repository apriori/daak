-----------------------------------------------------------------------------
--
-- Module      : ResidualSpace
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
{-# LANGUAGE FlexibleInstances #-}
module DAAK.Core.ResidualSpace where
import           Test.Framework

import           Control.Applicative
import           Control.Monad
import           DAAK.Core.Debug
import           DAAK.Core.Space3D   as SP
import           Data.Function
import           Data.List           as L
import           Data.Maybe
import qualified Data.Set            as S
import           Data.Vect
import qualified Data.Vector         as V
import           Debug.Trace

-- | Defines equality of vectors, heavily relying on
-- 'Eq Vec'
instance Ord Vec3 where
  compare a@(Vec3 x y z) b@(Vec3 x1 y1 z1)
    | a == b = EQ
    | absDeq z z1 && absDeq y y1 = compare x x1
    | absDeq z z1 = compare y y1
    | otherwise = compare z z1


type LoadSpace = EuclideanSpace3D
data ResidualSpace = EmptyResidualSpace
                   | ResidualSpace !EuclideanSpace3D deriving (Show, Eq)

rspace :: ResidualSpace -> EuclideanSpace3D
rspace EmptyResidualSpace = EmptySpace3D
rspace (ResidualSpace a) = a

maybeEmptySpace :: EuclideanSpace3D -> Maybe EuclideanSpace3D
maybeEmptySpace EmptySpace3D = Nothing
maybeEmptySpace a = Just a


maybeEmptyResSpace :: ResidualSpace -> Maybe ResidualSpace
-- maybeEmptyResSpace EmptyResidualSpace = Nothing
-- maybeEmptyResSpace a = Just a
maybeEmptyResSpace = maybeEmptySpace . rspace >=> return . ResidualSpace

minMax :: (Ord a) => a -> a -> (a, a)
minMax a b
  | a >= b = (b, a)
  | otherwise = (a, b)


mkResidualSpace :: EuclideanSpace3D -> ResidualSpace
mkResidualSpace EmptySpace3D = EmptyResidualSpace
mkResidualSpace s = ResidualSpace s

startOrdSpace :: EuclideanSpace3D -> EuclideanSpace3D -> Ordering
startOrdSpace EmptySpace3D _ = GT
startOrdSpace _ EmptySpace3D = LT
startOrdSpace a b
  | eq == EQ
  = (compare `on` volume) b a
  | otherwise
  = eq
  where eq = (compare `on` start) a b

startOrd :: ResidualSpace -> ResidualSpace -> Ordering
startOrd EmptyResidualSpace _ = GT
startOrd _ EmptyResidualSpace = LT
startOrd (ResidualSpace a) (ResidualSpace b)
  | eq == EQ
  = (compare `on` volume) b a
  | otherwise
  = eq
  where eq = (compare `on` start) a b


toMaybeResSpace :: Vec3 -> Vec3 -> Maybe ResidualSpace
toMaybeResSpace l = maybeEmptyResSpace . mkResidualSpace . mkEuclidean3D l

-- | direct splitting case of spaces
-- | it is assumed that 2nd space lies completely in 1st space
splitSpaceDirect :: EuclideanSpace3D -> EuclideanSpace3D -> [ResidualSpace]
splitSpaceDirect EmptySpace3D _ = []
splitSpaceDirect a EmptySpace3D = [mkResidualSpace a]
splitSpaceDirect a@(EuclideanSpace3D l1 r1) b@(EuclideanSpace3D l2 r2) = catMaybes
  [ -- FRONT
    toMaybeResSpace (start a) (Vec3 (ex a) (sy b) (ez a))
    -- LEFT
  ,  toMaybeResSpace (start a) (Vec3 (sx b) (ey a) (ez a))
    -- RIGHT
  , toMaybeResSpace (Vec3 (ex b) (sy a) (sz a)) (end a)
    -- BACK
  , toMaybeResSpace (Vec3 (sx a) (ey b) (sz a)) (end a)
    -- BOT (nope)
    -- TOP
  --, toMaybeResSpace (Vec3 (sx a) (sy a) (ez b)) (end a)
  ]



-- | direct splitting case of spaces
-- | it is assumed that 2nd space lies completely in 1st space
splitSpaceDirectSupport :: EuclideanSpace3D -> EuclideanSpace3D -> [ResidualSpace]
splitSpaceDirectSupport EmptySpace3D _ = []
splitSpaceDirectSupport a EmptySpace3D = [mkResidualSpace a]
splitSpaceDirectSupport a b = catMaybes
  [ -- FRONT
    toMaybeResSpace (start a) (Vec3 (ex a) (sy b) (ez a))
    -- LEFT
  ,  toMaybeResSpace (start a) (Vec3 (sx b) (ey a) (ez a))
    -- RIGHT
  , toMaybeResSpace (Vec3 (ex b) (sy a) (sz a)) (end a)
    -- BACK
  , toMaybeResSpace (Vec3 (sx a) (ey b) (sz a)) (end a)
    -- BOT (nope)
    -- TOP
  -- , toMaybeResSpace (Vec3 (sx b) (sy b) (ez b)) (Vec3 (ex b) (ey b) (ez a))
  ]

-- | split 1st space by 2nd space, taking into account non overlapping and only
-- | partially overlapping cases
splitSpaceSupport :: EuclideanSpace3D -> EuclideanSpace3D -> [ResidualSpace]
splitSpaceSupport a b
  | overlap a b
  = splitSpaceDirectSupport a (a `SP.intersect` b)
  | otherwise = [mkResidualSpace a]

-- | split 1st space by 2nd space, taking into account non overlapping and only
-- | partially overlapping cases
splitSpace :: EuclideanSpace3D -> EuclideanSpace3D -> [ResidualSpace]
splitSpace a b
  | overlap a b
  = splitSpaceDirect a (a `SP.intersect` b)
  | otherwise = [mkResidualSpace a]

-- | Does the given space fit in the given freespace completely?
-- | If so return Just tupeliziation of both
fitMaybe :: EuclideanSpace3D -> ResidualSpace -> Maybe (EuclideanSpace3D, ResidualSpace)
fitMaybe _ EmptyResidualSpace = Nothing
fitMaybe a r@(ResidualSpace b)
  | sizex a <= sizex b
  , sizey a <= sizey b
  , sizez a <= sizez b
  = Just (a, r)
  | otherwise
  = Nothing

isDominant :: ResidualSpace -> ResidualSpace -> Bool
isDominant = isInside `on` rspace

dominantsExtend :: [ResidualSpace] -> ResidualSpace -> [ResidualSpace]
dominantsExtend rs r
  | any (isDominant r) rs
  = rs
  | otherwise
  -- TODO: sorting?
  = r : rs

-- | Split every given residual space by given space and concat the result
splitAll :: [ResidualSpace] -> EuclideanSpace3D -> [ResidualSpace]
splitAll rs e = concatMap (flip splitSpace e . rspace) rs

-- split all residual spaces by given spaces, also
-- splitting all resulting new residual spaces by the next given space
splitFold :: [ResidualSpace] -> [EuclideanSpace3D] -> [ResidualSpace]
splitFold = L.foldl splitAll

-- | Build reduced list of residuals filtered by dominance relation
buildUpDominants :: [ResidualSpace] -> [ResidualSpace]
buildUpDominants = L.foldl dominantsExtend [] . sortBy startOrd

-- | Split all residual spaces by the given spaces (and their resulting new
-- | residual spaces) and make sure the dominance relation holds
splitFoldDominant :: [ResidualSpace] -> [EuclideanSpace3D] -> [ResidualSpace]
splitFoldDominant rs es = sortBy startOrd $ buildUpDominants $ splitFold rs es

---------
-- | Split every given residual space by given space and concat the result
splitAllSupport :: [ResidualSpace] -> EuclideanSpace3D -> [ResidualSpace]
splitAllSupport rs e = concatMap (flip splitSpaceSupport e . rspace) rs

-- split all residual spaces by given spaces, also
-- splitting all resulting new residual spaces by the next given space
splitFoldSupport :: [ResidualSpace] -> [EuclideanSpace3D] -> [ResidualSpace]
splitFoldSupport = L.foldl splitAllSupport

-- | Split all residual spaces by the given spaces (and their resulting new
-- | residual spaces) and make sure the dominance relation holds
splitFoldDominantSupport :: [ResidualSpace] -> [EuclideanSpace3D] -> [ResidualSpace]
splitFoldDominantSupport rs es = sortBy startOrd $ buildUpDominants $ splitFoldSupport rs es
----


-- | Filter in all spaces that lie in the given residual space
determineInside :: ResidualSpace -> [EuclideanSpace3D] -> [EuclideanSpace3D]
determineInside r = filter (`isInside` rspace r)

splitFoldSupport' :: [EuclideanSpace3D] -> [EuclideanSpace3D] -> [(ResidualSpace, [EuclideanSpace3D])]
splitFoldSupport' grouped others =
  zipWith (\r es -> (r, determineInside r es))
          (splitFoldDominantSupport [mkResidualSpace groupBB] sortedOthers) $ repeat grouped
  where
    groupBB = boundingBox grouped
    sortedOthers = sortBy (compare `on` ez) others

-- splitFold2 :: LoadSpace -> [EuclideanSpace3D] -> [ResidualSpace] -> [ResidualSpace]
-- splitFold2 l packs res =
--   sortBy (startOrdSpace `on` rspace) $ rest ++ splitFoldDominant splitrs (rspace <$> rest)
--   where
--     mz = maximum $ ez <$> packs
--     (splitrs, rest) = partition ((==) mz . sz . rspace) res

splitFold2 :: LoadSpace -> [EuclideanSpace3D] -> [ResidualSpace] -> [ResidualSpace]
splitFold2 l packs res =
  sortBy (startOrdSpace `on` rspace) $ splitFoldDominant [mkResidualSpace l] $ rspace <$> res


heightLayerUnionSplitWork :: [[EuclideanSpace3D]] -> [[(ResidualSpace, [EuclideanSpace3D])]]
heightLayerUnionSplitWork [] = []
heightLayerUnionSplitWork (frontGroup : rest) = splitFoldSupport' frontGroup (concat rest) : heightLayerUnionSplitWork rest

heightLayerUnionSplit :: [[EuclideanSpace3D]] -> [(ResidualSpace, [EuclideanSpace3D])]
heightLayerUnionSplit = concat . heightLayerUnionSplitWork
