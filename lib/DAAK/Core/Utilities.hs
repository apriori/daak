-----------------------------------------------------------------------------
--
-- Module      : Utilities
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
{-# LANGUAGE BangPatterns #-}
module DAAK.Core.Utilities where

import           Data.Function
import           Data.List                       as L
import           Moo.GeneticAlgorithm.Statistics


repeatM :: Monad m => m a -> m [a]
repeatM = sequence . repeat

{-# INLINE iterateM #-}
iterateM n f = go n
 where
   go 0 !x = return x
   go n !x = f x >>= go (n-1)


breakList :: [a] -> Int -> [[a]]
breakList [] _ = []
breakList as l = take l as : breakList (drop l as) l

deconstruct5 :: [a] -> Int -> ([a], [a], [a], [a], [a])
deconstruct5 as l = (a, b, c, d, e)
  where [a, b, c, d, e] = breakList as l

deconstruct5VarWork :: [a] -> [(a, a, a, a, a)]
deconstruct5VarWork (a:b:c:d:e:as) = (a, b, c, d, e) : deconstruct5VarWork as
deconstruct5VarWork _ = []

deconstruct5Var :: [a] -> ([a], [a], [a], [a], [a])
deconstruct5Var = unzip5 . deconstruct5VarWork

break5 :: [[a]] -> Int -> [([a], [a], [a], [a], [a])]
break5 as l = (`deconstruct5` l) <$> as

uniont3 :: (a, b, c) -> d -> (a, b, c, d)
uniont3 (a, b, c) d = (a, b, c, d)

sortByKeys :: [Double] -> [a] -> [a]
sortByKeys rs ss = snd <$> sortBy (compare `on` fst) (zip rs ss)

tsnd :: (a, b, c, d) -> b
tsnd (_, b, _, _) = b

takeBack :: Int -> [a] -> [a]
takeBack i = reverse . take i . reverse

harmonicMean :: Floating a => [a] -> a
harmonicMean xs = fromIntegral (length xs) / sum ((1/) <$> xs)

varianceBoundUpper :: (Ord a, Num a, Floating a) => [a] -> a
varianceBoundUpper xs
  | avg == havg = 0
  | otherwise = (xmax * (avg - havg) * (xmax - avg)) / (xmax - havg)
  where
    xmax = maximum xs
    avg = average xs
    havg = harmonicMean xs

minmax :: Ord a => a -> a -> a -> a
minmax down up v = max down $ min up v

replaceElem :: Int -> [a] -> a -> [a]
replaceElem _ []  _ = []
replaceElem loc xs x = left ++ (x : exclusiveRight)
  where
    (left, inclusiveRight) = splitAt loc xs
    exclusiveRight = tail inclusiveRight

replaceAllElem :: (a -> a -> a) -> [a] -> [(Int, a)] -> [a]
replaceAllElem f = L.foldl (\xs' (i, a) -> replaceElem i xs' $ f (xs' !! i) a)
