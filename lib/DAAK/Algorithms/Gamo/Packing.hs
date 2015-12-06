-----------------------------------------------------------------------------
--
-- Module      : Packing
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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module DAAK.Algorithms.Gamo.Packing where

import           Test.Framework

import           Control.Monad
import           DAAK.Algorithms.Gamo.Encoding
import           DAAK.Core.ResidualSpace
import           DAAK.Core.Space3D             as S
import           DAAK.Core.Utilities
import           Data.List                     as L
import           Data.Map                      as M
import           Data.Vect

-- | Package ids are of type Int
type IdKey = Int
-- | Package quantities are of type Int
type Quantity = Int
-- | A single item/package has an id and a quantity
type ItemQuantity = (IdKey, Quantity)
-- | Several items with their quantities
type ItemQuantities = [ItemQuantity]
-- | A map from an item key index and its quantity
type ItemQuantityMap = Map IdKey Quantity

-- | A Problem consists of a loadspace (EuclideanSpace3D) several indexed
-- packages and items with their quantities
data ProblemDescription = ProblemDescription !LoadSpace ![IndexedSpace] !ItemQuantities
                          deriving (Show, Eq)

-- | A packing algebraic type
data Packing = Packing !LoadSpace ![IndexedSpace] ![ResidualSpace] Int -- ^ A non empty packing with loadspace, indexed packages, residual spaces and genes used
             | EmptyPacking !LoadSpace -- ^ An empty packing with the loadspace used
             deriving (Show)

-- | Constructor for type Packing, creating either a Packing or EmptyPacking
mkPacking :: LoadSpace -> [IndexedSpace] -> [ResidualSpace] -> Int -> Packing
mkPacking load [] _ _ = EmptyPacking load
mkPacking load ispaces residuals usedC = Packing load ispaces residuals usedC

-- | Constructor for type Packing, creating either a Just Packing or Nothing
mkPackingMaybe :: LoadSpace -> [IndexedSpace] -> [ResidualSpace] -> Int -> Maybe Packing
mkPackingMaybe l is rs used
  | packing <- mkPacking l is rs used
  , Packing{} <- packing
  = Just packing
  | otherwise
  = Nothing

-- | For a given list of 'ItemQuantity', calculate the total amount of items.
itemsCount :: ItemQuantities -> Int
itemsCount = sum . fmap snd

-- | Replicate all indexed packages with their quantities into a flat list.
-- Say package A is to be placed 3 times, package B twice. Then the resulting list will be:
-- [A, A, A, B, B]. The order of packages is defined by the order of the provided list of packages.
qutantityReplicateItems :: ItemSpaces -> ItemQuantities -> ItemSpaces
qutantityReplicateItems is =
  concatMap (\(key, quantity) -> replicate quantity $ is !! key)

genesUsed :: Maybe Packing -> Int
genesUsed Nothing = 0
genesUsed (Just (EmptyPacking _)) = 0
genesUsed (Just (Packing _ _ _ used)) = used


type AxeOrder = [Axis]
-- | Possible fill orders as a list of a list of axis (currently for 2D only)
allFillOrders :: [AxeOrder]
--allFillOrders = [[Y, Z], [Z, Y], [X, Y], [Y, X], [X, Z], [Z, X]]
allFillOrders = [[X, Y], [Y, X]]

-- | try to fill a residual space @rspace@
fillResidual :: (EuclideanSpace3D, ResidualSpace) -- ^ Package and picked residual space
           -> Quantity -- ^ quantity of packages (to be maximized in the filling)
           -> AxeOrder -- ^ axe fill order
           -> Maybe (EuclideanSpace3D, [Vec3], Quantity) -- ^ used package, all translation vectors and used quantity or @Nothing@
                                                       -- if not a single item fits
fillResidual (space, residual) q order
  | order == [Y, Z]
  , facs <- take minq
           [ Vec3 x y z
           | y <- [0 .. fromIntegral ys]
           , z <- [0 .. fromIntegral zs]
           , x <- [0 .. fromIntegral xs]
           ]
  , not $ L.null facs
  = Just (space, facs, minq)
  | order == [Z, Y]
  , facs <- take minq
           [ Vec3 x y z
           | z <- [0 .. fromIntegral zs]
           , y <- [0 .. fromIntegral ys]
           , x <- [0 .. fromIntegral xs]
           ]
  , not $ L.null facs
  = Just (space, facs, minq)
  | order == [X, Y]
  , facs <- take minq
           [ Vec3 x y z
           | x <- [0 .. fromIntegral xs]
           , y <- [0 .. fromIntegral ys]
           , z <- [0 .. fromIntegral zs]
           ]
  , not $ L.null facs
  = Just (space, facs, minq)
  | order == [Y, X]
  , facs <- take minq
           [ Vec3 x y z
           | y <- [0 .. fromIntegral ys]
           , x <- [0 .. fromIntegral xs]
           , z <- [0 .. fromIntegral zs]
           ]
  , not $ L.null facs
  = Just (space, facs, minq)
  | order == [X, Z]
  , facs <- take minq
           [ Vec3 x y z
           | x <- [0 .. fromIntegral xs]
           , z <- [0 .. fromIntegral zs]
           , y <- [0 .. fromIntegral ys]
           ]
  , not $ L.null facs
  = Just (space, facs, minq)
  | order == [Z, X]
  , facs <- take minq
           [ Vec3 x y z
           | z <- [0 .. fromIntegral zs]
           , x <- [0 .. fromIntegral xs]
           , y <- [0 .. fromIntegral ys]
           ]
  , not $ L.null facs
  = Just (space, facs, minq)
  | otherwise
  = Nothing
  where
    sresidual = S.size $ rspace residual
    sspace = mapVec (1/) $ S.size space
    fits = sresidual `pointwise` sspace
    (xs, ys, zs) = ( floor (vx fits) -1
                   , floor (vy fits) -1
                   , min (floor (vz fits) -1 ) 0
                   )
    minq = min q ((xs + 1) * (ys + 1) * (zs + 1))


-- | Try to fill a residual space 'rspace'.
-- in contrast to @fillResidual@ the used freespace is also returned
fillQuantityMaybe :: (EuclideanSpace3D, ResidualSpace) -- ^ Package and picked residual space
                -> AxeOrder -- ^ axe fill order
                -> Quantity -- ^ quantity of packages (to be maximized in the filling)
                -> Maybe (ResidualSpace, EuclideanSpace3D, [Vec3], Quantity) -- ^ used residual space, ackage, all translation vectors and used quantity or @Nothing@
                                                            -- if not a single item fits
fillQuantityMaybe (space, residual) axeOrder quantity
  | quantity == 0
  = Nothing
  | Just (es, vectors, possibleQuantity) <- fillResidual (space, residual) quantity axeOrder
  , not $ L.null vectors
  = Just (residual, es, vectors, possibleQuantity)
  | otherwise
  = Nothing



-- | Pick next residualspace for which a filling with a package succeeds or 'Nothing'
-- in case the package cannot be placed in any of the residual spaces.
pickNextResidual :: [ResidualSpace] -- ^ The residual spaces to be tried
              -> EuclideanSpace3D -- ^ A package
              -> AxeOrder -- ^ The fillorder to be used
              -> Quantity -- ^ The maximum amount of packages to be placed
              -> Maybe (ResidualSpace, EuclideanSpace3D, [Vec3], Quantity) -- ^ The picked residual space, used package, all translation vectors and used quantity
                                                                        -- or 'Nothing' if not a single package could be placed in any residual space
pickNextResidual rs p ao q = L.foldl (\b a -> b `mplus` fillQuantityMaybe (p, a) ao q) Nothing rs

-- | For every @q@ in [1..q_i] replicate incomplete selections of the
-- the specified filled residualspace
quantityReplicate :: (ResidualSpace, EuclideanSpace3D, [Vec3], Quantity) -- ^ The used residual space, untranslated package, translation vectors and quantity
               -> [(ResidualSpace, EuclideanSpace3D, [Vec3])] -- ^ A list with the same residual space, same untranslated package but selections of the translation vectors
                                                           -- for every q in [1..q]
quantityReplicate (r, e, vs, q) = fmap (\qi -> (r, e, take qi vs)) [1..q]

-- | Select the next fitting residual space that can contain
-- at least one package. Calculate a filled block for the given fill ordr with max
-- @q@ packages. Generate all subselections from 1..'q$ for that block.
-- If no single item fits in any of the residual spaces, return 'Nothing'.
placeNextCandidates :: LoadSpace -- ^ The used load space
                  -> [ResidualSpace] -- ^ All current residual spaces
                  -> AxeOrder -- ^ A fill order
                  -> EuclideanSpace3D -- ^ A package to be placed
                  -> Quantity -- ^ The package quantity
                  -> Maybe [(ResidualSpace, EuclideanSpace3D, [Vec3])] -- ^ All selections to place at least one package for the next fitting residual space
                                                                    -- (selections in the sense of partial selections of a filled block)
placeNextCandidates _ [] _ _ _ = Nothing
placeNextCandidates load residual order package quantity
  | quantity == 0
  = Nothing
  | not $ package `isInside` load
  = Nothing
  | otherwise
  = pickNextResidual residual package order quantity >>= Just . quantityReplicate

-- | Index all provided packages with the given index as 'IndexedSpace'.
-- | Also keep the provided residual spaces.
indexPackingStep :: Int -> ([EuclideanSpace3D], [ResidualSpace]) -> ([IndexedSpace], [ResidualSpace])
indexPackingStep i (es, rs) = (fmap (indexSpace i) es, rs)


-- | Expand a placement. For the given package index 'i',
--   residual spaces, and a packing candidate selection, index
--   and translate all packages. Also make sure the resulting new residuals
--   maintain the dominance/inclusion and order relation.
-- The following condition must be true at all times:
--
-- prop> r `elem` rs == True
expandPlacement :: Int  -- ^ The index of the package used
              -> LoadSpace
              -> [EuclideanSpace3D]
              -> [ResidualSpace] -- ^All current residual spaces 'rs'
              -> (ResidualSpace, EuclideanSpace3D, [Vec3])  -- ^ The picked residual space 'r', the untranslated package and translation vectors
              -> ([IndexedSpace], [ResidualSpace]) -- ^ The indexed packages and the resulting list of new residual spaces
expandPlacement i load spaces residualspaces (residual, package, translations) =
  indexPackingStep i (tes, splitFoldDominant residualspaces tes)
  where
    residualStart = start $ rspace residual
    packageSize = S.size package
    tes = fmap (\v -> package `translate` (residualStart &+ (packageSize `pointwise` v))) translations


modChromsomePackages :: ([ChromosomeType] -> [ChromosomeType]) -> PackingSelectors -> PackingSelectors
modChromsomePackages f (orientationCs, fillCs, selectCs) = (f orientationCs, f fillCs, f selectCs)

-- | Combine flat representation of an intermediate packing state.
-- This would be the location to implement a "failure as fast as possible" variant.
--
-- Conditions for 'Just' and 'Nothing' for this combination:
--
-- A packing order that can not even place its first package is considered useless:
-- 'isNothing' A => 'Nothing'
--
-- A packing order that did already place something will always remain valid
-- 'isJust' A && 'isNothing' B => 'Just A'
--
-- A packing order that was already valid and is to be extended will always be valid
-- 'isJust' A && 'isJust' B => 'Just A + B'
combinePlacements :: Maybe ([IndexedSpace], [ResidualSpace], PackingSelectors, ItemQuantityMap, Int)
                  -- ^ The currently placed packages, residual spaces, packing selecotrs, quantities and used chromosomes ('A')
               -> Maybe ([IndexedSpace], [ResidualSpace]) -- ^ To be added packages and to be replaced residual spaces ('B')
               -> Maybe ([IndexedSpace], [ResidualSpace], PackingSelectors, ItemQuantityMap, Int) -- ^ The resulting update of the state
combinePlacements Nothing _ = Nothing
--combinePlacements (Just _) Nothing = Nothing
-- A single chromosome was used, even though no extension happened
combinePlacements (Just (packages, residuals, chromosomePackages, qm, used)) Nothing =
  Just (packages, residuals, chromosomePackages, qm, succ used)
-- A single chromosome was used to extend the packing by a number of packages.
combinePlacements (Just (packages, _, chromosomePackages, oqm, used)) (Just (newPackages, newResiduals)) =
  Just (packages ++ newPackages, newResiduals, safeTail chromosomePackages, qm, succ used)
  where
    lengthNew = length newPackages
    qm = if L.null newPackages then oqm
         else M.adjust (\k -> k - lengthNew) (fst $ head newPackages) oqm
    safeTail = modChromsomePackages (drop 1)


-- | Select an element from a list by projecting a selector in [0.0, 1.0]
-- over the indices of the list. Eg. a selector of 1 will always select the last element,
-- 0.5 in the center and 0 at the front.
select01 :: (Fractional b, RealFrac b) => [a] -> b -> a
select01 ls i = ls !! idx
  where
   lenlist = length ls
   idx = floor $ minmax 0.0 1.0 i * fromIntegral (lenlist - 1)

-- | Retrieve the packing extension fora  given loadspace, an
-- index package and its quantity, a single chromosome package and the current
-- list of residual spaces.
pickNextPack :: LoadSpace -- ^ The loadspace used
          -> [EuclideanSpace3D]
          -> IndexedSpace -- ^ The index package and its space
          -> Quantity -- ^ The package quantity
          -> PackingSelector -- ^ The chromosome package for the package
          -> [ResidualSpace] -- ^ The current list of residual spaces
          -> Maybe ([IndexedSpace], [ResidualSpace]) -- ^ A packing extension, defined by a number of placed packages and a new list of residual spaces
pickNextPack load packed (idx, package) quantity (orientationSelect, fillOrder, candidateSelect) rs
  | orientation <- sizePermuteReduced package
  , pickedOrientation <- select01 orientation orientationSelect
  , pickedAxeOrder <- select01 allFillOrders fillOrder
  -- sort residual spaces first, then try to get a packing extension for the supplied
  -- orientation, fill order and candidate selector
  , Just cs <- placeNextCandidates load (L.sortBy startOrd rs) pickedAxeOrder pickedOrientation quantity
  , not $ L.null cs
  , selected <- select01 cs candidateSelect
  -- A packing extension could be found, now it needs to be indexed
  = Just $ expandPlacement idx load packed rs selected
  | otherwise
  = Nothing


-- | Try to place all packages on a loadspace using the provided
-- information in the PackingSelectors (the genome), all packages and their
-- quantities. This is the "start" of the heuristic packing algorithm.
-- In case no single item could be placed, even with skipping, the algorithm
-- returns 'Nothing', otherwise a packing in form of translated indexed packages,
-- the current residual spaces and the amount of chromosomes used.
placeAll :: LoadSpace -- ^ The loadspace
        -> PackingSelectors -- ^ The full genome
        -> [IndexedSpace] -- ^ Untranslated indexed packages
        -> ItemQuantities -- ^ The quantities for every packge
        -> Maybe ([IndexedSpace], [ResidualSpace], Int) -- ^ A packing with translated indexed packages,
        --  residual spaces and the amount of chromosomes used.
placeAll load chromosomePackages@(orientatioCs, fillCs, selectCs) ipackages iqs
-- Check prerequisites for algorithm, especially the size of the genome
  | length orientatioCs /= length fillCs ||
    length orientatioCs /= length selectCs
  = error $ "incorrectly sized genome lists (orientatioCs " ++
              show (length orientatioCs) ++ ", fillCs " ++
              show (length fillCs) ++ ", selectCs " ++
              show (length selectCs) ++ ")"
  | otherwise
  = extractData <$>
  L.foldl (\oldState ipackage -> do
      (packed, residual, (orientation, fillOrder, candidateSelect), qm, _) <- oldState
      -- check item quantity
      let q = qm ! fst ipackage
      -- skip already fully packed items
      if q == 0 then oldState
      else combinePlacements oldState $
           pickNextPack load (snd <$> packed) ipackage q
            (head orientation, head fillOrder, head candidateSelect)
            residual
    )
  -- The initial state. nothing placed, only the loadspace as residual,
  -- chromosomes unpacked and split, initialized quantities, and 0 chromosomes used.
  (Just ([], [mkResidualSpace load], chromosomePackages, M.fromList iqs, 0)) ipackages
  where extractData (placements, residuals, _, _, used) = (placements, residuals, used)
