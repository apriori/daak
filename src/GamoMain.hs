-----------------------------------------------------------------------------
--
-- Module      : GamoMain
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
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import           Control.DeepSeq
import           Control.Parallel.Strategies
import           DAAK.Algorithms.Gamo
import           DAAK.Core.Puzzle.Convert.Converters
import           DAAK.Core.Puzzle.PuzzleOrderDocument          as PO
import           DAAK.Core.Puzzle.PuzzleOrderDocumentInstances
import           DAAK.Core.Puzzle.XmlTransform
import           DAAK.Core.Space3D                             as S
import           Data.Map                                      as M
import           Data.Maybe                                    as MY
import           Data.Vect
import           Test.Framework

import           Control.Monad
import           Control.Monad.Identity
import           DAAK.Algorithms.Gamo.Criteria
import           DAAK.Algorithms.Gamo.Packing
import           DAAK.Core.Debug
import           DAAK.Core.Random.MT
import           DAAK.Core.ResidualSpace
import           DAAK.Core.Utilities
import           Data.Either
import           Data.Function
import           Data.IORef
import           Data.List                                     as LI
import           Data.Word
import           Debug.Trace
import           GHC.Float
import           Moo.GeneticAlgorithm.Continuous
import           Moo.GeneticAlgorithm.Multiobjective
import           Moo.GeneticAlgorithm.Run
import           System.Environment
import           System.IO
import           System.Random.Mersenne.Pure64

generations = 500
maxsolutions = 20


-- | Deconstruct a packing into a tupel of placements lists and residual spaces lists
deconstructPacking :: Packing -> (PlacementSpaces, [ResidualSpace])
deconstructPacking (EmptyPacking _) = ([], [])
deconstructPacking (Packing _ is rs _) = (is, rs)

-- | Do two packing have the same placements?
-- Note that the placements are sorted by 'startOrdSpace' prior to comparison.
samePlacements :: Packing -> Packing -> Bool
samePlacements (EmptyPacking _) (EmptyPacking _) = True
samePlacements (Packing{}) (EmptyPacking _) = False
samePlacements (EmptyPacking _) (Packing{}) = False
samePlacements (Packing _ ais _ _) (Packing _ bis _ _) =
  sortBy (startOrdSpace `on` snd) ais == sortBy (startOrdSpace `on` snd) bis

-- | Evaluate a multiple objective problem on a packing and return all
-- evaluated criteria values in a list
evalMop :: Maybe Packing -> MultiObjectiveProblem (Maybe Packing -> Double) -> [Double]
evalMop Nothing _ = []
evalMop p mopf = fmap ((\f -> f p) . snd) mopf

-- | Construct a string for a given list of values using the respective 'Show' instance.
-- Seperate every item in the list by a whitespace.
spaceString :: Show a => [a] -> String
spaceString = concatMap (flip (++) " " . show)

-- | For a given 'ProblemDescription' and a 'MultiObjectiveProblem' and a generation number
-- print the current worst and best solutions criteria values for the 'MultiObjectiveProblem'.
showBest :: ProblemDescription -> MultiObjectiveProblem (Maybe Packing -> Double) -> Int -> Population Double -> IO ()
showBest pd mopf i ps = putStrLn $ show i ++ " " ++ spaceString valBest ++ spaceString valWorst
  where bestList = bestFirst Minimizing ps
        bestPack = packing pd $ fst $ head bestList
        worstPack = packing pd $ fst $ last bestList
        valBest = evalMop bestPack mopf
        valWorst= evalMop worstPack mopf
        delta = zipWith (-) valBest valWorst


-- | Stalled function.  This function shall return objects whichs occurence is being
-- counted for an estimate of stallness. Say the returned list remains absolutely
-- identical, then after a set amount of generations this is considered a stall.
-- This function merely returns all objective values. A more sophisticated algorithm
-- to detect stallness is not needed.
stalledFun :: Int -> MultiObjectiveProblem a -> [Objective] -> [Objective]
stalledFun pop objs vals = vals
--  | length vals /= pop * length objs
--  = error $ "invalid objective array size, is " ++ show vals
--  | otherwise
--  = all (\a -> minddif a == 0) $ traceShowSId "breaks: " transBreaks
--  where
--    transBreaks = LI.transpose $ breakList vals $ length objs
--    minddif a = maximum a - minimum a'


-- | Helper function to run the entire algorithm in the 'IO' monad.
runIO' :: PureMT
      -> Rand [Genome a] -- ^ function to create initial population
      -> (IORef PureMT -> [Genome a] -> IO (Population a)) -- ^ genetic algorithm, see also 'loopIO'
      -> IO (Population a) -- ^ final population
runIO' rng initialize gaIO = do
  let (genomes0, rng') = runRandom initialize rng
  rngref <- newIORef rng'
  gaIO rngref genomes0

-- | The declared mutliple objective problem which evaluates the usefullness
-- of a given packing. Currently active functions are 'filling2DRate' and 'heightVariance'
-- with maximize and minimize respectively.
mop :: ProblemDescription -> MultiObjectiveProblem (Maybe Packing -> Double)
mop p@(ProblemDescription l es _) =
  [
--    (Maximizing, (maybe 0.0 packedItems))
--   (Minimizing, (maybe (float2Double $ ez l) packingMaxZ))
--    (Minimizing, maybe (float2Double $ volume l) freeVolume)
--   (Minimizing, (maybe 1e20 fragmentation)),
    (Maximizing, maybe 0 filling2DRate)
  -- (Maximizing, maybe 0 fillingRate)
  , (Minimizing, maybe (float2Double $ ez l) (heightVariance p))
  --, (Maximizing, maybe 0 accDensity)
--  , (Minimizing, (maybe (float2Double $ lensqr $ S.size l) groupingDistanceAverage))
--  , (Maximizing, maybe 0 heightGroupBiggestSupportCrit)
  ]


-- | The primary algorithm function. For the given RNG, loadspace and packages
-- run the genetic algorithm and return an amount of `maxsolutions` solutions,
-- with 50% best and 50% worst (in that order).
algo :: (PureMT, Word64) -- ^ The mersenne twister RNG to use (and its seed), see 'newPureMTGetSeed'
      -> LoadSpace -- ^ The loadspace to load packages on
      -> ItemSpaces -- ^ The indexed (untranslated) packages to use
      -> ItemQuantities -- ^ And their respective quantities
      -> IO ([PlacementSpaces], [[ResidualSpace]]) -- ^ The whole solution set after the optimization (entire population)
                                                    -- with all placements (translated indexed packages) and residual space lists
algo (gen, seed) load items itemQuantities =
  do
     -- output some information about the setup (and yes, this is sloppy)
     mapM_ (hPutStrLn stderr)
     [ "algo setup"
     , "items: " ++ show (itemsCount itemQuantities)
     , "itemtypes: " ++ show (length itemQuantities)
     , "replspaces: " ++ show (length replSpaces)
     , "total volume (3D): " ++ show totalVolume3D
     , "total volume (2D): " ++ show totalVolume2D
     , "load volume (3D): " ++ show (volume load)
     , "load volume (2D): " ++ show (surfaceArea load)
     , "volume overestimation (3D): " ++ show (totalVolume3D / float2Double (volume load * 100))
     , "volume overestimation (2D): " ++ show (totalVolume2D / float2Double (surfaceArea load * 100))
     , "seed is: " ++ show seed
     , "the problem is : " ++ show p
     ]
     putStrLn $ "seed is: " ++ show seed

     let act i ps = putStrLn $ "generation is " ++ show i ++ " population size is " ++ show (length ps)
         validGenes _ ps = putStrLn $ "valid packs: " ++  show (length $ catMaybes $ parMap rdeepseq (packing p . fst) ps)

    -- run the genetic algorithm and output the current best and worst objective values
    -- in for every generation
     result <- runIO' gen (initialize popsize p) $
               loopIO [DoEvery 1 $
               showBest p (mop p)] (Or (Generations generations) stalled) (step p mop)

     let
      -- best first sorting (according to internal NSGA2 ranking)
      solutions = takeGenome <$> bestFirst Minimizing result
      -- there might still be completely identital solutions - get rid of them
      packings = nubBy samePlacements $ catMaybes $ fmap (packing p) solutions

      -- alternatively augment packings with a residual space that shows the biggest support space
      --packings = take maxsolutions $ nubBy samePlacements $ catMaybes $ fmap (packing p >=> maxSupportResidualPacking) solutions

      -- split amount of demanded solutions in 50% best and 50% worst
      packings_best = take (maxsolutions `div` 2) packings
      packings_worst = take (maxsolutions `div` 2) $ reverse packings
      allPackings = packings_best ++ packings_worst
     return $ unzip $ fmap deconstructPacking allPackings
  where
    p = ProblemDescription load items itemQuantities
    -- stallness function asks for no change in the generations objective values for 50 generatinos
    stalled = GensNoChange 50 (stalledFun popsize (mop p)) Nothing
    -- the replicated packages according to their quantity
    replSpaces = qutantityReplicateItems items itemQuantities
    totalVolume3D = sum $ (float2Double . volume . snd)  <$> replSpaces
    totalVolume2D = sum $ (float2Double . surfaceArea . snd)  <$> replSpaces


-- | A wrapper over 'algo' to be supplied with XML representation types for packages and
-- order lines.
runAlgo:: (PureMT, Word64) -- ^ The mersenne twister RNG and its seed
       -> Map String Item  -- ^ A map from an item name to an Item (the xml reprsentation of a package)
       -> (String, LoadSpace) -- ^ The loadspace name and LoadSpace xml representation
       -> [OrderLineType] -- ^ The order lines (xml representation)
       -> IO ([LoadType], [ItemType], [OrderLineType])
          -- ^ the solutions of the algorithm as defined in 'algo', just with xml
          -- representation types instead of internally used types for calculation.
runAlgo genConf itemMap (loadName, load) orders =
  do
    -- run the algo, and split results in placements and residual space lists
    (places, residuals) <- algo genConf load indexItems quantityList
    let
      lp = length places
      lr = length residuals
      -- running numbers for all residuals
      lidx = backpermuteList residuals [0 :: Int ..]
      -- flatten list of list in list
      allResiduals = concat residuals
      -- construct fake packages for all residual spaces
      (rItemTypes, _, rorder, ritems) = wholeResidualData [0 :: Int ..] allResiduals
    return
      ( take maxsolutions $
          zipWith3 (\ipackages residual residualNums -> createLoad loadName $
                  -- also add residual space packages (inactive)
--                itemsFromResidual residual residualNums ++
                  -- pack permute orientations by comparing the base package and the rotate one
                  backpermuteItemsOrientation itemMap ipackages )
--        places places lidx
          places residuals lidx
--        places places places
      , rItemTypes
      , rorder ++ allOrders
      )
  where
    -- i with ep marker item type
    allOrders = orders
    -- ugly code to create a map from item name to its quantity
    quantityMap = M.fromList $ fmap (\ot -> (toString $ _orderLineType_itemId ot, fromIntegral $ _orderLineType_quantity ot)) orders
    -- extract keyed items, indexed starting with 0
    indexItems = zipWith (\idx item -> (idx, itemSpace item)) [0..] (elems itemMap)
    -- extract item quantities
    quantityList = zip [0 :: Int ..] $ (quantityMap ! ) <$> keys itemMap


-- | Usage function, outputting the usage of the program
usage :: IO ()
usage = putStrLn "usage: gamo filename [seed]"

-- | The main function. Currently ouput of the algorithm is always written to
-- "test.xml".
main = do
  args <- getArgs
  case () of _
              | length args < 1 -> usage
              | length args == 2 -> do
                let seed = read $ last args
                processDoc (runAlgo (pureMT seed, seed)) (head args) "test.xml"
              | otherwise -> do
                gen <- newPureMTGetSeed
                processDoc (runAlgo gen) (head args) "test.xml"
