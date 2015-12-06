-----------------------------------------------------------------------------
--
-- Module      : PackRendering
-- Copyright   : Armin Kazmi (2015)
-- License     : MIT
--
-- Maintainer  : Armin Kazmi
-- Stability   : experimental
-- Portability : portable
--
-- | The converter module dealing with 'EuclideanSpace3D' and respective xml
-- representations.
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import           Test.Framework

import           Vis
import           Vis.Camera                          (Camera (..),
                                                      cameraKeyboardMouse,
                                                      cameraMotion, makeCamera)
import           Vis.Vis

import           Control.DeepSeq
import           Control.Parallel.Strategies
import           DAAK.Algorithms.Gamo
import           Data.Map                            as M

import           Control.Monad
import           Control.Monad.Identity
import           DAAK.Algorithms.Gamo.Criteria
import           DAAK.Algorithms.Gamo.JsonImport
import           DAAK.Algorithms.Gamo.Packing
import           DAAK.Core.Debug
import           DAAK.Core.Random.MT
import           DAAK.Core.ResidualSpace
import           DAAK.Core.Space3D                   as S
import           DAAK.Core.Utilities
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.IORef
import           Data.List                           as LI
import           Data.Maybe
import           Data.Vect
import           Data.Word
import           GHC.Float
import qualified Graphics.UI.GLUT                    as GLUT
import           Linear                              hiding (normalize)
import           Moo.GeneticAlgorithm.Continuous
import           Moo.GeneticAlgorithm.Multiobjective
import           Moo.GeneticAlgorithm.Run
import           System.Environment
import           System.IO
import           System.Random.Mersenne.Pure64

data SimulationState a = SimulationState ProblemDescription PureMT (PopulationState a) Int

modAlpha :: Float -> Color -> Color
modAlpha a c = rawColor r g b a
  where (r, g, b, _ ) = rgbaOfColor c

colorTable :: [Color]
colorTable = cycle
  [
  -- makeColor8 215 228 246 255
  --  makeColor8 152 186 228 255
  --makeColor8 79 129 193 255
   makeColor8 51 90 147 255
  --, makeColor8 43 58 97 255
  --, makeColor8 197 192 224 255
  , makeColor8 140 131 186 255
  , makeColor8 64 55 135 255
  , makeColor8 52 43 102 255
  , makeColor8 194 14 27 255
  , makeColor8 137 39 37 255
  , makeColor8 104 35 47 255
  , makeColor8 239 202 144 255
  , makeColor8 220 144 37 255
  , makeColor8 208 101 28 255
  , makeColor8 249 240 137 255
  , makeColor8 245 220 54 255
  , makeColor8 237 193 47 255
  , makeColor8 211 221 137 255
  , makeColor8 182 201 50 255
  , makeColor8 149 165 54 255
  , makeColor8 110 117 73 255
  , makeColor8 190 219 212 255
  , makeColor8 137 190 172 255
  , makeColor8 86 151 121 255
  , makeColor8 195 175 121 255
  , makeColor8 66 47 28 255
  , makeColor8 85 99 108 255
  , makeColor8 109 183 203 255
  , makeColor8 63 110 146 255
  ]

spaceToBox :: (EuclideanSpace3D, Color) -> Flavour -> VisObject Float
spaceToBox (s, c) f =
  Trans (V3 (sx s + vx hs) (sy s + vy hs) (sz s+ vz hs)) $
  Box (sizex s, sizey s, sizez s) f c
  where hs = S.size s &* 0.5

bestWorstPack :: ProblemDescription
              -> Population Double
              -> ((Maybe ([IndexedSpace], [ResidualSpace]), Maybe Packing), (Maybe ([IndexedSpace], [ResidualSpace]), Maybe Packing))
bestWorstPack pd ps = ((liftM deconstructPacking bestPack, bestPack), (liftM deconstructPacking worstPack, worstPack))
  where
    bestList = bestFirst Minimizing ps
    pack = packing pd . fst
    bestPack =  pack $ head bestList
    worstPack = pack $ last bestList

colorZip :: IndexedSpace -> (EuclideanSpace3D, Color)
colorZip space = (snd space, colorTable !! fst space)

modResidual :: ResidualSpace -> ResidualSpace
--modResidual EmptyResidualSpace = EmptyResidualSpace
--modResidual (ResidualSpace EmptySpace3D) = EmptyResidualSpace
--modResidual (ResidualSpace space) = ResidualSpace $ mkSizeSpace (sizex space) (sizey space) 10 `translate` (start space)
modResidual = id

drawFunc :: SimulationState Double -> VisObject Float
drawFunc (SimulationState pd _ (Left _) _) = VisObjects []
drawFunc (SimulationState pd@(ProblemDescription l _ _) _ (Right ps) gen) =
  VisObjects
    [ Scale (1/100.0, 1/100.0, 1/100.0) $ VisObjects $ [axes] ++ boxesBest ++ boxesoutlinesBest ++ [confinebox]  -- ++ residualBoxesBest
    , Scale (1/100.0, 1/100.0, 1/100.0) $ Trans trans $ VisObjects $ [axes] ++ boxesWorst ++ boxesoutlinesWorst ++ [confinebox] -- ++ residualBoxesWorst
    , Text2d ("Gen: " ++ show gen ++ "    best: " ++ show mopsBest ++ "     worst: " ++ show mopsWorst) (10, 10) Fixed9By15 $ makeColor8 255 0 0 255
    ]
  where
    resColor = makeColor8 125 125 125 125
    trans = V3 (sizex l * 2) 0 0
    confinebox = spaceToBox (l, red) Wireframe
    (best, bestPack) = fst $ bestWorstPack pd ps
    (worst, worstPack) = snd $ bestWorstPack pd ps
    (spacesBest, residualsBest) = fromMaybe ([], []) best
    (spacesWorst, residualsWorst) = fromMaybe ([], []) worst
    mopsBest = evalMop bestPack (mop pd)
    mopsWorst = evalMop worstPack (mop pd)
    boxesBest = flip spaceToBox Solid . colorZip <$> spacesBest
    -- residualBoxesBest = flip spaceToBox Solid <$> zip (rspace . modResidual <$> residualsBest) (repeat resColor)
    residualBoxesBest = flip spaceToBox Solid <$> zip (rspace . modResidual <$> residualsBest) (modAlpha 0.5 <$> colorTable)
    boxesoutlinesBest = flip spaceToBox Wireframe <$> zip (snd <$> spacesBest) (repeat black)
    boxesWorst = flip spaceToBox Solid . colorZip <$> spacesWorst
    boxesoutlinesWorst = flip spaceToBox Wireframe <$> zip (snd <$> spacesWorst) (repeat black)
    -- residualBoxesWorst = flip spaceToBox Solid <$> zip (rspace . modResidual <$> residualsWorst) (repeat resColor)
    residualBoxesWorst = flip spaceToBox Solid <$> zip (rspace . modResidual <$> residualsWorst) (modAlpha 0.5 <$> colorTable)
    axes = Axes (0.5, 15)

drawFuncIO :: SimulationState Double -> IO (VisObject Float)
drawFuncIO = return . drawFunc

simFunc :: Float -> SimulationState Double -> SimulationState Double
simFunc _ state@(SimulationState p mt ps gen) =
  case runRandom (step p mop cond ps) mt of
    (StopGA _, _) -> state
    (ContinueGA newpop, mt') -> SimulationState p mt' (Right newpop) (gen +1)
  where
    --cond = Or (Generations generations) stalledFun
    cond = Generations generations

simFuncIO :: Float -> SimulationState Double -> IO (SimulationState Double)
simFuncIO a b = return $ simFunc a b

defaultCamera :: Camera0
defaultCamera = Camera0 0 (-20) 7

setCamera :: Camera -> IO ()
setCamera camera = GLUT.lookAt (GLUT.Vertex3 xc yc zc)
                                (GLUT.Vertex3 x0 y0 z0)
                                (GLUT.Vector3 0 0 1)
  where
    V3 x0 y0 z0 = pos camera
    phi'   = phi   camera
    theta' = theta camera
    rho'   = rho   camera
    xc = x0 + rho'*cos(phi'*pi/180)*cos(theta'*pi/180)
    yc = y0 + rho'*sin(phi'*pi/180)*cos(theta'*pi/180)
    zc = z0 - rho'*sin(theta'*pi/180)


-- | run a simulation impurely
simulateIOCam :: Real b =>
                 Options -- ^ user options
              -> Double  -- ^ sample rate
              -> world   -- ^ initial state
              -> (world -> IO (VisObject b))  -- ^ draw function
              -> (Float -> world -> IO world) -- ^ state propogation function (takes current time and state as inputs)
              -> IO ()
simulateIOCam opts tsa userState0 userDrawFun userSimFun =
  vis opts tsa (userState0, cameraState0) simFun drawFun setCameraFun (Just kmCallback) (Just motionCallback) Nothing
  where
    drawFun ((userState, _),_) = do
      obs <- userDrawFun userState
      return (obs, Nothing)
    simFun ((userState,cameraState),time) = do
      nextUserState <- userSimFun time userState
      return (nextUserState, cameraState)
    cameraState0 = makeCamera $ fromMaybe defaultCamera (optInitialCamera opts)
    kmCallback (state, camState) k0 k1 _ _ = (state, cameraKeyboardMouse camState k0 k1)
    motionCallback (state, cameraState) posc = (state, cameraMotion cameraState posc)
    setCameraFun (_,cameraState) = setCamera cameraState


keyHandler :: SimulationState Double -> GLUT.Key -> GLUT.KeyState -> SimulationState Double
keyHandler s _ _ = s


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

-- | Stalled function.  This function shall return objects whichs occurence is being
-- counted for an estimate of stallness. Say the returned list remains absolutely
-- identical, then after a set amount of generations this is considered a stall.
-- This function merely returns all objective values. A more sophisticated algorithm
-- to detect stallness is not needed.
stalledFun :: Int -> MultiObjectiveProblem a -> [Objective] -> [Objective]
stalledFun _ _ vals = vals

-- | The declared mutliple objective problem which evaluates the usefullness
-- of a given packing. Currently active functions are 'filling2DRate' and 'heightVariance'
-- with maximize and minimize respectively.
mop :: ProblemDescription -> MultiObjectiveProblem (Maybe Packing -> Double)
mop p@(ProblemDescription l es _) =
  [
--   (Maximizing, (maybe 0.0 packedItems))
--   (Minimizing, (maybe (float2Double $ ez l) packingMaxZ))
--    (Minimizing, maybe (float2Double $ volume l) freeVolume)
   --(Minimizing, maybe 1e20 fragmentation)
    (Maximizing, maybe 0 filling2DRate)
--  , (Maximizing, maybe 0 fillingRate)
  , (Minimizing, maybe (float2Double $ S.ez l) (heightVariance p))
  , (Maximizing, maybe 0 accDensity)
  --, (Minimizing, (fromIntegral . countOverlaps . packedSpaces))
  -- , (Minimizing, (maybe (float2Double $ lensqr $ S.size l) groupingDistanceAverage))
--  , (Maximizing, maybe 0 heightGroupBiggestSupportCrit)
  ]

packedSpaces :: Maybe Packing -> [EuclideanSpace3D]
packedSpaces = maybe [] (fmap snd . fst . deconstructPacking)

bool01 :: Bool -> Int
bool01 True = 1
bool01 False = 1

countOverlaps :: [EuclideanSpace3D] -> Int
countOverlaps ps = sum (bool01 . uncurry overlap <$>  zip ps ps) - length ps


playSimulateIO :: SimulationState Double -> EuclideanSpace3D -> IO ()
playSimulateIO s confines =
  playIO opts 0.35 (s, cameraState0) drawFun simFun setCameraFun (Just kmCallback) (Just motionCallback) Nothing
  where
    drawFun (state, _) = do
     obs <- drawFuncIO state
     return (obs, Nothing)
    simFun time (userState, cameraState) = do
     nextUserState <- simFuncIO time userState
     return (nextUserState, cameraState)

    opts = defaultOpts { optWindowName = "simulate test"
           , optInitialCamera = Just defaultCamera
           , optBackgroundColor = Just $ makeColor 0.0 0.0 0.0 0.0
           , optAntialiasing = Smoothed
           }
    cameraState0 = makeCamera $ fromMaybe defaultCamera (optInitialCamera opts)
    kmCallback (state, camState) k0 k1 _ _ = (keyHandler state k0 k1, cameraKeyboardMouse camState k0 k1)
    motionCallback (state, cameraState) posc = (state, cameraMotion cameraState posc)
    setCameraFun (_,cameraState) = setCamera cameraState

createInitialState :: PureMT -> ProblemDescription -> SimulationState Double
createInitialState mt p = SimulationState p mt' (Left genomes0) 0
  where (genomes0, mt') = runRandom (initialize popsize p) mt

-- | Usage function, outputting the usage of the program
usage :: IO ()
usage = putStrLn "usage: gamo filename [seed]"


liveAlgo :: PureMT -> CompleteProblem -> IO ()
liveAlgo mt (CompleteProblem _ p@(ProblemDescription l _ _ )) =
  let state = createInitialState mt p in
  playSimulateIO state l
liveAlgo _ _ = return ()

runLiveAlgo :: PureMT -> FilePath -> IO ()
runLiveAlgo mt p = eitherDecodeFile p >>= either putStrLn (liveAlgo mt)

-- | The main function. Currently ouput of the algorithm is always written to "test.xml".
main = do
  args <- getArgs
  case () of _
              | length args < 1 -> usage
              | length args == 2 -> do
                let seed = read $ last args
                    mt = pureMT seed
                runLiveAlgo mt (head args)
              | otherwise -> do
                (mt, seed) <- newPureMTGetSeed
                runLiveAlgo mt (head args)
