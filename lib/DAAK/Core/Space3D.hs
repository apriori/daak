-----------------------------------------------------------------------------
--
-- Module      : Space3D
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
module DAAK.Core.Space3D where

import           Test.Framework

import           Control.Applicative
import           Control.Monad
import           DAAK.Core.Debug
import           Data.Function
import qualified Data.List                 as L
import           Data.Maybe
import qualified Data.Set                  as S
import           Data.Vect
import           Data.Vect.Float.Util.Dim3
import qualified Data.Vector               as V
import           Debug.Trace
import           Prelude                   hiding (recip)

data Axis = X | Y | Z deriving (Bounded, Enum, Show, Eq)
data PointAxis = PX | PY | PZ deriving (Bounded, Enum, Show, Eq)
data Direction = Direction Vec3 deriving (Show, Eq)

type ItemSpaces = [(Int, EuclideanSpace3D)]
type PlacementSpaces = [(Int, EuclideanSpace3D)]
type IndexedSpace = (Int, EuclideanSpace3D)
type ItemSpace = IndexedSpace
type PlacementSpace = IndexedSpace

crossProd :: [a] -> [a] -> [(a, a)]
crossProd a b = [(x, y) | x <- a, y <- b]

crossProdX :: Eq a => [a] -> [a] -> [(a, a)]
crossProdX a b = [(x, y) | x <- a, y <- b, x /= y]

boolMaybe :: Bool -> a -> Maybe a
boolMaybe True a = Just a
boolMaybe _ _ = Nothing

safeNormalize :: Vec3 -> Vec3
safeNormalize v = v &* (1.0 / max 0.001 (len v))

vAxis :: Axis -> Vec3 -> Float
vAxis X = vx
vAxis Y = vy
vAxis Z = vz

enumToList :: (Enum a, Bounded a) =>  [a]
enumToList = [minBound .. maxBound]



indexSpace :: Int -> EuclideanSpace3D -> IndexedSpace
indexSpace i e = (i, e)


class Space a where
  intersect :: a -> a -> a
  union :: a -> a -> a
  splitXY :: Int -> Int -> a
  isInside :: a -> a -> Bool
  translate :: a -> Vec3 -> a
  size :: a -> Vec3


absDeq :: Float -> Float -> Bool
absDeq a b = (absEq . abs) (a - b)

absEq :: Float -> Bool
absEq = (< 0.001)

instance Eq Vec3 where
  a == b = all (< 0.001) [x, y, z]
    where Vec3 x y z = mapVec abs $ a &- b

instance Eq Vec2 where
  a == b = all (< 0.001) [x, y]
    where Vec2 x y = mapVec abs $ a &- b


vx, vy, vz :: Vec3 -> Float
vx (Vec3 x _ _) = x
vy (Vec3 _ y _) = y
vz (Vec3 _ _ z) = z

data EuclideanSpace3D = EuclideanSpace3D !Vec3 !Vec3 | EmptySpace3D
                       deriving (Show)

instance Eq EuclideanSpace3D where
  EmptySpace3D == EmptySpace3D = True
  _ == EmptySpace3D = False
  EmptySpace3D == _ = False
  (EuclideanSpace3D a1 b1) == (EuclideanSpace3D a2 b2) = a1 == a2 && b1 == b2



instance Arbitrary EuclideanSpace3D where
  arbitrary = genSpaces


data Side = RIGHT | LEFT | FRONT | BACK | TOP | BOT deriving (Show, Eq, Enum, Bounded)


class Reciprocal a where
  recip :: a  -> a

instance Reciprocal Side where
  recip RIGHT = LEFT
  recip LEFT = RIGHT
  recip FRONT = BACK
  recip BACK = FRONT
  recip TOP = BOT
  recip BOT = TOP

isPointSideOfSpace :: EuclideanSpace3D -> Side -> Vec3 -> Bool
isPointSideOfSpace s@(EuclideanSpace3D l r) (RIGHT) p = vx p >= ex s
isPointSideOfSpace s@(EuclideanSpace3D l r) (LEFT) p = vx p <= sx s
isPointSideOfSpace s@(EuclideanSpace3D l r) (FRONT) p = vy p <= sy s
isPointSideOfSpace s@(EuclideanSpace3D l r) (BACK) p = vy p >= ey s
isPointSideOfSpace s@(EuclideanSpace3D l r) (TOP) p = vz p >= ez s
isPointSideOfSpace s@(EuclideanSpace3D l r) (BOT) p = vz p <= sz s
isPointSideOfSpace _ _ _ = False

isSpaceSideOfSpace :: EuclideanSpace3D -> Side -> EuclideanSpace3D -> Bool
isSpaceSideOfSpace a s b = isPointSideOfSpace a s (start b) && isPointSideOfSpace a s (end b)

vectorReplaceAxis :: Axis -> Vec3 -> Float -> Vec3
vectorReplaceAxis X s b = Vec3 b (vy s) (vz s)
vectorReplaceAxis Y s b = Vec3 (vx s) b (vz s)
vectorReplaceAxis Z s b = Vec3 (vx s) (vy s) b

nullVec3 :: Vec3
nullVec3 = Vec3 0 0 0

isAnyNegative :: Vec3 -> Bool
isAnyNegative (Vec3 a b c) = a < 0 || b < 0 || c < 0

isAnyZero :: Vec3 -> Bool
isAnyZero (Vec3 a b c) = a == 0 || b == 0 || c == 0

mkSizeSpace :: Float -> Float -> Float -> EuclideanSpace3D
mkSizeSpace l w h = mkEuclidean3D (Vec3 0 0 0) (Vec3 l w h)

mkEuclidean3D :: Vec3 -> Vec3 -> EuclideanSpace3D
mkEuclidean3D bfl tbr
  | isAnyNegative $ tbr &- bfl = EmptySpace3D
  | isAnyZero $ tbr &- bfl = EmptySpace3D
  | isAnyNegative bfl || isAnyNegative tbr = EmptySpace3D
  | otherwise = EuclideanSpace3D bfl tbr


genPositive0 :: (Arbitrary a, Num a, Ord a) => Gen a
genPositive0 = liftM abs arbitrary


genPositiveVec :: Gen Vec3
genPositiveVec = Vec3 <$> genPositive0 <*> genPositive0 <*> genPositive0

genSpaces :: Gen EuclideanSpace3D
genSpaces = do
 bfl <- genPositiveVec
 tbr <- liftA2 (&+) (return bfl) genPositiveVec
 return $ mkEuclidean3D bfl tbr

genSpacePair :: Gen (EuclideanSpace3D, EuclideanSpace3D)
genSpacePair = (,) <$> genSpaces <*> genSpaces

pairwise  :: Num a => (Float-> Float -> Float) -> Vec3 -> Vec3 -> Vec3
pairwise f (Vec3 a b c) (Vec3 a2 b2 c2) = Vec3 (f a a2) (f b b2) (f c c2)

minVec :: Vec3 -> Vec3 -> Vec3
minVec = pairwise min

maxVec :: Vec3 -> Vec3 -> Vec3
maxVec = pairwise max

instance Space EuclideanSpace3D where
   intersect _ EmptySpace3D = EmptySpace3D
   intersect EmptySpace3D _ = EmptySpace3D
   intersect a@(EuclideanSpace3D  l  r)
    b@(EuclideanSpace3D l1 r1)
             | isInside a b = a
             | isInside b a = b
             | otherwise = mkEuclidean3D (maxVec l l1) (minVec r r1)

   union a EmptySpace3D = a
   union EmptySpace3D b = b
   union a@(EuclideanSpace3D l r)
    b@(EuclideanSpace3D l1 r1)
             | isInside a b = b
             | isInside b a = a
             | otherwise = mkEuclidean3D (minVec l l1) (maxVec r r1)

   isInside EmptySpace3D _ = False
   isInside _ EmptySpace3D = False
   isInside (EuclideanSpace3D (Vec3 asx asy asz) (Vec3 aex aey aez))
            (EuclideanSpace3D (Vec3 bsx bsy bsz) (Vec3 bex bey bez))
               = asx >= bsx && asx <= bex &&
                 asy >= bsy && asy <= bey &&
                 asz >= bsz && asz <= bez &&
                 aex >= bsx && aex <= bex &&
                 aey >= bsy && aey <= bey &&
                 aez >= bsz && aez <= bez


   splitXY _ _ = EmptySpace3D
   translate EmptySpace3D _ = EmptySpace3D
   translate (EuclideanSpace3D s e) v = mkEuclidean3D (s &+ v) (e &+ v)
   size EmptySpace3D = Vec3 0 0 0
   size (EuclideanSpace3D s e )= e &- s

ex, ey, ez :: EuclideanSpace3D -> Float
ex = vx . end
ey = vy . end
ez = vz . end

sx, sy, sz :: EuclideanSpace3D -> Float
sx = vx . start
sy = vy . start
sz = vz . start

dx, dy, dz :: EuclideanSpace3D -> Float
dx EmptySpace3D = 0
dx (EuclideanSpace3D l r) = vx l - vx r
dy EmptySpace3D = 0
dy (EuclideanSpace3D l r) = vy l - vy r
dz EmptySpace3D = 0
dz (EuclideanSpace3D l r) = vz l - vz r

sizex, sizey, sizez :: EuclideanSpace3D -> Float
sizex = vx . size
sizey = vy . size
sizez = vz . size

volume :: EuclideanSpace3D -> Float
volume s = sizex s * sizey s * sizez s

baseArea :: EuclideanSpace3D -> Float
baseArea s = sizex s * sizey s

start :: EuclideanSpace3D -> Vec3
start EmptySpace3D = Vec3 0 0 0
start (EuclideanSpace3D s _) = s

end :: EuclideanSpace3D -> Vec3
end EmptySpace3D = Vec3 0 0 0
end (EuclideanSpace3D _ e) = e

center :: EuclideanSpace3D -> Vec3
center EmptySpace3D = Vec3 0 0 0
center s@(EuclideanSpace3D l r) = l &+ (size s &* 0.5)

prop_union (a, b) = (a == EmptySpace3D || b == EmptySpace3D) || a `union` b /= EmptySpace3D


spaceCorners :: EuclideanSpace3D -> [Vec3]
spaceCorners s = [ Vec3 (sx s) (sy s) (sz s)
                 , Vec3 (sx s) (ey s) (sz s)
                 , Vec3 (ex s) (sy s) (sz s)
                 , Vec3 (ex s) (ey s) (sz s)
                 , Vec3 (sx s) (sy s) (ez s)
                 , Vec3 (sx s) (ey s) (ez s)
                 , Vec3 (ex s) (sy s) (ez s)
                 , Vec3 (ex s) (ey s) (ez s) ]



spaceHasCorner :: EuclideanSpace3D -> Vec3 -> Bool
spaceHasCorner s v = v `L.elem` spaceCorners s

pointInSpace :: EuclideanSpace3D -> Vec3 -> Bool
pointInSpace EmptySpace3D _ = False
pointInSpace s@(EuclideanSpace3D (Vec3 lx ly lz) (Vec3 rx ry rz)) v@(Vec3 x y z)
  = ((x >= lx) && (x <= rx)) &&
    ((y >= ly) && (y <= ry)) &&
    ((z >= lz) && (z <= rz))


pointInSpaceNoCorner :: EuclideanSpace3D -> Vec3 -> Bool
pointInSpaceNoCorner EmptySpace3D _ = False
pointInSpaceNoCorner s@(EuclideanSpace3D (Vec3 lx ly lz) (Vec3 rx ry rz)) v@(Vec3 x y z)
  = ((x > lx) && (x < rx)) &&
    ((y > ly) && (y < ry)) &&
    ((z > lz) && (z < rz))

overlap :: EuclideanSpace3D -> EuclideanSpace3D -> Bool
overlap a b = not $ any (flip (isSpaceSideOfSpace a) b) enumToList

noverlap :: EuclideanSpace3D -> EuclideanSpace3D -> Bool
noverlap i k = not $ overlap i k

raycastFace :: Direction -> Vec3 -> (Float, Axis)-> (Float, Float)-> (Float, Axis)
raycastFace (Direction dir) o (li, ax) (l, r) = (mg, sax)
  where mg = min (max gl gr) li
        g b = (b - vAxis ax o) / vAxis ax dir
        gl = g l
        gr = g r
        sax = if ax /= Z then succ ax else ax

boundPairs :: EuclideanSpace3D -> [(Float, Float)]
boundPairs sa = [(sx sa, ex sa), (sy sa, ey sa), (sz sa, ez sa)]

pointProject :: Vec3 -> Direction -> Float -> EuclideanSpace3D -> Vec3
pointProject o (Direction d) i sa = o &+ (d &* (fst $ foldl (raycastFace (Direction d) o) (i, X) $
                                                         boundPairs sa))

centerProjectPoint :: EuclideanSpace3D -> EuclideanSpace3D -> Vec3
centerProjectPoint sa sb = pointProject o (Direction d) cda sa
  where o = center sa
        dc = center sb &- center sa
        d = safeNormalize dc
        cda = sizex sa * sizey sa * sizez sa

collisionAxisPoints :: EuclideanSpace3D -> EuclideanSpace3D -> (Vec3, Vec3)
collisionAxisPoints sa sb = (centerProjectPoint sa sb, centerProjectPoint sb sa)

contacted :: EuclideanSpace3D -> EuclideanSpace3D -> Bool
contacted a b = intersect a b == EmptySpace3D &&
                 cb == ca
  where (ca, cb) = collisionAxisPoints a b

partiallyOutside :: EuclideanSpace3D -> EuclideanSpace3D -> Bool
partiallyOutside a b = any (not . pointInSpace a) $ spaceCorners b

sideAxisVec :: Side -> Vec3
sideAxisVec LEFT = neg vec3X
sideAxisVec RIGHT = vec3X
sideAxisVec FRONT = neg vec3Y
sideAxisVec BACK = vec3Y
sideAxisVec TOP = vec3Z
sideAxisVec BOT = neg vec3Z

sameSide :: Normal3 -> Vec3 -> Bool
sameSide n v
  | v == nullVec3
  = True
  | otherwise
  = n &. toNormalUnsafe v >= 0

pointSideOfPoint :: Vec3 -> Side -> Vec3  -> Bool
pointSideOfPoint a s b = sameSide (toNormalUnsafe $ sideAxisVec s) $ b &- a

centerDistSqr :: EuclideanSpace3D -> EuclideanSpace3D -> Float
centerDistSqr a b = lensqr $ center a &- center b

centerDist :: EuclideanSpace3D -> EuclideanSpace3D -> Float
centerDist a b = len $ center a &- center b

sizePermute :: EuclideanSpace3D -> [EuclideanSpace3D]
sizePermute a = (\[x, y, z] -> mkSizeSpace x y z `translate` start a) <$> L.permutations sp
  where sp = [sizex a, sizey a, sizez a]

sizePermuteReduced :: EuclideanSpace3D -> [EuclideanSpace3D]
sizePermuteReduced = L.nub . sizePermute

boundingBox :: [EuclideanSpace3D] -> EuclideanSpace3D
boundingBox = L.foldl union EmptySpace3D

surfaceArea :: EuclideanSpace3D -> Float
surfaceArea s = sizex s * sizey s
