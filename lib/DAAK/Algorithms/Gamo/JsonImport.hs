{-# LANGUAGE OverloadedStrings #-}

module DAAK.Algorithms.Gamo.JsonImport where

import           Control.Monad
import           DAAK.Algorithms.Gamo.Packing
import           DAAK.Core.Space3D
import           Data.Aeson
import           Data.ByteString.Lazy         as BS hiding (zip)
import           Data.Map                     as M
import           Data.Text                    hiding (zip)
import           Data.Vect
import qualified Data.Vector                  as V

data LoadItem = LoadItem
              { itemName  :: Text
              , quantity  :: Int
              , itemSize  :: Vec3
              , positions :: Maybe [Vec3]
              }
              deriving (Show)
data Load = Load
          { loadSpaceSize :: Vec3
          }
          deriving (Show)

data Document = Document
              { load  :: Load
              , items :: [LoadItem]
              }

instance FromJSON Vec3 where
  parseJSON (Array a)
    | V.length a == 3
    , vals <- fmap parseJSON a
    = Vec3 <$> (vals V.! 0) <*> (vals V.! 1) <*> (vals V.! 2)
    | otherwise
    = mzero
  parseJSON _ = mzero

instance ToJSON Vec3 where
  toJSON (Vec3 x y z) = toJSON [x, y, z]


instance FromJSON LoadItem where
  parseJSON (Object v) = LoadItem <$>
                          v .: "itemName" <*>
                          v .: "quantity" <*>
                          v .: "itemSize" <*>
                          v .:? "positions"
  parseJSON _ = mzero

instance ToJSON LoadItem where
  toJSON (LoadItem n q i p) = object [ "itemName" .= n
                                      , "quantity" .= q
                                      , "itemSize" .= i
                                      , "positions" .= p
                                      ]

instance FromJSON Load where
  parseJSON (Object v) = Load <$> v .: "loadSpaceSize"
  parseJSON _ = mzero

instance ToJSON Load where
  toJSON (Load s) = object [ "loadSpaceSize" .= s ]


instance FromJSON Document where
  parseJSON (Object v) = Document <$> v .: "load" <*> v .: "items"
  parseJSON _ = mzero

instance ToJSON Document where
  toJSON (Document l i) = object [ "load" .= l, "items" .= i ]

data CompleteProblem = CompleteProblem ItemQuantityMap ProblemDescription deriving (Show)

extractProblem :: Document -> CompleteProblem
extractProblem (Document l i) =
  CompleteProblem itemMap $ ProblemDescription (mkSizeSpaceVec $ loadSpaceSize l) indexedItems quantities
  where
    mkSizeSpaceVec (Vec3 l w h) = mkSizeSpace l w h
    indexedItems = zip [0..] $ mkSizeSpaceVec . itemSize <$> i
    quantities = zip [0..] $ quantity <$> i
    itemMap = M.fromList quantities

eitherDecodeFile :: FilePath -> IO (Either String CompleteProblem)
eitherDecodeFile = liftM (eitherDecode >=> Right . extractProblem) . BS.readFile
