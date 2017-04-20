--
-- Adapted from the K-Means example in the remote-0.1.1 package,
--   (c) Jeff Epstein <jepst79@gmail.com>
--
-- transform Point: Double Double -> (Double, Double)
{-# LANGUAGE DeriveDataTypeable #-}
module KMeansCore where

import Data.List
import Data.Typeable (Typeable)
import Data.Data (Data)
import qualified Data.ByteString.Char8 as B
import Data.Binary
import Control.DeepSeq

-- -----------------------------------------------------------------------------
-- Points

type Point = (Double, Double)

-- deepseq changed in GHC 7.10 to use Generic instances, so for backwards
-- compatibility define it manually.


--instance NFData Point where
  --rnf (Point x y) = () -- all fields are strict

-- <<point-ops
zeroPoint :: Point
zeroPoint = (0, 0)

sqDistance :: Point -> Point -> Double
sqDistance (x1, y1) (x2, y2) = ((x1-x2)^2) + ((y1-y2)^2)
-- >>

-----------------------------------------------------------------------------
-- Clusters

type Cluster = (Int, Point)

--instance NFData Cluster where
--  rnf (Cluster id cent) = () -- all fields are strict

makeCluster :: Int -> [Point] -> Cluster
makeCluster clid points =
    (clid, (a / fromIntegral count, b / fromIntegral count))
 where
  pointsum@((a, b)) = foldl' addPoint zeroPoint points
  count = length points

  addPoint :: Point -> Point -> Point
  addPoint (x1, y1) (x2, y2) = (x1+x2, y1+y2)
