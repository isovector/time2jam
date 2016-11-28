module Types
  ( getZ
  , V3
  , unpackV3
  , Rel3
  , mkV3
  ) where

import Data.SG.Geometry.ThreeDim
import Data.SG.Vector

type V3 = Point3' Double
type Rel3 = Rel3' Double

unpackV3 :: V3 -> (Double, Double, Double)
unpackV3 (Point3 pos) = pos

mkV3 :: Double -> Double -> Double -> V3
mkV3 x y z = Point3 (x, y, z)

