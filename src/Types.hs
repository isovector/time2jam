module Types
  ( getZ
  , V3
  , unpackV3
  , Rel3
  , mkV3
  , rel3
  , Prop
  ) where

import Data.SG.Geometry.ThreeDim
import Data.SG.Vector
import Game.Sequoia.Types

type V3 = Point3' Double
type Rel3 = Rel3' Double
type Prop = Prop' ()


unpackV3 :: V3 -> (Double, Double, Double)
unpackV3 (Point3 pos) = pos

mkV3 :: Double -> Double -> Double -> V3
mkV3 x y z = Point3 (x, y, z)

rel3 :: Double -> Double -> Double -> Rel3
rel3 x y z = makeRel3 (x, y, z)

