module Types
  ( getZ
  , V3
  , unpackV3
  , Rel3
  , mkV3
  , rel3
  , Prop
  , toPoly
  , ellipse
  ) where

import Data.SG.Geometry.ThreeDim
import Data.SG.Vector
import Game.Sequoia.Scene
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

toPoly :: Pos -> [Pos] -> Shape
toPoly x = polygon x . fmap (flip posDif x)

ellipse :: Pos -> Double -> Double -> Shape
ellipse p w h = polygon p
              $ fmap (rad2rel . (* drad) . fromIntegral) [0..samples]
  where
    samples = 15 :: Int
    drad = 2 * pi / fromIntegral samples
    rad2rel :: Double -> Rel
    rad2rel rad = rel (cos rad * w / 2) (sin rad * h / 2)
