{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Types where

import Control.Lens
import Data.SG.Geometry.ThreeDim
import Data.SG.Vector as V
import Game.Sequoia.Scene
import Game.Sequoia.Types

type V3 = Point3' Double
type Rel3 = Rel3' Double
type Prop = Prop' ()


unpackV3 :: V3 -> (Double, Double, Double)
unpackV3 (Point3 x y z) = (x, y, z)

mkV3 :: Double -> Double -> Double -> V3
mkV3 = Point3

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

getZ :: Coord3 p => p a -> a
getZ = V.getZ

unitX :: Rel3
unitX = rel3 1 0 0

unitY :: Rel3
unitY = rel3 0 1 0

unitZ :: Rel3
unitZ = rel3 0 0 1

data Name
  = NBaller Int
  | NNetL
  | NNetR
  | NBall
  deriving (Eq, Ord, Show)

data Keypress = ShootKP
              | PassKP
              deriving (Show, Eq, Ord)

data Action = Jump
            | Shoot
            | Pass
            | Shove
            | Dunk
            deriving (Show, Eq, Ord)

data Capsule = Capsule
  { _capName      :: Name
  , _capPos       :: V3
  , _capRadius    :: Double
  , _capHeight    :: Double
  , _capEphemeral :: Bool
  } deriving (Eq, Show)
makeLenses ''Capsule

data Baller = Baller
  { _bCap   :: Capsule
  , _bColor :: Color
  , _bFwd   :: Rel3  -- ^ Direction toward the baller's net.
  , _bDir   :: Rel3
  }
makeLenses ''Baller

data BallState = BSDefault
               | BSShoot
               | BSRebound
               | BSPassed

data Ball = Ball
  { _ballCap   :: Capsule
  -- TODO(sandy): make this a ball
  , _ballInput :: (Ball -> Ball) -> IO ()
  , _ballState :: BallState
  , _ballOwner :: Maybe Baller
  }
makeLenses ''Ball

