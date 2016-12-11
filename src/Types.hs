{-# LANGUAGE TemplateHaskell           #-}

module Types where

import Control.Lens
import Data.SG.Geometry.ThreeDim
import Data.SG.Vector as V
import Game.Sequoia.Scene
import Data.Maybe (isJust)
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
  { _capPos      :: V3
  , _capRadius   :: Double
  , _capHeight   :: Double
  , _capEthereal :: Bool
  } deriving (Eq, Show)
makeLenses ''Capsule

data Baller = Baller
  { _bCap   :: Capsule
  , _bColor :: Color
  , _bFwd   :: Rel3  -- ^ Direction toward the baller's net.
  , _bDir   :: Rel3
  } deriving (Eq, Show)
makeLenses ''Baller

data BallState = BSDefault
               | BSShoot
               | BSRebound
               | BSPassed
               deriving (Eq, Show, Ord, Bounded)

data Ball = Ball
  { _ballCap   :: Capsule
  , _ballState :: BallState
  , _ballOwner :: Maybe Int
  } deriving (Eq, Show)
makeLenses ''Ball

data GObject = BallObj Ball
             | BallerObj Int Baller
             deriving (Eq, Show)
makePrisms ''GObject

instance Ord GObject where
  compare (BallObj _)     (BallObj _)     = EQ
  compare (BallObj _)     (BallerObj _ _) = LT
  compare (BallerObj _ _) (BallObj _)     = GT
  compare (BallerObj i _) (BallerObj j _) = compare i j

isBall :: GObject -> Bool
isBall = isJust . preview _BallObj

isBaller :: Int -> GObject -> Bool
isBaller i (BallerObj j _) = i == j
isBaller _ _               = False

objCap :: Lens' GObject Capsule
objCap = lens getter setter
  where
    getter (BallObj b)     = view ballCap b
    getter (BallerObj _ b) = view bCap    b
    setter (BallObj b) c     = BallObj $     b & ballCap .~ c
    setter (BallerObj i b) c = BallerObj i $ b &    bCap .~ c

