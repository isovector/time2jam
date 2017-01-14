{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Types
  ( module Types
  , Schema
  ) where

import Control.Lens
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Writer (Writer)
import Data.Default
import Data.Maybe (isJust)
import Data.Spriter.Types
import Game.Sequoia

toPoly :: V2 -> [V2] -> Shape
toPoly x = polygon . fmap (subtract x)

unitX :: V3
unitX = V3 1 0 0

unitY :: V3
unitY = V3 0 1 0

unitZ :: V3
unitZ = V3 0 0 1

zero :: V3
zero = V3 0 0 0

data Net = LNet
         | RNet
         deriving (Eq, Show, Ord, Bounded, Enum)

netDirection :: Net -> V3
netDirection LNet = -unitX
netDirection RNet = unitX

otherNet :: Net -> Net
otherNet LNet = RNet
otherNet RNet = LNet

data Keypress = JumpKP
              | ShootKP
              | PassKP
              deriving (Show, Eq, Ord)

data RawController = RawController
  { _rctrlDir   :: V2
  , _rctrlShoot :: Bool
  , _rctrlPass  :: Bool
  , _rctrlTurbo :: Bool
  } deriving (Eq, Show)

data Controller = Controller
  { _ctrlDir    :: V2
  , _ctrlTurbo  :: Bool
  , _ctrlAction :: Maybe Keypress
  } deriving (Eq, Show)

instance Default RawController where
  def = RawController (V2 0 0) False False False

data Shove = ShoveData
               V3
               V3
               Double  -- ^ Distance
               Double  -- ^ Force

data Action = Shoot (Capsule -> Motion)
            | Shove Shove
            | Debug String
            | Point Net Int
            | TurnOver Net
            | PlayAnimation CannedAnim
            | Pass

type Machine a = Coroutine (Request V3 Double) (Writer [Action]) a
newtype Motion = Motion (Time -> Machine V3)

instance Eq Motion where
  _ == _ = True

instance Show Motion where
  show _ = "Motion"

data Capsule = Capsule
  { _capPos      :: V3
  , _capRadius   :: Double
  , _capHeight   :: Double
  , _capEthereal :: Bool
  , _capMotion   :: Maybe Motion
  } deriving (Eq, Show)


data Possession = Has
                | Doesnt
                deriving (Eq, Show, Ord, Bounded, Enum)

data BallerState = BSDefault
                 | BSJumping Possession
                 | BSGrounded
                 deriving (Eq, Show, Ord)

data Stats = Stats
  { _sSpeed :: Double
  , _sTurboMult :: Double
  } deriving (Eq, Show)

instance Default Stats where
  def = Stats 5 1.5

data CannedAnim = CannedAnim
  { _aSchema    :: Schema
  , _aEntity    :: EntityName
  , _aAnim      :: AnimationName
  , _aSpeedMult :: Double
  , _aRepeat    :: Bool
  } deriving (Eq)

instance Show CannedAnim where
  show CannedAnim{..} = show (_aEntity, _aAnim, _aSpeedMult, _aRepeat)

data Art = Art
  { _aCanned  :: CannedAnim
  , _aStarted :: Time
  } deriving (Eq, Show)

data Baller = Baller
  { _bCap    :: Capsule
  , _bColor  :: Color
  , _bStats  :: Stats
  , _bFwd    :: Net
  , _bDir    :: V3
  , _bFacing :: Net
  , _bState  :: BallerState
  , _bArt    :: Art
  } deriving (Eq, Show)

data BallState = BallUnowned
               | BallOwned Int
               | BallShoot Int -- ^ Shooter.
               deriving (Eq, Show)

data Ball = Ball
  { _ballCap   :: Capsule
  , _ballState :: BallState
  , _ballArt   :: Art
  } deriving (Eq, Show)

data GObject = BallObj Ball
             | BallerObj Int Baller
             deriving (Eq, Show)

instance Ord GObject where
  compare (BallObj _)     (BallObj _)     = EQ
  compare (BallObj _)     (BallerObj _ _) = LT
  compare (BallerObj _ _) (BallObj _)     = GT
  compare (BallerObj i _) (BallerObj j _) = compare i j

makePrisms ''Action
makeLenses ''Art
makeLenses ''Ball
makePrisms ''BallState
makeLenses ''Baller
makePrisms ''BallerState
makeLenses ''CannedAnim
makeLenses ''Capsule
makePrisms ''GObject
makePrisms ''Motion
makePrisms ''Shove
makeLenses ''Stats

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

