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

makePrisms ''Action
makePrisms ''Shove
makePrisms ''Motion
makeLenses ''Capsule

data Possession = Has
                | Doesnt
                deriving (Eq, Show, Ord, Bounded, Enum)

data BallerState = BSDefault
                 | BSJumping Possession
                 | BSGrounded
                 deriving (Eq, Show, Ord)
makePrisms ''BallerState

data Stats = Stats
  { _sSpeed :: Double
  , _sTurboMult :: Double
  } deriving (Eq, Show)

instance Default Stats where
  def = Stats 5 1.5
makeLenses ''Stats

data Art = Art
  { _aSchema    :: Schema
  , _aEntity    :: EntityName
  , _aAnim      :: AnimationName
  , _aStarted   :: Time
  , _aSpeedMult :: Double
  } deriving (Eq)
makeLenses ''Art

instance Show Art where
  show Art{..} = show (_aEntity, _aAnim, _aStarted, _aSpeedMult)

data Baller = Baller
  { _bCap   :: Capsule
  , _bColor :: Color
  , _bStats :: Stats
  , _bFwd   :: Net
  , _bDir   :: V3
  , _bState :: BallerState
  , _bArt   :: Art
  } deriving (Eq, Show)
makeLenses ''Baller

data BallState = BallUnowned
               | BallOwned Int
               | BallShoot Int -- ^ Shooter.
               deriving (Eq, Show)
makePrisms ''BallState

data Ball = Ball
  { _ballCap   :: Capsule
  , _ballState :: BallState
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

