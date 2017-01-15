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
import Data.Spriter.Types
import Game.Sequoia



------------------------------------------------------------------------------
data Action = Shoot (Capsule -> Motion)
            | Shove Shove
            | Debug String
            | Point Net Int
            | ChangeGameMode GameMode
            | PlayAnimation CannedAnim
            | Pass


------------------------------------------------------------------------------
data Art = Art
  { _aCanned  :: CannedAnim
  , _aStarted :: Time
  } deriving (Eq, Show)


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
data BallerState = BSDefault
                 | BSJumping Possession
                 | BSDunking
                 | BSGrounded
                 deriving (Eq, Show, Ord)


------------------------------------------------------------------------------
data CannedAnim = CannedAnim
  { _aSchema    :: Schema
  , _aEntity    :: EntityName
  , _aAnim      :: AnimationName
  , _aSpeedMult :: Double
  , _aRepeat    :: Bool
  } deriving (Eq)

instance Show CannedAnim where
  show CannedAnim{..} = show (_aEntity, _aAnim, _aSpeedMult, _aRepeat)


------------------------------------------------------------------------------
data Ball = Ball
  { _ballCap   :: Capsule
  , _ballState :: BallState
  , _ballArt   :: Art
  } deriving (Eq, Show)


------------------------------------------------------------------------------
data BallState = BallUnowned
               | BallOwned Int
               | BallShoot Int -- ^ Shooter.
               deriving (Eq, Show)


------------------------------------------------------------------------------
data Camera = Camera
  { _camFocus     :: V3
  , _camPos       :: V3
  , _camSpeed     :: Double
  , _camDeadzone  :: Double
  }

instance Default Camera where
  def = Camera (V3 0 0 0)
               (V3 0 0 0)
               3
               50


------------------------------------------------------------------------------
data Capsule = Capsule
  { _capPos      :: V3
  , _capRadius   :: Double
  , _capHeight   :: Double
  , _capEthereal :: Bool
  , _capMotion   :: Maybe Motion
  } deriving (Eq, Show)


------------------------------------------------------------------------------
data Controller = Controller
  { _ctrlDir    :: V2
  , _ctrlTurbo  :: Bool
  , _ctrlAction :: Maybe Keypress
  } deriving (Eq, Show)


------------------------------------------------------------------------------
data GObject = BallObj Ball
             | BallerObj Int Baller
             deriving (Eq, Show)

instance Ord GObject where
  compare (BallObj _)     (BallObj _)     = EQ
  compare (BallObj _)     (BallerObj _ _) = LT
  compare (BallerObj _ _) (BallObj _)     = GT
  compare (BallerObj i _) (BallerObj j _) = compare i j


------------------------------------------------------------------------------
data Game = Game
  { _gCamera  :: Camera
  , _gBall    :: Ball
  , _gBallers :: [Baller]
  , _gMode    :: GameMode
  }


------------------------------------------------------------------------------
data GameMode = Play
              | TurnOver Net
              | TipOff
              deriving (Eq, Show)


------------------------------------------------------------------------------
data Keypress = JumpKP
              | ShootKP
              | PassKP
              deriving (Show, Eq, Ord)


------------------------------------------------------------------------------
type Machine a = Coroutine (Request V3 Double) (Writer [Action]) a


------------------------------------------------------------------------------
newtype Motion = Motion (Time -> Machine V3)

instance Eq Motion where
  _ == _ = True

instance Show Motion where
  show _ = "Motion"


------------------------------------------------------------------------------
data Net = LNet
         | RNet
         deriving (Eq, Show, Ord, Bounded, Enum)


------------------------------------------------------------------------------
data Possession = Has
                | Doesnt
                deriving (Eq, Show, Ord, Bounded, Enum)


------------------------------------------------------------------------------
data RawController = RawController
  { _rctrlDir   :: V2
  , _rctrlShoot :: Bool
  , _rctrlPass  :: Bool
  , _rctrlTurbo :: Bool
  } deriving (Eq, Show)

instance Default RawController where
  def = RawController (V2 0 0) False False False


------------------------------------------------------------------------------
data Shove = ShoveData
               V3
               V3
               Double  -- ^ Distance
               Double  -- ^ Force


------------------------------------------------------------------------------
data Stats = Stats
  { _sSpeed :: Double
  , _sTurboMult :: Double
  } deriving (Eq, Show)

instance Default Stats where
  def = Stats 5 1.5


------------------------------------------------------------------------------
makePrisms ''Action
makeLenses ''Art
makeLenses ''Ball
makePrisms ''BallState
makeLenses ''Baller
makePrisms ''BallerState
makeLenses ''Camera
makeLenses ''CannedAnim
makeLenses ''Capsule
makePrisms ''GObject
makeLenses ''Game
makePrisms ''GameMode
makePrisms ''Motion
makePrisms ''Shove
makeLenses ''Stats

