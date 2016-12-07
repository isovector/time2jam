{-# LANGUAGE TemplateHaskell #-}

module Ball where

import Control.Lens
import Camera
import Capsule
import Game.Sequoia
import Game.Sequoia.Color
import Data.SG.Geometry.ThreeDim
import Types


data BallState = BSDefault
               | BSShoot
               | BSRebound
               | BSPassed

data Ball = Ball
  { _ballCap   :: Capsule
  -- TODO(sandy): make this a ball
  , _ballInput :: (Capsule -> Capsule) -> IO ()
  , _ballState :: BallState
  }
makeLenses ''Ball

ballCapsule :: Capsule
ballCapsule = Capsule
  { _capName      = NBall
  , _capPos       = mkV3 0 0 0
  , _capRadius    = 0.2
  , _capHeight    = 0.2
  , _capEphemeral = True
  }

makeBall :: V3 -> N (B Ball)
makeBall p = do
  (cap, input) <- foldmp (ballCapsule & capPos .~ p) return
  return $ Ball <$> cap <*> pure input <*> pure BSDefault

orange :: Color
orange = rgb 0.98 0.51 0.13

drawBall :: Camera -> Ball -> Prop
drawBall cam ball =
    group [ filled black
            $ circle (toScreen cam shadowPos) shadowRadius
          , styled orange lineStyle
            $ circle (toScreen cam pos) radius
          ]
  where
    lineStyle = defaultLine { lineWidth = 2 }
    pos       = ball ^. ballCap . capPos
    radius    = 10 * depthMod cam pos

    shadowPos    = yPos .~ 0 $ pos
    shadowRadius = 10 * depthMod cam pos

instance Managed Ball where
  managedCapsule = view ballCap
  managedInput = view ballInput

