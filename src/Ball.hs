{-# LANGUAGE TemplateHaskell #-}

module Ball where

import Baller
import Control.Lens
import Camera
import Capsule
import Game.Sequoia
import Game.Sequoia.Color
import Data.SG.Geometry.ThreeDim
import Types

defaultBall :: Ball
defaultBall = Ball
  { _ballCap = ballCapsule
  , _ballState = BSDefault
  , _ballOwner = Nothing
  }

ballCapsule :: Capsule
ballCapsule = Capsule
  { _capPos      = mkV3 0 0 0
  , _capRadius   = 0.2
  , _capHeight   = 0.2
  , _capEthereal = True
  }

updateBall :: Ball -> Ball
updateBall = id

makeBall :: V3 -> (Ball -> N Ball) -> N (B Ball)
makeBall p f = do
  (ball, input) <-
    foldmp (defaultBall & ballCap . capPos .~ p) f
  -- sync $ input (ballInput .~ input)
  return ball

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

-- instance Managed Ball where
--   managedCapsule = view ballCap
--   managedInput x = view ballInput x . over ballCap
--   managedOnHit = undefined

