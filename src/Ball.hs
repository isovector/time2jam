{-# LANGUAGE RecordWildCards #-}

module Ball where

import Data.Maybe (isJust)
import Control.Lens
import Camera
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
  { _capPos      = mkV3 1.5 0 0
  , _capRadius   = 0.2
  , _capHeight   = 0.2
  , _capEthereal = True
  , _capMotion   = Nothing
  }

updateBall :: Time -> Maybe Int -> [Baller] ->  Ball -> Ball
updateBall _ hit ballers b@Ball{..} =
    b & ballCap.capPos .~ pos'
      & ballOwner      .~ owner'
  where
    owner' = _ballOwner <|> hit
    pos' = maybe (view (ballCap.capPos) b)
                 (view (bCap.capPos) . (ballers !!))
                 owner'

orange :: Color
orange = rgb 0.98 0.51 0.13

drawBall :: Camera -> Time -> Maybe Baller -> Ball -> Prop
drawBall cam when owner ball =
    group [ filled black
            $ circle (toScreen cam shadowPos) shadowRadius
          , styled orange lineStyle
            $ circle (toScreen cam pos) radius
          ]
  where
    lineStyle = defaultLine { lineWidth = 2 }
    pos       = plusDir (ball ^. ballCap . capPos) dpos
    dpos      = case isJust . view (bCap.capMotion) <$> owner of
                  Just True  -> unitY
                  Just False -> dribble
                  Nothing    -> zero
    dribble   = scaleRel ((/2) $ sin (when * 9) + 1) unitY
    radius    = 10 * depthMod cam pos

    shadowPos    = yPos .~ 0 $ pos
    shadowRadius = 10 * depthMod cam pos

