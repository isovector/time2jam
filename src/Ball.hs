{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Ball where

import Baller
import Camera
import Capsule
import Control.Lens
import Control.Monad.Writer (Writer)
import Data.Bits (complementBit)
import Data.Bool (bool)
import Data.Maybe (isJust)
import Data.SG.Geometry.ThreeDim
import Game.Sequoia
import Game.Sequoia.Color
import Motion
import Types

defaultBall :: Ball
defaultBall = Ball
  { _ballCap   = ballCapsule
  , _ballState = BallUnowned
  }

ballCapsule :: Capsule
ballCapsule = Capsule
  { _capPos      = mkV3 0 0 0
  , _capRadius   = 0.2
  , _capHeight   = 0.2
  , _capEthereal = True
  , _capMotion   = Nothing
  }

updateBall :: Time
           -> Maybe Int
           -> [Baller]
           -> Maybe Action
           -> Ball
           -> Writer [Action] Ball
updateBall dt hit ballers action b@Ball{..} = do
    cap' <- updateCapsule dt
          . motion'
          . (capPos .~ pos')
          $ _ballCap
    return $ b { _ballCap = cap'
               , _ballState = state'
               }
  where
    motion' =
      case (action, killMotion) of
        (Just (Shoot m), _) -> flip setMotionwtf m
        (Just Pass, _)      -> flip setMotionwtf doPass
        (_,  True)          -> capMotion .~ Nothing
        _                   -> id

    hasMotion = isJust $ view capMotion _ballCap

    state' =
      case (_ballState, hit, action, hasMotion) of
        (BallUnowned, Just x, _, _)          -> BallOwned x
        (BallUnowned, Nothing, _, _)         -> BallUnowned
        (BallOwned x, _, Nothing, _)         -> BallOwned x
        (BallOwned x, _, Just (Shoot _), _)  -> BallShoot x
        (BallOwned x, _, Just Pass, _)       -> BallShoot x
        (BallOwned x, _, Just _, _)          -> BallOwned x
        (BallShoot x, Just y, _, _) | x /= y -> BallOwned y
        (BallShoot x, _, _, True)            -> BallShoot x
        (BallShoot _, _, _, False)           -> BallUnowned

    killMotion = has _BallOwned state'
              && state' /= _ballState

    pos' = maybe (b ^. ballCap.capPos)
                 (view (bCap.capPos) . (ballers !!))
                 $ preview _BallOwned state'

    doPass c = motion $ do
      let ownerId = maybe (error "impossible -- no owner") id
                  $ preview (ballState._BallOwned) b
          owner = ballers !! ownerId
          teammateId = complementBit ownerId 0
          teammate@Baller{..} = ballers !! teammateId
          passVelocity = 30
          -- TODO(sandy): this is crap -- do some calculus to figure out when
          -- the ball will intersect with the baller
          passTime = 0.1
      velBezier passVelocity
                [plusDir (_capPos _bCap)
                         ( scaleRel passTime _bDir
                         + ballerBallHeight teammate
                         )
                ] $ plusDir (_capPos c) (ballerBallHeight owner)


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
    pos       = plusDir (ball ^. ballCap.capPos) $ bool dpos zero hasMotion
    hasMotion = isJust $ ball ^. ballCap.capMotion
    dpos      = case isJust . view (bCap.capMotion) <$> owner of
                  Just True  -> unitY
                  Just False -> dribble
                  Nothing    -> zero
    dribble   = scaleRel ((/2) $ sin (when * 9) + 1) unitY
    radius    = 10 * depthMod cam pos

    shadowPos    = yPos .~ 0 $ pos
    shadowRadius = 10 * depthMod cam pos

