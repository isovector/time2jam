{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Ball where

import Baller
import Camera
import Capsule
import Control.Monad.Writer (Writer)
import Data.Bits (complementBit)
import Data.Bool (bool)
import Data.Maybe (isJust)
import Game.Sequoia.Color
import JamPrelude
import Motion

defaultBall :: Ball
defaultBall = Ball
  { _ballCap   = ballCapsule
  , _ballState = BallUnowned
  }

ballCapsule :: Capsule
ballCapsule = Capsule
  { _capPos      = V3 0 0 0
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
                [_capPos _bCap
                  + (passTime *^ _bDir + ballerBallHeight teammate)
                ] $ _capPos c + ballerBallHeight owner


orange :: Color
orange = rgb 0.98 0.51 0.13

drawBall :: Camera -> Time -> Maybe Baller -> Ball -> Form
drawBall cam when owner ball =
    group [ move shadowPos
            . filled black
            $ circle (shadowSize * radius)
          , move pos2d
            . filled orange
            $ circle (size * radius)
          ]
  where
    -- lineStyle = defaultLine { lineWidth = 2 }
    pos       = ball ^. ballCap.capPos + bool dpos zero hasMotion
    (pos2d, size) = toScaledScreen cam pos
    (shadowPos, shadowSize) = toScaledScreen cam $ pos & _y .~ 0
    hasMotion = isJust $ ball ^. ballCap.capMotion
    dpos      = case isJust . view (bCap.capMotion) <$> owner of
                  Just True  -> unitY
                  Just False -> dribble
                  Nothing    -> zero
    dribble   = ((/2) $ sin (when * 9) + 1) *^ unitY
    radius = 7

