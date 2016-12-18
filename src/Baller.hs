{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Baller where

import Control.Monad.Writer
import Basket
import Camera
import Capsule
import Control.Lens
import Data.Bool (bool)
import Data.Maybe (isJust)
import Data.SG.Geometry.ThreeDim (yPos)
import Game.Sequoia
import Game.Sequoia.Color
import Input
import Types

ballerCapsule :: Capsule
ballerCapsule = Capsule
  { _capPos      = mkV3 0 0 0
  , _capRadius   = 0.75
  , _capHeight   = 2
  , _capEthereal = False
  , _capMotion   = Nothing
  }

defaultBaller :: Baller
defaultBaller = Baller
  { _bCap   = ballerCapsule
  , _bColor = rgb 0.67 0 0.47
  , _bFwd   = RNet
  , _bDir   = rel3 0 0 0
  , _bState = BSDefault
  }

updateBaller :: Time
             -> Controller
             -> Maybe Keypress
             -> Possession
             -> Baller
             -> Writer [Action] Baller
updateBaller dt ctrl kp p b@Baller{..} = do
  tell actions
  return $
    b { _bCap = cap'
      , _bDir = velocity
      , _bState = state'
      }
  where
    speed = case (_bState, _ctrlTurbo ctrl) of
              (BSDefault, True)  -> 1.5
              (BSDefault, False) -> 1
              (_, _)             -> 0

    dx = scaleRel (5 * speed) $ _ctrlDir ctrl
    velocity  = rel3 (getX dx) 0 (getY dx)
    hasMotion = isJust $ view capMotion _bCap
    jumpAction = bool (jump 1.5 velocity)
                      (dunk _bFwd)
                      shouldDunk
    shouldDunk = (&& _ctrlTurbo ctrl)
               . (> 0)
               $ dot velocity (posDif (netPos _bFwd)
                                    $ _capPos _bCap)

    cap' = updateCapsule dt
         . motion'
         $ moveCapsule (scaleRel dt velocity) _bCap

    canJump = kp == Just JumpKP
           && _bState /= BSJumping
           && _bState /= BSShooting
           && not hasMotion

    canShoot = kp == Just ShootKP
            && ( _bState == BSShooting
              || _bState == BSJumping
               )
            && p == Has

    state' =
      -- TODO(sandy): this could be hella simplified if we knew whether they
      -- had the ball when left the ground
      case (_bState, p, hasMotion, canJump, canShoot) of
        (BSJumping,  _,      False, _,     _    ) -> BSDefault
        (BSShooting, Has,    False, _,     _    ) -> BSGrounded
        (BSShooting, Doesnt, False, _,     _    ) -> BSDefault
        (BSGrounded, Doesnt, False, _,     _    ) -> BSDefault
        (bs,         _,      True,  _,     False) -> bs
        (_,          Has,    False, True,  _    ) -> BSShooting
        (_,          Doesnt, False, True,  _    ) -> BSJumping
        (BSGrounded, Has,    False, False, _    ) -> BSGrounded
        (BSDefault,  _,      False, False, _    ) -> BSDefault
        (BSShooting, Has,    True,  _,     True ) -> BSJumping
        (BSJumping,  _,      _,     _,     True ) -> BSJumping
        (BSGrounded, _,      _,     _,     True ) -> error "tried to shoot while grounded"
        (BSDefault,  _,      _,     _,     True ) -> error "tried to shoot while default"
        (BSShooting, Doesnt, _,     _,     True ) -> error "tried to shoot without ball"

    motion' = bool id jumpAction canJump
    actions = bool [] [Shoot $ shoot _bFwd] canShoot

jump :: Double -> Rel3 -> Capsule -> Capsule
jump jumpHeight velocity c@Capsule{..} =
  moveTo 1 [ plusDir _capPos $ scaleRel (2 * jumpHeight) unitY
                             + scaleRel 0.5 velocity
            , plusDir _capPos velocity
            ] c

shoot :: Net -> Capsule -> Motion
shoot net c@Capsule {..} =
  makeMotion 1 [ jumpCtrlPt
               , netCtrlPt
               , netPos'
               ] c
    & mAfterwards .~
      Just (makeMotion 0.2 [ netPos' & yPos .~ 0 ])
  where
    -- TODO(sandy): make this less copy-paste
    dunkHeight = 3
    dunkCtrl = scaleRel dunkHeight unitY
    netDir = let (x, _, z) = unpackRel3 $ posDif netPos' _capPos
              in normalize $ rel3 x 0 z
    jumpCtrlPt = add (scaleRel (view yPos netPos') unitY)
               . add netDir
               $ add dunkCtrl _capPos
    netCtrlPt = add (negate netDir) $ add dunkCtrl netPos'
    netPos' = netPos net
    add = flip plusDir

dunk :: Net -> Capsule -> Capsule
dunk net c@Capsule{..} =
    after  0.3 [ netPos' & yPos .~ 0
               ]
  $ moveTo 0.7 [ jumpCtrlPt
               , netCtrlPt
               , netPos'
               ] c
  where
    dunkHeight = 3
    dunkCtrl = scaleRel dunkHeight unitY
    netDir = let (x, _, z) = unpackRel3 $ posDif netPos' _capPos
              in normalize $ rel3 x 0 z
    jumpCtrlPt = add (scaleRel (view yPos netPos') unitY)
               . add netDir
               $ add dunkCtrl _capPos
    netCtrlPt = add (scaleRel 2 netDir) $ add dunkCtrl netPos'
    netPos' = netPos net
    add = flip plusDir



drawBaller :: Camera -> Baller -> Prop
drawBaller cam b =
  group [ traced black
          $ ellipse (toScreen cam shadowPos)
                    shadowWidth
                    shadowHeight
        , traced (_bColor b)
          $ polygon (toScreen cam pos)
            [ rel (-width) 0
            , rel (-width) height
            , rel   width  height
            , rel   width 0
            ]
        ]
  where
    pos = b ^. bCap . capPos
    shadowPos = pos & yPos .~ 0
    size = depthMod cam pos
    width = 50 * size / 2
    height = negate $ 135 * size
    shadowWidth = 80 * size
    shadowHeight = 30 * size

