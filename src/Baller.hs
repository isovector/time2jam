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
import Data.SG.Geometry.ThreeDim (xPos, yPos)
import Game.Sequoia
import Game.Sequoia.Color
import Motion
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

otherBaller :: Baller
otherBaller = Baller
  { _bCap   = ballerCapsule & capPos.xPos .~ 1
  , _bColor = rgb 0.67 0 0.47
  , _bFwd   = LNet
  , _bDir   = rel3 0 0 0
  , _bState = BSDefault
  }

updateBaller :: Time
             -> Controller
             -> Possession
             -> Baller
             -> Writer [Action] Baller
updateBaller dt ctrl p b@Baller{..} = do
  tell actions
  newCap <- cap'
  return $
    b { _bCap = newCap
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

    kp = _ctrlAction ctrl
    canJump = kp == Just JumpKP
           && isn't _BSJumping _bState
           && not hasMotion

    canShoot = kp == Just ShootKP
            && has _BSJumping _bState
            && p == Has

    canShove = kp == Just PassKP
            && has _BSDefault _bState
            && p == Doesnt

    state' =
      case (_bState, p, hasMotion, canJump) of
        (BSJumping Has,    Has,    False, _)     -> BSGrounded
        (BSJumping Doesnt, Has,    _,     _)     -> BSJumping Doesnt
        (BSJumping _,      Doesnt, False, _)     -> BSDefault
        (BSGrounded,       Doesnt, False, _)     -> BSDefault
        (bs,               _,      True,  _)     -> bs
        (_,                pos,    False, True)  -> BSJumping pos
        (BSGrounded,       Has,    False, False) -> BSGrounded
        (BSDefault,        _,      False, False) -> BSDefault

    motion' = bool id jumpAction canJump
    actions = [Shoot $ shoot _bFwd
              | canShoot]
           ++ [Shove $ ShoveData (b ^. bCap.capPos) velocity 1 2
              | canShove]

jump :: Double -> Rel3 -> Capsule -> Capsule
jump jumpHeight velocity c@Capsule{..} =
  moveTo 1 [ plusDir _capPos $ scaleRel (2 * jumpHeight) unitY
                             + scaleRel 0.5 velocity
            , plusDir _capPos velocity
            ] c

shoot :: Net -> Capsule -> Motion
shoot net Capsule {..} = motion $ do
    a <- runBezier 1 [ jumpCtrlPt
                     , netCtrlPt
                     , netPos'
                     ] _capPos
    lift $ tell [Debug "two points!"]
    runBezier 0.2 [ netPos' & yPos .~ 0 ] a
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
dunk net c@Capsule{..} = setMotion c . motion $ do
    runBezier 0.7 [ jumpCtrlPt
                  , netCtrlPt
                  , netPos'
                  ] _capPos
     >>= runBezier 0.3 [ netPos' & yPos .~ 0 ]
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


doShove :: [Shove] -> [GObject] -> [GObject]
doShove shoves objs = do
  obj <- objs
  let hits = join . forM shoves $ \shove -> do
        let (pos, dir, dist, force) = view _ShoveData shove
            dpos = posDif (obj ^. objCap.capPos) pos
        guard $ dot dpos dir > 0.75
        guard $ mag dpos <= dist
        return $ scaleRel force dpos

      forces = mconcat hits

  case (forces, obj) of
    (0, _) -> return obj

    (_, BallerObj _ _) ->
      return $ flip (over objCap) obj
             $ \c -> setMotion c . motion $ do
               let pos = c ^. capPos
               runBezier 0.15 [plusDir pos forces] pos

    (_, BallObj _) ->
      return $ flip (over objCap) obj
            $ \c -> setMotion c . motion $ do
              let pos = c ^. capPos
              runBezier 0.15 [plusDir pos forces] pos
