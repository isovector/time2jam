{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Baller where

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
  }

updateBaller :: Time
             -> Controller
             -> Maybe Keypress
             -> Baller
             -> Baller
updateBaller dt ctrl kp b@Baller{..} =
    b { _bCap = updateCapsule dt
                . motion'
                $ moveCapsule (scaleRel dt velocity) _bCap
      , _bDir = velocity
      }
  where
    speed = bool 1 1.5 $ _ctrlTurbo ctrl
    dx = scaleRel (5 * speed) $ _ctrlDir ctrl
    velocity  = rel3 (getX dx) 0 (getY dx)
    jumpAction = bool (jump 1.5 velocity)
                      (dunk _bFwd)
                      shouldDunk
    shouldDunk = (&& _ctrlTurbo ctrl)
               . (> 0)
               $ dot velocity (posDif (netPos _bFwd)
                                    $ _capPos _bCap)
    motion' =
        case kp of
          Just JumpKP -> bool id jumpAction
                          . not
                          . isJust
                          $ view (bCap.capMotion) b
          _           -> id

jump :: Double -> Rel3 -> Capsule -> Capsule
jump jumpHeight velocity c@Capsule{..} =
  moveTo 1 [ plusDir _capPos $ scaleRel (2 * jumpHeight) unitY
                             + scaleRel 0.5 velocity
            , plusDir _capPos velocity
            ] c

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

