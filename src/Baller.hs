{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Baller where

import Data.Bool (bool)
import Input
import Camera
import Control.Lens
import Capsule
import Game.Sequoia.Color
import Game.Sequoia
import Types

ballerCapsule :: Capsule
ballerCapsule = Capsule
  { _capPos      = mkV3 0 0 0
  , _capRadius   = 0.75
  , _capHeight   = 2
  , _capEthereal = False
  }

defaultBaller :: Baller
defaultBaller = Baller
  { _bCap   = ballerCapsule
  , _bColor = rgb 0.67 0 0.47
  , _bFwd   = unitX
  , _bDir   = rel3 0 0 0
  }

updateBaller :: Time -> Controller -> Baller -> Baller
updateBaller dt ctrl b@Baller{..} =
    b { _bCap = moveCapsule dir _bCap
      , _bDir = dir
      }
  where
    speed = bool 1 1.5 $ _ctrlTurbo ctrl
    dx = scaleRel (5 * speed * dt) $ _ctrlDir ctrl
    dir  = rel3 (getX dx) 0 (getY dx)

drawBaller :: Camera -> Baller -> Prop
drawBaller cam b =
  group [ traced black
          $ ellipse (toScreen cam pos)
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
    size = depthMod cam pos
    width = 50 * size / 2
    height = negate $ 135 * size
    shadowWidth = 80 * size
    shadowHeight = 30 * size

