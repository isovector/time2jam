{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Camera where

import Control.Lens ((+~))
import Data.Default
import Constants
import JamPrelude
import Linear.Projection
import Linear.Matrix
import Linear.V4

projection :: M44 Double
projection = perspective (15 * pi / 180)
                         (gameWidth / gameHeight)
                         5
                         35


data Camera = Camera
  { _camFocus     :: V3
  , _camPos       :: V3
  , _camSpeed     :: Double
  , _camDeadzone  :: Double
  }
makeLenses ''Camera

instance Default Camera where
  def = Camera (V3 0 0 0)
               (V3 0 0 0)
               3
               50

moveCamera :: V3 -> Camera -> Camera
moveCamera dx = camFocus +~ dx

toScreen :: Camera -> V3 -> V2
toScreen = (fst .) . toScaledScreen

toScaledScreen :: Camera -> V3 -> (V2, Double)
toScaledScreen (Camera {..}) (unpackV3 -> (wx, wy, wz)) =
    ( V2 ( sx * gameWidth  / (2 * sz))
         (-sy * gameHeight / (2 * sz))
    , size
    )
  where
    cam = lookAt (V3 0 20 30) (V3 0 0 0) unitY
    pos = identity & translation .~ -_camPos
    m = cam !*! pos
    mxcol = V4 (m ^. _x._x) (m ^. _y._x) (m ^. _z._x) 0
    size = norm $ m !* mxcol
    (V4 sx sy sz _) = (projection !*! m) !* V4 wx wy wz 1

updateCam :: Double -> Camera -> Camera
updateCam delta cam@(Camera {..}) =
    if distance (toScreen cam _camFocus) (toScreen cam _camPos) > _camDeadzone
       then cam'
       else cam
  where
    cam' = cam { _camPos = _camPos + dir  }
    dir = delta * _camSpeed *^ (_camFocus - _camPos)

