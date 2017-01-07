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
toScreen (Camera {..}) (unpackV3 -> (wx, wy, wz)) =
    V2 ( sx * gameWidth  / (2 * sz))
       (-sy * gameHeight / (2 * sz))
  where
    m = lookAt (V3 0 20 30) (V3 0 0 0) unitY
    pos = identity & translation .~ -_camPos
    (V4 sx sy sz _) = (projection !*! m !*! pos) !* V4 wx wy wz 1


depthMod :: Camera -> V3 -> Double
depthMod cam world = 1 / ((view _z (_camPos cam) - view _z world) / courtDepth + 1.5)

updateCam :: Double -> Camera -> Camera
updateCam delta cam@(Camera {..}) =
    if distance (toScreen cam _camFocus) (toScreen cam _camPos) > _camDeadzone
       then cam'
       else cam
  where
    cam' = cam { _camPos = _camPos + dir  }
    dir = delta * _camSpeed *^ (_camFocus - _camPos)

