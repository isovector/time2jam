{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera where

import Control.Lens ((+~))
import Data.Default
import Constants
import JamPrelude
import Linear.Projection
import Linear.Matrix
import Linear.V4
import Game.Sequoia.Utils

projection :: M44 Double
projection = perspective (15 * pi / 180) (700/400) 5 35



data Camera = Camera
  { _camFocus     :: V3
  , _camPos       :: V3
  , _camSpeed     :: Double
  , _camDeadzone  :: Double
  , _camDepthMult :: Double
  , _camWidthMult :: Double
  }
makeLenses ''Camera

instance Default Camera where
  def = Camera (V3 0 0 0)
               (V3 0 0 0)
               3
               50
               (courtGfxDepth / courtDepth)
               (courtGfxLength / courtLength)

moveCamera :: V3 -> Camera -> Camera
moveCamera dx = camFocus +~ dx

heightScaling :: Double
heightScaling = 75


toScreen :: Camera -> V3 -> V2
toScreen cam@(Camera {..}) world = V2 ((v2 ^. _x) * 700 / (2 * v2 ^. _z) - 175) ((v2 ^. _y) * 400 / (2 * v2 ^. _z) - 100)
  where
    screen = V4 (V4 1 0 0 1) (V4 0 (-1) 0 1) (V4 0 0 1 1) (V4 0 0 0 1)
    m = lookAt (unitY ^* 15 + unitZ ^* 20) (V3 0 0 0) unitY
    pos = identity & translation .~ -_camPos
    v2 = (screen !*! projection !*! m !*! pos) !* V4 (world ^. _x) (world ^. _y) (world ^. _z) 1


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

