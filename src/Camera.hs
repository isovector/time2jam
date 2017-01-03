{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera where

import Control.Lens ((+~))
import Data.Default
import Constants
import JamPrelude

data Camera = Camera
  { _camFocus :: V3
  , _camPos :: V3
  , _camDeadzone :: Double
  , _camDepthMult :: Double
  , _camWidthMult :: Double
  }
makeLenses ''Camera

instance Default Camera where
  def = Camera (V3 0 0 0)
               (V3 0 0 0)
               50
               (courtGfxDepth / courtDepth)
               (courtGfxLength / courtLength)

moveCamera :: V3 -> Camera -> Camera
moveCamera dx = camFocus +~ dx

heightScaling :: Double
heightScaling = 75


toScreen :: Camera -> V3 -> V2
toScreen cam@(Camera {..}) world = V2 x y
  where
    local = world - _camPos
    dmod = depthMod cam world
    x = view _x local * _camWidthMult * dmod
    y = view _z local * _camDepthMult
      - view _y local * heightScaling * dmod

depthMod :: Camera -> V3 -> Double
depthMod cam world = 1 / ((view _z (_camPos cam) - view _z world) / courtDepth + 1.5)

updateCam :: Double -> Camera -> Camera
updateCam delta cam@(Camera {..}) =
    if distance (toScreen cam _camFocus) (toScreen cam _camPos) > _camDeadzone
       then cam'
       else cam
  where
    cam' = cam { _camPos = _camPos + dir  }
    dir = delta * 2 *^ (_camFocus - _camPos)

