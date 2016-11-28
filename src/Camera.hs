{-# LANGUAGE RecordWildCards #-}

module Camera where

import Game.Sequoia
import Court
import Types

data Camera = Camera
  { _camFocus :: V3
  , _camPos :: V3
  , _camDeadzone :: Double
  , _camDepthMult :: Double
  , _camWidthMult :: Double
  }

heightScaling :: Double
heightScaling = 75


toScreen :: Camera -> V3 -> Pos
toScreen cam@(Camera {..}) world = mkPos x y
  where
    local = posDif world _camPos
    dmod = depthMod cam world
    x = getX local * _camWidthMult * dmod
    y = getZ local * _camDepthMult
      + getY local * heightScaling * dmod

depthMod :: Camera -> V3 -> Double
depthMod cam world = 1 / ((getZ world - getZ (_camPos cam)) / courtDepth + 1.5)

updateCam :: Camera -> Double -> Camera
updateCam cam@(Camera {..}) delta =
    if distance _camFocus _camPos > _camDeadzone
       then cam'
       else cam
  where
    cam' = cam { _camPos = plusDir _camPos dir  }
    dir = scaleRel (delta * 2) $ v3Dif _camPos _camFocus

