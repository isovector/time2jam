{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera where

import Data.Default
import Control.Lens
import Game.Sequoia
import Constants
import Types

data Camera = Camera
  { _camFocus :: V3
  , _camPos :: V3
  , _camDeadzone :: Double
  , _camDepthMult :: Double
  , _camWidthMult :: Double
  }
makeLenses ''Camera

instance Default Camera where
  def = Camera (mkV3 0 0 0)
               (mkV3 0 0 0)
               100
               (courtGfxDepth / courtDepth)
               (courtGfxLength / courtLength)

moveCamera :: Rel3 -> Camera -> Camera
moveCamera dx = camFocus %~ flip plusDir dx

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

updateCam :: Double -> Camera -> Camera
updateCam delta cam@(Camera {..}) =
    if distance (toScreen cam _camFocus) (toScreen cam _camPos) > _camDeadzone
       then cam'
       else cam
  where
    cam' = cam { _camPos = plusDir _camPos dir  }
    dir = scaleRel (delta * 2) $ posDif _camFocus _camPos

