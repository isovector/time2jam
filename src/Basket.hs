module Basket where

import Camera
import Constants
import Control.Lens ((.~), (&))
import Data.SG.Geometry.ThreeDim (yPos)
import Game.Sequoia
import Game.Sequoia.Color
import Types


drawBasket :: Camera -> Net -> Prop
drawBasket cam net =
    group [ filled (rgb 0.13 0.13 0.13)
            $ billboard cam
                        (basketPos net)
                        unitY
                        unitZ
                        courtBoardWidth
                        courtBoardHeight
          , traced red
            $ ellipse (toScreen cam (netPos net)) netWidth netHeight
          ]
  where
    netWidth = 40
    netHeight = 20

basketPos :: Net -> V3
basketPos n = plusDir (mkV3 0 0 0) $ scaleRel (courtLength / 2) fwd
                                   + scaleRel courtBasketHeight unitY
  where
    fwd = netDirection n

basketGroundPos :: Net -> V3
basketGroundPos n = basketPos n & yPos .~ 0

netPos :: Net -> V3
netPos n = plusDir ( plusDir (basketPos n)
                    $ scaleRel (courtBoardHeight / 2) (-unitY))
         $ scaleRel 0.5 (-fwd)
  where
    fwd = netDirection n

billboard :: Camera
          -> V3
          -> Rel3 -> Rel3
          -> Double -> Double
          -> Shape
billboard cam pos u r w h = toPoly3 cam pos board
  where
    up    = scaleRel (h / 2) u
    right = scaleRel (w / 2) r
    board = fmap (plusDir pos)
                 [ right + up
                 , right - up
                 , (-right) - up
                 , (-right) + up
                 ]

toPoly3 :: Camera -> V3 -> [V3] -> Shape
toPoly3 cam pos = toPoly (toScreen cam pos)
                . fmap   (toScreen cam)

turnoverDefPos :: Int -> Net -> V3
turnoverDefPos 0 n = plusDir (basketGroundPos n) (netDirection n)
turnoverDefPos 1 n = plusDir (basketGroundPos n) (scaleRel (-4) $ netDirection n)
turnoverDefPos _ _ = error "invalid index"

turnoverOffPos :: Int -> Net -> V3
turnoverOffPos i n = plusDir (basketGroundPos n)
                             (offset + scaleRel (fromIntegral i * 2 - 1) unitZ)
  where
    offset = scaleRel (-8) $ netDirection n

