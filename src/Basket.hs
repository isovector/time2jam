module Basket where

import Types
import Game.Sequoia
import Game.Sequoia.Color
import Camera
import Constants


drawBasket :: Camera -> Rel3 -> Prop
drawBasket cam fwd =
    group [ filled (rgb 0.13 0.13 0.13)
            $ billboard cam
                        bPos
                        unitY
                        unitZ
                        courtBoardWidth
                        courtBoardHeight
          , traced red
            $ ellipse (toScreen cam netPos) netWidth netHeight
          ]
  where
    netWidth = 40
    netHeight = 20
    bPos = plusDir (mkV3 0 0 0) $ scaleRel (courtLength / 2) (-fwd)
                                + scaleRel courtBasketHeight (-unitY)
    netPos = plusDir ( plusDir bPos
                     $ scaleRel (courtBoardHeight / 2) unitY)
           $ scaleRel 0.5 fwd

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

