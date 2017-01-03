{-# LANGUAGE RankNTypes #-}

module Basket where

import Camera
import Constants
import Control.Lens ((+~), Lens')
import Game.Sequoia.Color
import Game.Sequoia.Utils
import JamPrelude


drawBasket :: Camera -> Net -> Form
drawBasket cam net =
    group [ threePointLine cam net
          , move (toScreen cam $ basketPos net)
            . filled (rgb 0.13 0.13 0.13)
            $ billboard cam
                        (basketPos net)
                        unitY
                        unitZ
                        courtBoardWidth
                        courtBoardHeight
          , move (toScreen cam (netPos net))
            . traced' red $ oval netWidth netHeight
          ]
  where
    netWidth = 40
    netHeight = 20

arc3d :: Int
      -> V3
      -> Double
      -> Double
      -> Double
      -> Lens' V3 Double
      -> Lens' V3 Double
      -> [V3]
arc3d samples pos r a1 a2 x y =
    fmap (rad2rel . getRad . fromIntegral) [0..samples]
  where
    drad = (a2 - a1) / fromIntegral samples
    getRad i = drad * i + a1
    rad2rel rad = pos & x +~ cos rad * r
                      & y +~ sin rad * r

threePointLine :: Camera -> Net -> Form
threePointLine cam n = traced' red
                     . polygon
                     . fmap (toScreen cam)
                     $ arc3d 20 pos courtLongRadius a1 a2 _x _z
  where
    pos = basketGroundPos n
    mid = toScreen cam pos
    (a1, a2) = case n of
                 LNet -> (pi*3/2, pi*5/2)
                 RNet -> (pi/2, pi*3/2)

basketPos :: Net -> V3
basketPos n = courtLength / 2 *^ fwd
            + courtBasketHeight *^ unitY
  where
    fwd = netDirection n

basketGroundPos :: Net -> V3
basketGroundPos n = basketPos n & _y .~ 0

netPos :: Net -> V3
netPos n = basketPos n
         + courtBoardHeight / 2 *^ (-unitY)
         + 0.5 *^ (-fwd)
  where
    fwd = netDirection n

billboard :: Camera
          -> V3
          -> V3 -> V3
          -> Double -> Double
          -> Shape
billboard cam pos u r w h = toPoly3 cam pos board
  where
    up    = h / 2 *^ u
    right = w / 2 *^ r
    board = fmap (pos +)
                 [ right + up
                 , right - up
                 , -right - up
                 , -right + up
                 ]

toPoly3 :: Camera -> V3 -> [V3] -> Shape
toPoly3 cam pos = toPoly (toScreen cam pos)
                . fmap   (toScreen cam)

turnoverDefPos :: Int -> Net -> V3
turnoverDefPos 0 n = basketGroundPos n + netDirection n
turnoverDefPos 1 n = basketGroundPos n + (-4) *^ netDirection n
turnoverDefPos _ _ = error "invalid index"

turnoverOffPos :: Int -> Net -> V3
turnoverOffPos i n = basketGroundPos n
                   + offset + (fromIntegral i * 2 - 1) *^ unitZ
  where
    offset = -8 *^ netDirection n

