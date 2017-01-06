{-# LANGUAGE RankNTypes #-}

module Court where

import Basket
import Camera
import Constants
import Control.Lens ((+~), Lens')
import Game.Sequoia
import Game.Sequoia.Color
import JamPrelude

data Court = Court
  { _courtTopLeft  :: !V3
  , _courtTopRight :: !V3
  , _courtBotLeft  :: !V3
  , _courtBotRight :: !V3
  }

court :: Court
court = Court (V3 (-width) 0 (-depth))
              (V3   width  0 (-depth))
              (V3 (-width) 0   depth)
              (V3   width  0   depth)
  where
    width = courtLength / 2
    depth = courtDepth / 2

drawCourt :: Court -> Camera -> Form
drawCourt c cam =
  group [ filled (rgb 1 0.67 0.47)
          . polygon
          $ fmap (toScreen cam)
          [ _courtTopLeft  c
          , _courtTopRight c
          , _courtBotRight c
          , _courtBotLeft  c
          ]
        , threePointLine cam LNet
        , threePointLine cam RNet
        , outlined' red
          . polygon
          $ fmap (toScreen cam)
          [ V3 0 0 (-courtDepth / 2)
          , V3 0 0 (courtDepth / 2)
          ]
        , outlined' red
          . polygon
          . fmap (toScreen cam)
          $ arc3d 40 (V3 0 0 0) 1.8 0 (2 * pi) _x _z
        ]

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
threePointLine cam n = outlined' red
                     . polygon
                     . fmap (toScreen cam)
                     $ arc3d 20 pos courtLongRadius a1 a2 _x _z
  where
    pos = basketGroundPos n
    (a1, a2) = case n of
                 LNet -> (pi*3/2, pi*5/2)
                 RNet -> (pi/2, pi*3/2)

