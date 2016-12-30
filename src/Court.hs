module Court where

import Game.Sequoia
import Types
import Constants
import Camera

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
drawCourt c cam = move (toScreen cam $ V3 0 0 0)
                . filled (rgb 1 0.67 0.47)
                . toPoly (toScreen cam $ V3 0 0 0)
                $ fmap (toScreen cam)
                [ _courtTopLeft  c
                , _courtTopRight c
                , _courtBotRight c
                , _courtBotLeft  c
                ]

