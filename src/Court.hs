module Court where

import Types

courtLength :: Double
courtLength = 28

courtGfxLength :: Double
courtGfxLength = 1800

courtDepth :: Double
courtDepth = 15

courtGfxDepth :: Double
courtGfxDepth = 300

courtBasketOffset :: Double
courtBasketOffset = 1.575

courtBasketHeight :: Double
courtBasketHeight = 3.04

courtBoardWidth :: Double
courtBoardWidth = 1.8

courtBoardHeight :: Double
courtBoardHeight = 1.05

courtRunoff :: Double
courtRunoff = 2

courtLongRadius :: Double
courtLongRadius = 7.24

data Court = Court
  { _courtTopLeft  :: !V3
  , _courtTopRight :: !V3
  , _courtBotLeft  :: !V3
  , _courtBotRight :: !V3
  }

court :: Court
court = Court (mkV3 (-width) 0 (-depth))
              (mkV3   width  0 (-depth))
              (mkV3 (-width) 0   depth)
              (mkV3   width  0   depth)
  where
    width = courtLength / 2
    depth = courtDepth / 2
