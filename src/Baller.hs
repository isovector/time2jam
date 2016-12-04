{-# LANGUAGE TemplateHaskell #-}

module Baller where

import Camera
import Control.Lens
import Capsule
import Game.Sequoia.Color
import Game.Sequoia
import Types

data Baller = Baller
  { _bCap   :: Capsule
  , _bInput :: (Capsule -> Capsule) -> IO ()
  , _bColor :: Color
  }
makeLenses ''Baller

ballerCapsule :: Int -> Capsule
ballerCapsule n = Capsule
  { _capName      = NBaller n
  , _capPos       = mkV3 0 0 0
  , _capRadius    = 0.75
  , _capHeight    = 2
  , _capEphemeral = False
  }


makeBaller :: Int
           -> V3
           -> (Capsule -> N Capsule)
           -> N (B Baller)
makeBaller n p f = do
  (cap, input) <- foldmp (ballerCapsule n & capPos .~ p) f
  return $ Baller <$> cap <*> pure input <*> pure (rgb 0.67 0 0.47)

instance Managed Baller where
  managedCapsule = view bCap
  managedInput = view bInput

drawBaller :: Camera -> Baller -> Prop
drawBaller cam b =
  group [ traced black
          $ ellipse (toScreen cam pos)
                    shadowWidth
                    shadowHeight
        , traced (_bColor b)
          $ polygon (toScreen cam pos)
            [ rel (-width) 0
            , rel (-width) height
            , rel   width  height
            , rel   width 0
            ]
        ]

  where
    pos = b ^. bCap . capPos
    size = depthMod cam pos
    width = 50 * size / 2
    height = negate $ 100 * size
    shadowWidth = 80 * size
    shadowHeight = 30 * size

