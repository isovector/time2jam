{-# LANGUAGE TemplateHaskell #-}

module Baller where

import Control.Arrow (first)
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
  , _bFwd   :: Rel3  -- ^ Direction toward the baller's net.
  , _bDir   :: Rel3
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
           -> Rel3
           -> (Capsule -> N (Capsule, Rel3))
           -> N (B Baller)
makeBaller n p fwd f = do
  (cap'dir, input) <- foldmp (ballerCapsule n & capPos .~ p, fwd)
                           $ f . fst
  let cap = fmap fst cap'dir
      dir = fmap snd cap'dir
  return $ Baller <$> cap
                  <*> pure (input . first)
                  <*> pure (rgb 0.67 0 0.47)
                  <*> pure fwd
                  <*> dir

instance Managed Baller where
  managedCapsule = view bCap
  managedInput = view bInput
  managedOnHit _ = const $ return ()

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
    height = negate $ 135 * size
    shadowWidth = 80 * size
    shadowHeight = 30 * size

