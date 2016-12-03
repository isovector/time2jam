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

ballerCapsule :: Capsule
ballerCapsule = Capsule
  { _capPos    = mkV3 0 0 0
  , _capRadius = 0.5
  , _capHeight = 2
  }


makeBaller :: V3 -> (Capsule -> N Capsule) -> N (B Baller)
makeBaller p f = do
  (cap, input) <- foldmp (ballerCapsule & capPos .~ p) f
  return $ Baller <$> cap <*> pure input <*> pure (rgb 0.67 0 0.47)

instance Managed Baller where
  managedCapsule = view bCap
  managedInput = view bInput

drawBaller :: Camera -> Baller -> Prop
drawBaller cam b = filled (_bColor b)
                 $ rect (toScreen cam pos) width height
  where
    pos = b ^. bCap . capPos
    size = depthMod cam pos
    width = 50 * size
    height = 100 * size

