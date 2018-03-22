{-# LANGUAGE RecordWildCards #-}

module JamPrelude
  ( over, (.~), (^.), set, view, makeLenses, makePrisms, (%~), (&), has, isn't, preview, review
  , def
  , isJust
  , (*^), (^*)
  , (<>)
  , module Game.Sequoia
  , module Types
  , module JamPrelude
  , first, second
  , liftIO
  , lift
  , bool
  , module Control.Monad
  , module Data.Ecstasy
  , module Data.Foldable
  , module Data.Traversable
  ) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Bool (bool)
import Data.Default
import Data.Foldable (traverse_, for_)
import Data.Traversable (traverse, for)
import Data.Ecstasy
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Tuple (swap)
import Game.Sequoia
import Linear.Metric (distance, norm, signorm, dot)
import Linear.Vector ((*^), (^*))
import Types


unitX :: V3
unitX = V3 1 0 0

unitY :: V3
unitY = V3 0 1 0

unitZ :: V3
unitZ = V3 0 0 1

zero :: V3
zero = V3 0 0 0

toPoly :: V2 -> [V2] -> Shape
toPoly x = polygon . fmap (subtract x)

netDirection :: Net -> V3
netDirection LNet = -unitX
netDirection RNet = unitX

otherNet :: Net -> Net
otherNet LNet = RNet
otherNet RNet = LNet

isBall :: GObject -> Bool
isBall = isJust . preview _BallObj

isBaller :: Int -> GObject -> Bool
isBaller i (BallerObj j _) = i == j
isBaller _ _               = False

objCap :: Lens' GObject Capsule
objCap = lens getter setter
  where
    getter (BallObj b)     = view ballCap b
    getter (BallerObj _ b) = view bCap    b
    setter (BallObj b) c     = BallObj $     b & ballCap .~ c
    setter (BallerObj i b) c = BallerObj i $ b &    bCap .~ c

withObjects :: (Monad m) => Game -> ([GObject] -> m [GObject]) -> m Game
withObjects g@Game{..} f = do
  (BallObj ball : ballers) <- f . (BallObj _gBall :)
                                . fmap (uncurry BallerObj)
                                $ zip [0..] _gBallers
  return $ g & gBall .~ ball
             & gBallers .~ fmap toBaller ballers
 where
   toBaller (BallerObj _ baller) = baller
   toBaller _ = error "impossible -- baller was not a baller"

ownerToBaller :: Int -> Game -> Baller
ownerToBaller n = (!! n) . _gBallers

duplicateAndSwap :: [(a, a)] -> [(a, a)]
duplicateAndSwap as = join $ as >>= \p -> return [p , swap p]

setAt :: [a] -> Int -> a -> [a]
setAt [] _ _      = []
setAt (_:as) 0 a' = a':as
setAt (a:as) n a' = a : setAt as (n-1) a'

