{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Capsule where

import           Control.Arrow (second)
import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad.Writer
import           Data.List (nub)
import           Data.Maybe (isJust)
import           Game.Sequoia.Types
import           Types

moveCapsule :: Rel3 -> Capsule -> Capsule
moveCapsule r3 = capPos %~ flip plusDir r3

capsuleIntersection :: Capsule -> Capsule -> Maybe Rel3
capsuleIntersection a b =
    if (d <= _capRadius a || d <= _capRadius b)
       then Just dif
       else Nothing
  where
    (ax, _, az) = unpackV3 $ _capPos a
    (bx, _, bz) = unpackV3 $ _capPos b
    dif = posDif (_capPos b) (_capPos a)
    d = mag $ posDif (mkPos ax az) (mkPos bx bz)

checkCapsules :: Capsule -> Capsule -> Bool
checkCapsules a b = isJust (capsuleIntersection a b)
                 && ( (ay >= by && ay < by + _capHeight b)
                   || (by >= ay && by < ay + _capHeight a)
                    )
  where
    ay = getY $ _capPos a
    by = getY $ _capPos b


stepPos :: Ord s
        => [s]
        -> Store s Capsule
        -> Writer [(s, s)] Capsule
stepPos as w = do
  tell ids
  pure . flip moveCapsule cap
       . mconcat
       $ fmap (scaleRel mult) forces
  where
    cap = extract w
    loc = _capPos cap
    me  = pos w
    forces =
      if not $ _capEthereal cap
         then fmap (normalize . posDif loc . _capPos . snd)
                    realInts
         else []
    ints = filter (checkCapsules cap . snd)
         . fmap (\s -> (s, flip peek w s))
         $ filter (/= me) as
    realInts = filter (not . _capEthereal . snd) ints
    ids = fmap (\s -> (min s me, max s me)) $ fmap fst ints
    mult = ((1 / 10) *)
         . minimum
         . fmap _capRadius
         $ cap : (snd <$> realInts)

stepAllPos :: Ord s
           => Lens' s Capsule
           -> [s]
           -> Writer [(s, s)] [s]
stepAllPos l caps =
    forM caps $ \s -> do
      newCap <- peek s w
      return $ s & l .~ newCap
  where
    w = extend (stepPos caps)
      . store (view l)
      $ head caps

resolveCapsules :: (Ord s)
               => Lens' s Capsule
               -> [s]
               -> ([s], [(s, s)])
resolveCapsules l = second nub
                  . runWriter
                  . findFixedPoint (stepAllPos l)

findFixedPoint :: (Eq a, Monad m) => (a -> m a) -> a -> m a
findFixedPoint f a = go a (f a)
  where
     go b mb = do
      b' <- mb
      if b == b'
         then return b
         else go b' (f b')

