{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Capsule where

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad.Writer
import qualified Data.Map as M
import           Data.Maybe (isJust, fromJust)
import           Game.Sequoia.Types
import           Types

data Capsule = Capsule
  { _capPos :: V3
  , _capRadius :: Double
  , _capHeight :: Double
  }
makeLenses ''Capsule

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
    forces = fmap (posDif loc . _capPos . snd) ints
    ints = filter (checkCapsules cap . snd)
         . fmap (\s -> (s, flip peek w s))
         $ filter (/= me) as
    ids = fmap (\s -> (min s me, max s me)) $ fmap fst ints
    mult = 1 / fromIntegral (length forces)

stepAllPos :: forall s. Ord s
           => [(s, Capsule)]
           -> Writer [(s, s)] [(s, Capsule)]
stepAllPos caps = sequence $ fmap (liftM2 fmap (,) (flip peek w)) as
  where
    w :: Store s (Writer [(s, s)] Capsule)
    w = extend (stepPos as)
      . store (fromJust . flip lookup caps)
      $ head as
    as = fmap fst caps

updateCapsules :: (Ord a)
               => [(a, Capsule)]
               -> [(a, Rel3)]
               -> ([(a, Capsule)], [(a, a)])
updateCapsules caps rels
    = runWriter
    . stepAllPos
    . M.toList
    . M.differenceWith ((Just .) . flip moveCapsule)
                       (M.fromList caps)
    $ M.fromList rels

findFixedPoint :: (Eq a, Monad m) => (a -> m a) -> a -> m a
findFixedPoint f a = do
  l <- sequence
     . iterate (>>= f)
     $ pure a
  return . fst . head
         . dropWhile (uncurry (/=))
         $ pairwise (,) l
  where
    pairwise g xs = zipWith g xs $ tail xs
