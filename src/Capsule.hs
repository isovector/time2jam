{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Capsule where

import Control.Comonad
import Control.Comonad.Store
import Control.Lens ((+~), Lens')
import Control.Monad.Writer
import Data.List (nub, foldl')
import Data.Maybe (isJust, mapMaybe)
import JamPrelude
import Motion

moveCapsule :: V3 -> Capsule -> Capsule
moveCapsule r3 = capPos +~ r3

capsuleIntersection :: Capsule -> Capsule -> Maybe V3
capsuleIntersection a b =
    if (d <= _capRadius a || d <= _capRadius b)
       then Just dif
       else Nothing
  where
    (ax, _, az) = unpackV3 $ _capPos a
    (bx, _, bz) = unpackV3 $ _capPos b
    dif = _capPos b - _capPos a
    d = norm $ V2 ax az - V2 bx bz

checkCapsules :: Capsule -> Capsule -> Bool
checkCapsules a b = isJust (capsuleIntersection a b)
                 && ( (ay >= by && ay < by + _capHeight b)
                   || (by >= ay && by < ay + _capHeight a)
                    )
  where
    ay = a ^. capPos._y
    by = b ^. capPos._y


stepPos :: Ord s
        => [s]
        -> Store s Capsule
        -> Writer [(s, s)] Capsule
stepPos as w = do
  tell ids
  pure . flip moveCapsule cap
       . sum
       $ fmap (mult *^) forces
  where
    cap = extract w
    loc = _capPos cap
    me  = pos w
    isMovable = not $ _capEthereal cap || isJust (_capMotion cap)
    forces =
      if isMovable
         then fmap (signorm . (loc -) . _capPos . snd)
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
                  . findFixedPoint 1 (stepAllPos l)

findFixedPoint :: (Eq a, Monad m) => Int -> (a -> m a) -> a -> m a
findFixedPoint n f a = go n a (f a)
  where
     go 0 _ mb = mb
     go i b mb = do
      b' <- mb
      if b == b'
         then return b
         else go (i - 1) b' (f b')

updateCapsule :: Time -> Capsule -> Writer [Action] Capsule
updateCapsule dt c@Capsule{..}
  | Just m <- _capMotion = do
      (pos', motion') <- runMotion dt m
      return $ c & capPos .~ pos'
                 & capMotion .~ motion'
  | otherwise = return c

updateCapsuleAndAnim :: Time
                     -> Time
                     -> Art
                     -> Capsule
                     -> Writer [Action] (Capsule, Art)
updateCapsuleAndAnim now dt art c = do
  (c', w) <- censor (filter $ isn't _PlayAnimation)
           . listen
           $ updateCapsule dt c

  let art' = foldl' (((aStarted .~ now) .) . flip (set aAnim)) art
           $ mapMaybe (preview _PlayAnimation) w
  return (c', art')

makeMotion :: Time -> [V3] -> Capsule -> Motion
makeMotion duration v3s c = motion
                          . runBezier duration v3s
                          $ _capPos c

moveTo :: Time -> [V3] -> Capsule -> Capsule
moveTo duration v3s = flip setMotionwtf (makeMotion duration v3s)

setMotionwtf :: Capsule -> (Capsule -> Motion) -> Capsule
setMotionwtf c m = c & capMotion .~ Just (m c)

setMotion :: Capsule -> Motion -> Capsule
setMotion c m = c & capMotion .~ Just m

