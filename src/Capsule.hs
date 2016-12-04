{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Capsule where

import Data.List (nub)
import Data.Maybe (isJust, fromJust)
import Control.Lens
import Game.Sequoia.Signal
import Game.Sequoia.Types
import Control.Comonad
import Control.Comonad.Store
import Control.Monad.Writer
import Control.Arrow (second)
import Types

data Capsule = Capsule
  { _capName      :: Name
  , _capPos       :: V3
  , _capRadius    :: Double
  , _capHeight    :: Double
  , _capEphemeral :: Bool
  } deriving (Eq, Show)
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
    forces = fmap (normalize . posDif loc . _capPos . snd) realInts
    ints = filter (checkCapsules cap . snd)
         . fmap (\s -> (s, flip peek w s))
         $ filter (/= me) as
    realInts = filter (not . _capEphemeral . snd) ints
    ids = fmap (\s -> (min s me, max s me)) $ fmap fst ints
    mult = ((1 / 10) *)
         . minimum
         . fmap _capRadius
         $ cap : (snd <$> realInts)

stepAllPos :: Ord s
           => [(s, Capsule)]
           -> Writer [(s, s)] [(s, Capsule)]
stepAllPos caps = sequence $ fmap (liftM2 fmap (,) (flip peek w)) as
  where
    w = extend (stepPos as)
      . store (fromJust . flip lookup caps)
      $ head as
    as = fmap fst caps

resolveCapsules :: (Ord a)
               => [(a, Capsule)]
               -> ([(a, Capsule)], [(a, a)])
resolveCapsules = second nub
                . runWriter
                . findFixedPoint stepAllPos

findFixedPoint :: (Eq a, Monad m) => (a -> m a) -> a -> m a
findFixedPoint f a = go a (f a)
  where
     go b mb = do
      b' <- mb
      if b == b'
         then return b
         else go b' (f b')

manage :: ([a] -> N ([a]))
       -> [B ( a
             , (a -> a) -> IO ()
             )]
       -> N (B [a])
manage f mps = poll $ do
  (bs, mbs) <- fmap unzip . sample $ sequenceA mps
  res <- f bs
  forM_ (zip res mbs) $ \(a, mb) -> sync . mb $ const a
  return res

manageCapsules :: [B ( Capsule
                     , (Capsule -> Capsule) -> IO ()
                     )]
               -> N (B [Capsule])
manageCapsules = manage $ \caps -> do
  let (caps', hits) = resolveCapsules $ zip (fmap _capName caps) caps
  when (not $ null hits) $ sync $ putStrLn $ show hits
  return $ fmap snd caps'

class Managed t where
  managedCapsule :: t -> Capsule
  managedInput   :: t -> (Capsule -> Capsule) -> IO ()

managed :: Managed t => t -> (Capsule, (Capsule -> Capsule) -> IO ())
managed t = (managedCapsule t, managedInput t)


