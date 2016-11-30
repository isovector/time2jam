{-# LANGUAGE TemplateHaskell                                      #-}
{-# LANGUAGE TupleSections                                        #-}

module Capsule where

import           Control.Arrow ((***), second)
import           Control.Lens
import           Control.Monad.RWS
import           Data.List (nub)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Game.Sequoia.Types
import           Types

data Capsule = Capsule
  { _capPos :: V3
  , _capRadius :: Double
  , _capHeight :: Double
  }
makeLenses ''Capsule

moveCapsule :: Rel3 -> Capsule -> Capsule
moveCapsule r3 c = capPos %~ flip plusDir r3 $ c

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

stepCapsules :: Ord a
             => RWS () [(a, a)] (Map a (Capsule, Rel3)) ()
stepCapsules = do
  st <- M.toList <$> get
  -- Update positions and set rels to 0
  sequence_ $ do
    (a, (cap, v)) <- st
    return $ modify $ M.insert a (moveCapsule v cap, rel3 0 0 0)

  -- Perform pairwise hit-checks
  st' <- M.toList <$> get
  sequence_ $ do
    (a, (acap, _)) <- st'
    (b, (bcap, _)) <- st'
    guard $ a /= b
    guard $ checkCapsules acap bcap
    let Just dif = scaleRel 0.5 <$> capsuleIntersection acap bcap
    return $ do
      -- Collision!
      tell [(min a b, max a b)]
      modify $ M.insertWith merge a (acap, dif)
      modify $ M.insertWith merge b (bcap, negate dif)

merge :: Num b => (a, b) -> (a, b) -> (a, b)
merge (p1, r1) (_, r2) = (p1, r1 + r2)

updateCapsules :: (Ord a)
               => [(a, Capsule)]
               -> [(a, Rel3)]
               -> ([(a, Capsule)], [(a, a)])
updateCapsules caps rels
    = (fmap cleanup . M.toList *** nub)
    . wat
    . runRWS (replicateM_ precision stepCapsules) ()
    . M.unionWith merge (asMap (, rel3 0 0 0) caps)
    $ asMap (undefined, ) rels
  where
    wat (_, s, w) = (s, w)
    cleanup (a, (x, _)) = (a, x)
    precision = 10
    asMap f = M.fromList . fmap (second f)

