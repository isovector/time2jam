{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ViewPatterns #-}

module Motion where

import Types
import Bezier
import Game.Sequoia (Time)
import Control.Monad.Writer
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors


type Machine a = Coroutine (Request V3 Double) (Writer [Action]) a

runBezier :: Time -> [V3] -> V3 -> Machine V3
runBezier duration v3s pos = do
    let b = bezier $ pos : v3s
    loop b 0
    return $ b 1
  where
    loop b t = do
      dt <- request $ b (t / duration)
      case dt + t >= duration of
         True  -> return ()
         False -> loop b (dt + t)

runMotion :: V3 -> Machine ()
runMotion pos = do
  pos1 <- runBezier 1 [mkV3 1 0 0] pos
  pos2 <- runBezier 1 [mkV3 1 1 0] pos1
  lift $ tell [Shoot undefined]
  pos3 <- runBezier 1 [mkV3 0 1 0] pos2
  _ <- runBezier 1 [mkV3 0 0 0] pos3
  return ()

pump :: Machine a -> Writer [Action] [V3]
pump = resume >=> \case
  Left (Request v3 c) -> (v3 :) <$> pump (c 0.1)
  Right _ -> return []

showV3 :: V3 -> String
showV3 (unpackV3 -> (x, y, z)) = take 5 (show x) <> "\t"
                              <> take 5 (show y) <> "\t"
                              <> take 5 (show z)


