{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}

module Motion where

import Types
import Game.Sequoia
import Bezier
import Control.Monad.Writer
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

motion :: Machine V3 -> Motion
motion = Motion . const

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

velBezier :: Double -> [V3] -> V3 -> Machine V3
velBezier velocity v3s pos = runBezier (bezierDuration velocity v3s pos) v3s pos

bezierDuration :: Double -> [V3] -> V3 -> Time
bezierDuration velocity v3s pos =
    let len = bezierLength $ pos : v3s
     in len / velocity

runMotion :: Time -> Motion -> Writer [Action] (V3, Maybe Motion)
runMotion dt (Motion m) = (resume $ m dt) >>= \case
  Left (Request v3 c) -> return (v3, Just $ Motion c)
  Right v3            -> return (v3, Nothing)

