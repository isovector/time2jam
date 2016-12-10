{-# LANGUAGE RecursiveDo                 #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TupleSections               #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Data.Bool (bool)
import Ball
import Baller
import Basket
import Camera
import Capsule
import Control.Lens
import Court
import Data.Default
import Game.Sequoia
import Game.Sequoia.Keyboard
import Control.FRPNow.EvStream
import Input
import Types

getCamera :: B Time -> B V3 -> N (B Camera)
getCamera clock pos =
  fmap fst . foldmp def $ \cam -> do
    dt    <- sample clock
    focus <- sample pos
    return . updateCam dt
           $ cam & camFocus .~ focus

delaying :: B Time -> a -> B a -> B (B a)
delaying time a b = delay (toChanges time) a b

magic :: Engine -> N (B Prop)
magic _ = do
  clock <- deltaTime <$> getClock
  ctrl  <- keyboardController <$> getKeyboard

  b1 <- makeBaller 0 (mkV3 0 0 0) unitX $ \cap -> do
          dt    <- sample clock
          dx    <- sample $ _ctrlDir <$> ctrl
          turbo <- fmap (bool 1 1.5)
                 . sample
                 $ _ctrlTurbo <$> ctrl
          let dpos = scaleRel (5 * turbo * dt) dx
              dir  = rel3 (getX dpos) 0 (getY dpos)
          return (moveCapsule dir cap, dir)

  rec
    delayedBallers <- sample $ delaying clock [] ballers
    ball <-
      makeBall (mkV3 (-2) 1 0) $ \b -> do
        bs <- sample delayedBallers
        return $ case _ballOwner b of
                   Just owner -> b & ballCap . capPos .~
                                     view (bCap . capPos) owner
                   Nothing -> b

    let evs = actionEvents ctrl b1
    onEvent evs $ sync . putStrLn . show

    cam <- getCamera clock $ view (bCap.capPos) <$> b1

    let unmanagedBallers = [b1]
    caps <- manageCapsules $ fmap managed ball
                          : (fmap managed <$> [b1])
    let ballers = reconcile _capName bCap <$> sequenceA unmanagedBallers
                                          <*> caps

  return $ do
    cam' <- sample cam
    ballers' <- sample ballers
    ball' <- sample ball

    return $ group $ [ drawCourt court cam'
                     , drawBasket cam' unitX
                     , drawBasket cam' (-unitX)
                     , drawBall cam' ball'
                     ]
                   ++ fmap (drawBaller cam') ballers'


main :: IO ()
main = play config magic return
  where config = EngineConfig (700, 400) "hello" $ rgb 0.6 0.6 0.6


