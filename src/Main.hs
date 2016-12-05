{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TupleSections               #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Data.Bool (bool)
import Baller
import Basket
import Camera
import Capsule
import Control.Lens
import Court
import Data.Default
import Game.Sequoia
import Game.Sequoia.Keyboard
import Input
import Types
import Objects

getCamera :: B Time -> B V3 -> N (B Camera)
getCamera clock pos =
  fmap fst . foldmp def $ \cam -> do
    dt    <- sample clock
    focus <- sample pos
    return . updateCam dt
           $ cam & camFocus .~ focus


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

  let evs = actionEvents ctrl b1
  onEvent evs $ sync . putStrLn . show

  cam <- getCamera clock $ view (bCap.capPos) <$> b1
  let netDetector = detector NNetL (mkV3 3 0 0) 1 1

  bs <- manageCapsules $ pure netDetector : (fmap managed <$> [b1])

  return $ do
    cam' <- sample cam
    ballers  <- sample $ sequenceA [b1]
    ballers' <- reconcile _capName bCap ballers <$> sample bs

    return $ group $ [ drawCourt court cam'
                     , drawBasket cam' unitX
                     , drawBasket cam' (-unitX)
                     ]
                   ++ fmap (drawBaller cam') ballers'


main :: IO ()
main = play config magic return
  where config = EngineConfig (700, 400) "hello" $ rgb 0.6 0.6 0.6


