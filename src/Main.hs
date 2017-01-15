{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Ball
import Baller
import Basket
import Constants
import Control.FRPNow.Time (delayTime)
import Control.Lens
import Control.Monad.Writer (runWriter)
import Court
import Data.List (sortBy)
import Data.Ord (comparing)
import Game.Sequoia.Keyboard
import Input
import JamPrelude
import ModeGame

initGame :: Game
initGame = Game
  { _gCamera  = def
  , _gBall    = defaultBall
  , _gBallers = [ defaultBaller & bCap.capPos .~ V3 (-2) 0 (-2)
                , defaultBaller & bCap.capPos .~ V3 (-2) 0 2
                , otherBaller & bCap.capPos .~ V3 2 0 2
                , otherBaller & bCap.capPos .~ V3 2 0 (-2)
                ]
  , _gMode = Play
  }

render :: Engine -> N (B Element)
render _ = do
  clock      <- getClock
  controller <- keyboardController <$> getKeyboard
  oldCtrl    <- sample $ delayTime (deltaTime clock) def controller

  (game, _) <-
    foldmp initGame $ \g -> do
      now    <- sample $ totalTime clock
      dt     <- sample $ deltaTime clock
      rctrl  <- sample oldCtrl
      rctrl' <- sample controller
      let ctrl = foldController rctrl rctrl'
          controllers = setAt (replicate 4 $ Controller (V2 0 0) False Nothing)
                              (maybe 0 id $ preview (gBall.ballState._BallOwned) g)
                              ctrl
          (game', msgs) = runWriter
                        $ updateGame now dt controllers g
      liftIO $ forM_ msgs putStrLn
      return game'

  return $ do
    g@Game {..} <- sample game
    let cam = _gCamera
    now <- sample $ totalTime clock

    return $ centeredCollage (round gameWidth) (round gameHeight) $
           [ drawCourt court cam
           , drawBasket cam RNet
           , drawBasket cam LNet
           , drawBall cam
                      (flip ownerToBaller g
                          <$> preview (ballState._BallOwned) _gBall)
                      _gBall
           ] ++ fmap (drawBaller cam now) (sortBy (comparing $ view $ bCap.capPos._z) _gBallers)


main :: IO ()
main = play config render return
  where
    config = EngineConfig (round gameWidth, round gameHeight)
                          "Time 2 Jam"
                        $ rgb 0.6 0.6 0.6

