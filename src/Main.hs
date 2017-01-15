{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Ball
import Baller
import Constants
import Control.FRPNow.Time (delayTime)
import Control.Lens
import Control.Monad.Writer (runWriter)
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

runGame :: Engine -> N (B Element)
runGame _ = do
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
          controllers =
            setAt (replicate 4 $ Controller (V2 0 0) False Nothing)
                  (maybe 0 id $ preview (gBall.ballState._BallOwned) g)
                  ctrl

      case _gMode g of
        Play -> do
              (game', msgs) = runWriter
                            $ updatePlay now dt controllers g
          liftIO $ forM_ msgs putStrLn
          return game'

  return $ do
    game' <- sample game
    case _gMode game' of
      _ -> renderPlay clock game'


main :: IO ()
main = play config runGame return
  where
    config = EngineConfig (round gameWidth, round gameHeight)
                          "Time 2 Jam"
                        $ rgb 0.6 0.6 0.6

