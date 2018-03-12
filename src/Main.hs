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
    foldmp (OnMoon initGame defaultBaller 0) $ \gs -> do
      now    <- sample $ totalTime clock
      dt     <- sample $ deltaTime clock
      rctrl  <- sample oldCtrl
      rctrl' <- sample controller

      case gs of
        InGame g -> InGame <$> do
          let ctrl = foldController rctrl rctrl'
              controllers =
                setAt (replicate 4 $ Controller (V2 0 0) False Nothing)
                      (maybe 0 id $ preview (gBall.ballState._BallOwned) g)
                      ctrl

          case _gMode g of
            Play -> do
              let (game', msgs) = runWriter
                                $ updatePlay now dt controllers g
              liftIO $ forM_ msgs putStrLn
              pure game'
            TurnOver net False -> pure $ sendTurnoverMovement net g
            TurnOver _ True    -> pure $ waitForMotion now dt g

        x@(OnMoon {..}) ->
          pure $ case _gsProgress >= pi * 2 of
            True -> InGame $ _gsOldGame
            False -> x & gsProgress +~ dt

  pure $ sample game >>= \case
    InGame g     -> renderPlay clock g
    OnMoon _ b f -> drawMoonDunk b f


main :: IO ()
main = play config runGame pure
  where
    config = EngineConfig (round gameWidth, round gameHeight)
                          "Time 2 Jam"
                        $ rgb 0.6 0.6 0.6

