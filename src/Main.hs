{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeApplications            #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Game.Sequoia.Utils
import AnimBank
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
import Art


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


runEcstasy
    :: Sys a
    -> SystemState ECSWorld
    -> N (SystemState ECSWorld)
runEcstasy m = fmap fst . flip yieldSystemT m


runGame :: Engine -> N (B Element)
runGame _ = do
  clock      <- getClock
  controller <- keyboardController <$> getKeyboard
  oldCtrl    <- sample $ delayTime (deltaTime clock) def controller

  start <- flip runEcstasy (0, defWorld) $ do
    void $ newEntity $ defEntity
      { art = Just $ (Art __bDribble 0)
      , color = Just $ rgb 1 0 0
      }

  game <- fmap fst . foldmp start . runEcstasy $ do
    (now, dt, rctrl, rctrl') <- lift . sample $ (,,,) <$> totalTime clock
                                                      <*> deltaTime clock
                                                      <*> oldCtrl
                                                      <*> controller
    let ctrl = foldController rctrl rctrl'
    pure ()

  -- (game, _) <-
  --   foldmp (OnMoon initGame defaultBaller 0) $ \gs -> do

  --     case gs of
  --       InGame g -> InGame <$> do
  --         let ctrl = foldController rctrl rctrl'
  --             controllers =
  --               setAt (replicate 4 $ Controller (V2 0 0) False Nothing)
  --                     (maybe 0 id $ preview (gBall.ballState._BallOwned) g)
  --                     ctrl

  --         case _gMode g of
  --           Play -> do
  --             let (game', msgs) = runWriter
  --                               $ updatePlay now dt controllers g
  --             liftIO $ forM_ msgs putStrLn
  --             pure game'
  --           TurnOver net False -> pure $ sendTurnoverMovement net g
  --           TurnOver _   True  -> pure $ waitForMotion now dt g

  --       x@(OnMoon {..}) ->
  --         pure $ case _gsProgress >= pi * 2 of
  --           True -> InGame $ _gsOldGame
  --           False -> x & gsProgress +~ dt

  pure $ do
    g <- sample game
    now <- sample $ totalTime clock
    sample $ fmap snd $ yieldSystemT g $ do
      camera <- fmap head . efor . const $ get camera

      ballers <- efor . const $ (,,) <$> get cap
                                     <*> get art
                                     <*> getMaybe color
      pure $ centeredCollage (round gameWidth)
                             (round gameHeight)
           $ fmap (\(p, a, c) -> drawArt a c now)
           $ ballers


main :: IO ()
main = play config runGame pure
  where
    config = EngineConfig (round gameWidth, round gameHeight)
                          "Time 2 Jam"
                        $ rgb 0.6 0.6 0.6

