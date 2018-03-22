{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeApplications            #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Camera
import Control.Monad.Writer (runWriter)
import Ball
import Baller
import Basket
import Constants
import Control.FRPNow.Time (delayTime)
import Control.Lens
import Court
import Game.Sequoia.Keyboard
import Input
import JamPrelude


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


-- update ballers
-- check for shots
-- resolve capsules
-- focus camera
-- determine collisions
-- check for ball collision
-- update ball
-- run shoves
-- change game mode if necessary

runGame :: Engine -> N (B Element)
runGame _ = do
  clock      <- getClock
  controller <- keyboardController <$> getKeyboard
  oldCtrl    <- sample $ delayTime (deltaTime clock) def controller

  start <- flip runEcstasy (0, defWorld) $ do
    newEntity $ defEntity { camera = Just def }
    for_ (_gBallers initGame) $ \b ->
      newEntity defEntity
        { baller = Just b
        , avatar = Just ()
        , focus  = Just ()
        }

  game <- fmap fst . foldmp start . runEcstasy $ do
    (now, dt, rctrl, rctrl') <- lift . sample $ (,,,) <$> totalTime clock
                                                      <*> deltaTime clock
                                                      <*> oldCtrl
                                                      <*> controller
    let ctrl' = foldController rctrl rctrl'

    mcf <- fmap listToMaybe . efor . const $ do
      with focus
      get $ fmap (view $ bCap . capPos) . baller
    for_ mcf $ \cf -> emap $ do
      c <- get camera
      pure defEntity'
        { camera = Set $ updateCam dt $ c & camFocus .~ cf
        }

    emap $ with avatar >> pure defEntity'
      { ctrl    = Set ctrl'
      , rawCtrl = Set rctrl'
      }

    emap $ do
      b <- get baller
      c <- get ctrl
      pure defEntity'
        { baller = Set
                 . fst
                 . runWriter
                 $ updateBaller now dt (PlayBaller c Doesnt) b
        }

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
    g   <- sample game
    now <- sample $ totalTime clock

    sample $ fmap snd $ yieldSystemT g $ do
      cam     <- fmap (maybe def id . listToMaybe) . efor . const $ get camera
      ballers <- efor . const $ get baller

      pure $ centeredCollage (round gameWidth)
                             (round gameHeight)
           $ [ drawCourt court cam
             , drawBasket cam RNet
             , drawBasket cam LNet
             ] ++
           fmap (drawBaller cam now) ballers


main :: IO ()
main = play config runGame pure
  where
    config = EngineConfig (round gameWidth, round gameHeight)
                          "Time 2 Jam"
                        $ rgb 0.6 0.6 0.6

