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
import Camera
import Capsule
import Control.FRPNow.Time (delayTime)
import Control.Lens
import Control.Monad (join)
import Control.Monad.Writer (runWriter)
import Court
import Data.Bool (bool)
import Data.Default
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Tuple (swap)
import Game.Sequoia
import Game.Sequoia.Keyboard
import Input
import Types

data Game = Game
  { _gCamera  :: Camera
  , _gBall    :: Ball
  , _gBaller0 :: Baller
  }
makeLenses ''Game

initGame :: Game
initGame = Game
  { _gCamera  = def
  , _gBall    = defaultBall
  , _gBaller0 = defaultBaller
  }

ownerToBaller :: Int -> Game -> Baller
ownerToBaller 0 = _gBaller0
ownerToBaller _ = error "update ownerToBaller"

duplicate :: [(a, a)] -> [(a, a)]
duplicate as = join $ as >>= \p -> return [p , swap p]

updateGame :: Time -> Controller -> Maybe Keypress -> Game -> Game
updateGame dt ctrl kp Game{..} =
  let (baller0, actions) =
        runWriter $ updateBaller dt ctrl kp (possesses 0) _gBaller0
      shotAction = find isShootAction actions

      ([ BallObj ball
       , BallerObj _ baller0'
       ], hits
       ) = resolveCapsules objCap
             [ BallObj _gBall
             , BallerObj 0 baller0
             ]
      ballers = [baller0']
      camera' = updateCam dt
              $ _gCamera
              & camFocus .~ view (ballCap . capPos) ball'
      allHits  = duplicate hits
      ballHits = fmap snd $ filter (isBall . fst) allHits
      ball'    = updateBall dt
                   (fmap fst . preview _BallerObj
                      =<< listToMaybe ballHits)
                   ballers
                   shotAction
                   ball

   in Game camera' ball' baller0'
 where
   possesses i = maybe Doesnt
                       (bool Doesnt Has . (== i))
                       $ preview (ballState._BallOwned) _gBall

magic :: Engine -> N (B Prop)
magic _ = do
  clock      <- getClock
  controller <- keyboardController <$> getKeyboard
  oldCtrl    <- sample $ delayTime (deltaTime clock) def controller

  (game, _) <-
    foldmp initGame $ \g -> do
      dt    <- sample $ deltaTime clock
      ctrl  <- sample oldCtrl
      ctrl' <- sample controller
      return $ updateGame dt ctrl' (getKP ctrl ctrl') g

  return $ do
    g@Game {..} <- sample game
    let cam = _gCamera
    now <- sample $ totalTime clock

    return $ group
           [ drawCourt court cam
           , drawBasket cam RNet
           , drawBasket cam LNet
           , drawBall cam
                      now
                      (flip ownerToBaller g <$> preview (ballState._BallOwned) _gBall)
                      _gBall
           , drawBaller cam _gBaller0
           ]


main :: IO ()
main = play config magic return
  where config = EngineConfig (700, 400) "hello" $ rgb 0.6 0.6 0.6


