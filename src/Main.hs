{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE RecursiveDo                 #-}
{-# LANGUAGE ScopedTypeVariables         #-}
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
import Court
import Data.Default
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

initGame :: Game
initGame = Game
  { _gCamera  = def
  , _gBall    = defaultBall
  , _gBaller0 = defaultBaller
  }

duplicate :: [(a, a)] -> [(a, a)]
duplicate as = join $ as >>= \p -> return [p , swap p]

updateGame :: Time -> Controller -> Maybe Keypress -> Game -> Game
updateGame dt ctrl kp Game{..} =
  let baller0 = updateBaller dt ctrl kp _gBaller0
      ([ BallObj ball
       , BallerObj _ baller0'
       ], hits
       ) = resolveCapsules objCap
             [ BallObj _gBall
             , BallerObj 0 baller0
             ]
      camera' = updateCam dt
              $ _gCamera
              & camFocus .~ view (ballCap . capPos) ball'
      allHits  = duplicate hits
      ballHits = fmap snd $ filter (isBall . fst) allHits
      ball'    = updateBall dt
                            (fmap fst . preview _BallerObj =<< listToMaybe ballHits)
                            [baller0']
                            ball
   in Game camera' ball' baller0'

magic :: Engine -> N (B Prop)
magic _ = do
  clock      <- deltaTime          <$> getClock
  controller <- keyboardController <$> getKeyboard
  oldCtrl    <- sample $ delayTime clock def controller

  (game, _) <-
    foldmp initGame $ \g -> do
      dt   <- sample clock
      ctrl  <- sample oldCtrl
      ctrl' <- sample controller
      return $ updateGame dt ctrl' (getKP ctrl ctrl') g

  return $ do
    Game {..} <- sample game
    let cam = _gCamera

    return $ group $ [ drawCourt court cam
                     , drawBasket cam unitX
                     , drawBasket cam (-unitX)
                     , drawBall cam _gBall
                     , drawBaller cam _gBaller0
                     ]


main :: IO ()
main = play config magic return
  where config = EngineConfig (700, 400) "hello" $ rgb 0.6 0.6 0.6


