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
import Control.Lens
import Court
import Data.Default
import Game.Sequoia
import Game.Sequoia.Keyboard
import Game.Sequoia.Utils
import Input
import Types
import Data.Tuple (swap)
import Control.Monad (join)
import Data.Maybe (listToMaybe)

data Game = Game
  { _gCamera  :: Camera
  , _gBall    :: Ball
  , _gBaller1 :: Baller
  }

initGame :: Game
initGame = Game
  { _gCamera  = def
  , _gBall    = defaultBall
  , _gBaller1 = defaultBaller
  }

duplicate :: [(a, a)] -> [(a, a)]
duplicate as = join $ as >>= \p -> return [p , swap p]

updateGame :: Time -> Controller -> Game -> Game
updateGame dt ctrl Game{..} =
  let baller1 = updateBaller dt ctrl _gBaller1
      ([ BallObj ball
       , BallerObj _ baller1'
       ]
       , hits) = resolveCapsules objCap
                  [ BallObj _gBall
                  , BallerObj 1 baller1
                  ]
      camera' = updateCam dt
              $ _gCamera
              & camFocus .~ view (ballCap . capPos) ball'
      allHits  = duplicate hits
      ballHits = fmap snd $ filter (isBall . fst) allHits
      ball'    = updateBall dt
                            (fmap snd . preview _BallerObj =<< listToMaybe ballHits)
                            ball
   in Game camera' ball' baller1'

magic :: Engine -> N (B Prop)
magic _ = do
  clock      <- deltaTime          <$> getClock
  controller <- keyboardController <$> getKeyboard

  (game, _) <-
    foldmp initGame $ \g -> do
      dt   <- sample clock
      ctrl <- sample controller
      return $ updateGame dt ctrl g

  return $ do
    Game {..} <- sample game
    let cam = _gCamera

    return $ group $ [ drawCourt court cam
                     , drawBasket cam unitX
                     , drawBasket cam (-unitX)
                     , drawBall cam _gBall
                     , drawBaller cam _gBaller1
                     ]


main :: IO ()
main = play config magic return
  where config = EngineConfig (700, 400) "hello" $ rgb 0.6 0.6 0.6


