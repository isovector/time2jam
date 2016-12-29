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
import Control.Monad (join, forM_, forM)
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.IO.Class (liftIO)
import Court
import Data.Bool (bool)
import Data.Default
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Tuple (swap)
import Game.Sequoia
import Game.Sequoia.Keyboard
import Input
import Types

data Game = Game
  { _gCamera  :: Camera
  , _gBall    :: Ball
  , _gBallers :: [Baller]
  }
makeLenses ''Game

initGame :: Game
initGame = Game
  { _gCamera  = def
  , _gBall    = defaultBall
  , _gBallers = [ defaultBaller
                , otherBaller
                ]
  }

withObjects :: (Monad m) => Game -> ([GObject] -> m [GObject]) -> m Game
withObjects g@Game{..} f = do
  (BallObj ball : ballers) <- f . (BallObj _gBall :)
                                . fmap (uncurry BallerObj)
                                $ zip [0..] _gBallers
  return $ g & gBall .~ ball
             & gBallers .~ fmap toBaller ballers
 where
   toBaller (BallerObj _ baller) = baller
   toBaller _ = error "impossible -- baller was not a baller"

ownerToBaller :: Int -> Game -> Baller
ownerToBaller n = (!! n) . _gBallers

duplicate :: [(a, a)] -> [(a, a)]
duplicate as = join $ as >>= \p -> return [p , swap p]

updateGame :: Time
           -> Controller
           -> Maybe Keypress
           -> Game
           -> Writer [String] Game
updateGame dt ctrl kp g = do
  let (ballers, ballerActs) = runWriter
                            $ forM (zip (_gBallers g) [0..]) $ \(baller, n) ->
                                updateBaller dt ctrl kp (possesses n) baller
      shotAction = find (has _Shoot) ballerActs

      (hits, g') = withObjects (g & gBallers .~ ballers)
                               (swap <$> resolveCapsules objCap)
      ball = _gBall g'
      camera' = updateCam dt
              $ _gCamera g
              & camFocus .~ view (ballCap . capPos) ball
      allHits  = duplicate hits
      ballHits = fmap snd $ filter (isBall . fst) allHits
      (ball', ballActs) =
        runWriter $
          updateBall dt
                     (fmap fst . preview _BallerObj
                        =<< listToMaybe ballHits)
                     ballers
                     shotAction
                     ball
      allActs = ballerActs ++ ballActs

      g'' = runIdentity $ withObjects (g' & gBall .~ ball')
                        (Identity . doShove (mapMaybe (preview _Shove) ballerActs))

  tell $ mapMaybe (preview _Debug) allActs
  return $ g'' & gCamera .~ camera'
 where
   possesses i = maybe Doesnt
                       (bool Doesnt Has . (== i))
                       $ preview (ballState._BallOwned) $ _gBall g

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
      let (game', msgs) = runWriter
                        $ updateGame dt ctrl' (getKP ctrl ctrl') g
      liftIO $ forM_ msgs putStrLn
      return game'

  return $ do
    g@Game {..} <- sample game
    let cam = _gCamera
    now <- sample $ totalTime clock

    return $ group $
           [ drawCourt court cam
           , drawBasket cam RNet
           , drawBasket cam LNet
           , drawBall cam
                      now
                      (flip ownerToBaller g
                          <$> preview (ballState._BallOwned) _gBall)
                      _gBall
           ] ++ fmap (drawBaller cam) _gBallers


main :: IO ()
main = play config magic return
  where config = EngineConfig (700, 400) "hello" $ rgb 0.6 0.6 0.6


