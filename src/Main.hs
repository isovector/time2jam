{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import AnimBank
import Ball
import Baller
import Basket
import Camera
import Capsule
import Constants
import Control.FRPNow.Time (delayTime)
import Control.Lens
import Control.Monad.Writer (Writer, runWriter, tell)
import Court
import Data.List (find, partition, sortBy)
import Data.Ord (comparing)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Tuple (swap)
import Game.Sequoia.Keyboard
import Input
import JamPrelude
import Motion

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

duplicateAndSwap :: [(a, a)] -> [(a, a)]
duplicateAndSwap as = join $ as >>= \p -> return [p , swap p]

updateGame :: Time
           -> Time
           -> [Controller]
           -> Game
           -> Writer [String] Game
updateGame now dt ctrls g = do
  let (ballers, ballerActs) = runWriter
                            $ forM (zip3 (_gBallers g) ctrls [0..]) $ \(baller, ctrl, n) ->
                                updateBaller now dt ctrl (possesses n) baller
      shotAction = find (liftM2 (||) (has _Shoot) (has _Pass)) ballerActs

      (hits, g') = withObjects (g & gBallers .~ ballers)
                               (swap <$> resolveCapsules objCap)
      ball = _gBall g'
      camera' = updateCam dt
              $ _gCamera g
              & camFocus .~ view (ballCap . capPos) ball
      allHits  = duplicateAndSwap hits
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

      g'' = runIdentity $ withObjects (g' & gBall .~ ball'
                                          & gCamera .~ camera')
                        (Identity . doShove (getActions ballerActs _Shove))

  handleActions allActs _Debug $ tell . return
  handleActions allActs _Point $ \(n, p) ->
    tell . pure $ show p <> " points for " <> show n

  return $ onAction allActs (_ChangeGameMode._TurnOver) g'' $ \net ->
    let (off, _) = partition ((== net) . view bFwd) $ _gBallers g''
        isOff = flip elem off
        ballers' = do
          (baller, i) <- zip (_gBallers g'') [0..]
          return $ baller & bCap.capMotion .~ Just (
            motion $ do
              let pos = baller ^. bCap.capPos
              wait 0 pos
              emit $ PlayAnimation __bRun
              velBezier ( baller ^. bStats.sSpeed
                          * baller ^. bStats.sTurboMult
                        )
                [ (bool turnoverDefPos
                        turnoverOffPos
                        $ isOff baller
                  ) (i `mod` 2) net
                ] $ pos
            )
     in g'' & gBallers .~ ballers'

 where
   getActions acts p = mapMaybe (preview p) acts
   handleActions acts p f = forM (getActions acts p) f
   onAction acts p g_ f = maybe g_ f . listToMaybe $ getActions acts p
   possesses i = maybe Doesnt
                       (bool Doesnt Has . (== i))
                       $ preview (ballState._BallOwned) $ _gBall g

setAt :: [a] -> Int -> a -> [a]
setAt [] _ _      = []
setAt (_:as) 0 a' = a':as
setAt (a:as) n a' = a : setAt as (n-1) a'

magic :: Engine -> N (B Element)
magic _ = do
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
main = play config magic return
  where config = EngineConfig (round gameWidth, round gameHeight) "hello" $ rgb 0.6 0.6 0.6


