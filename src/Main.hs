{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Art
import Ball
import Baller
import Basket
import Camera
import Capsule
import Control.FRPNow.Time (delayTime)
import Control.Lens
import Control.Monad.Writer (Writer, runWriter, tell)
import Court
import Game.Sequoia.Keyboard
import Data.List (find, partition)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Tuple (swap)
import Input
import JamPrelude
import Motion

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
  , _gBallers = [ defaultBaller & bCap.capPos .~ V3 2 0 (-2)
                , defaultBaller & bCap.capPos .~ V3 2 0 2
                , otherBaller & bCap.capPos .~ V3 (-2) 0 2
                , otherBaller & bCap.capPos .~ V3 (-2) 0 (-2)
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
           -> [Controller]
           -> Game
           -> Writer [String] Game
updateGame dt ctrls g = do
  let (ballers, ballerActs) = runWriter
                            $ forM (zip3 (_gBallers g) ctrls [0..]) $ \(baller, ctrl, n) ->
                                updateBaller dt ctrl (possesses n) baller
      shotAction = find (liftM2 (||) (has _Shoot) (has _Pass)) ballerActs

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

      g'' = runIdentity $ withObjects (g' & gBall .~ ball'
                                          & gCamera .~ camera')
                        (Identity . doShove (getActions ballerActs _Shove))

  handleActions allActs _Debug (tell . return)
  return $ onAction allActs _TurnOver g'' $ \net ->
    let (off, _) = partition ((== net) . view bFwd) $ _gBallers g''
        isOff = flip elem off
        ballers' = do
          (baller, i) <- zip (_gBallers g'') [0..]
          return $ baller & bCap.capMotion .~ Just (
            motion $ velBezier ( baller ^. bStats.sSpeed
                               * baller ^. bStats.sTurboMult
                               )
                   [ (bool turnoverDefPos
                           turnoverOffPos
                           $ isOff baller
                     ) (i `mod` 2) net
                   ] $ baller ^. bCap.capPos
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
  schema     <- getArt

  (game, _) <-
    foldmp initGame $ \g -> do
      dt    <- sample $ deltaTime clock
      rctrl  <- sample oldCtrl
      rctrl' <- sample controller
      let ctrl = foldController rctrl rctrl'
          controllers = setAt (replicate 4 $ Controller (V2 0 0) False Nothing)
                              (maybe 0 id $ preview (gBall.ballState._BallOwned) g)
                              ctrl
          (game', msgs) = runWriter
                        $ updateGame dt controllers g
      liftIO $ forM_ msgs putStrLn
      return game'

  return $ do
    g@Game {..} <- sample game
    let cam = _gCamera
    now <- sample $ totalTime clock
    let skel = move (V2 (-40) (-150))
             . toForm
             . centeredCollage 80 300
             . return
             $ scale 0.2
             $ doAnimation schema $ ((round $ now * 100) `mod` 300)

    return $ centeredCollage 700 400 $
           [ drawCourt court cam
           , drawBasket cam RNet
           , drawBasket cam LNet
           , drawBall cam
                      now
                      (flip ownerToBaller g
                          <$> preview (ballState._BallOwned) _gBall)
                      _gBall
           , move (toScreen cam $ V3 10 0 (-5)) skel
           ] ++ fmap (drawBaller cam) _gBallers


main :: IO ()
main = play config magic return
  where config = EngineConfig (700, 400) "hello" $ rgb 0.6 0.6 0.6


