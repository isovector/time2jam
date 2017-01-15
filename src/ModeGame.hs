{-# LANGUAGE RecordWildCards #-}

module ModeGame where

import AnimBank
import Court
import Constants
import Ball
import Baller
import Basket
import Camera
import Capsule
import Control.Lens
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.List (find, partition)
import Data.List (sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Tuple (swap)
import JamPrelude
import Motion

updatePlay :: Time
           -> Time
           -> [Controller]
           -> Game
           -> Writer [String] Game
updatePlay now dt ctrls g = do
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

  _ <- handleActions allActs _Debug $ tell . return
  _ <- handleActions allActs _Point $ \(n, p) ->
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

renderPlay :: Clock -> Game -> B Element
renderPlay clock g@Game {..} = do
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
           ] ++ fmap (drawBaller cam now)
                     (sortBy (comparing $ view $ bCap.capPos._z) _gBallers)

