{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Game.Sequoia
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Game.Sequoia.Utils

import Baller
import Capsule
import Data.Default
import Court
import Types
import Camera

getCamera :: B Time -> B [Key] -> N (B Camera)
getCamera clock keys =
  fmap fst . foldmp def $ \cam -> do
    dt <- sample clock
    -- dx <- sample $ arrows keys
    -- let dpos = scaleRel (5 * dt) dx
    return . updateCam dt
           -- . moveCamera (rel3 (getX dpos) 0
           --                    (negate $ getY dpos))
           $ cam


magic :: Engine -> Now (Behavior Prop)
magic _ = do
    clock    <- deltaTime <$> getClock
    keyboard <- getKeyboard
    cam      <- getCamera clock keyboard

    b1 <- makeBaller (mkV3 0 0 0) $ \cap -> do
            dt <- sample clock
            dx <- sample $ arrows keyboard
            let dpos = scaleRel (5 * dt) dx
            return . moveCapsule (rel3 (negate $ getX dpos) 0
                                 (getY dpos))
                   $ cap
    b2 <- makeBaller (mkV3 1 0 0) return
    b3 <- makeBaller (mkV3 2 0 0) return
    b4 <- makeBaller (mkV3 3 0 0) return
    b5 <- makeBaller (mkV3 2 0 0.6) return
    b6 <- makeBaller (mkV3 2 0 (-0.6)) return

    _ <- manageCapsules $ fmap managed <$> [b1, b2, b3, b4, b5, b6]

    return $ do
        cam' <- sample cam
        ballers <- sample $ sequenceA [b1, b2, b3, b4, b5, b6]

        return $ group $ [ drawCourt court cam' ]
                       ++ fmap (drawBaller cam') ballers


main :: IO ()
main = play (EngineConfig (700, 400) "hello") magic return


