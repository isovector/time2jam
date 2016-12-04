{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Baller
import Basket
import Camera
import Capsule
import Control.Lens
import Court
import Data.Default
import Game.Sequoia
import Game.Sequoia.Keyboard
import Types

getCamera :: B Time -> B V3 -> N (B Camera)
getCamera clock pos =
  fmap fst . foldmp def $ \cam -> do
    dt    <- sample clock
    focus <- sample pos
    return . updateCam dt
           $ cam & camFocus .~ focus


magic :: Engine -> Now (Behavior Prop)
magic _ = do
    clock    <- deltaTime <$> getClock
    keyboard <- getKeyboard

    b1 <- makeBaller (mkV3 0 0 0) $ \cap -> do
            dt <- sample clock
            dx <- sample $ arrows keyboard
            let dpos = scaleRel (5 * dt) dx
            return . moveCapsule (rel3 (getX dpos) 0
                                 (getY dpos))
                   $ cap
    cam <- getCamera clock $ view (bCap.capPos) <$> b1

    b2 <- makeBaller (mkV3 1 0 0) return
    b3 <- makeBaller (mkV3 2 0 0) return
    b4 <- makeBaller (mkV3 3 0 0) return
    b5 <- makeBaller (mkV3 2 0 0.6) return
    b6 <- makeBaller (mkV3 2 0 (-0.6)) return

    -- TODO(sandy): we need to draw these capsules
    manageCapsules $ fmap managed <$> [b1, b2, b3, b4, b5, b6]

    return $ do
        cam' <- sample cam
        ballers <- sample $ sequenceA [b1, b2, b3, b4, b5, b6]

        return $ group $ [ drawCourt court cam'
                         , drawBasket cam' unitX
                         , drawBasket cam' (-unitX)
                         ]
                       ++ fmap (drawBaller cam') ballers


main :: IO ()
main = play (EngineConfig (700, 400) "hello") magic return


