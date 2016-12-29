{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Input where

import Game.Sequoia
import Game.Sequoia.Keyboard
import Types

keyboardController :: B [Key] -> B RawController
keyboardController keys =
  RawController <$> wasd keys
             <*> isDown keys EKey
             <*> isDown keys FKey
             <*> isDown keys LeftShiftKey

foldController :: RawController -> RawController -> Controller
foldController old new@RawController{..} = Controller _rctrlDir _rctrlTurbo
                                         $ getKP old new

getKP :: RawController -> RawController -> Maybe Keypress
getKP (RawController _ False _ _) (RawController _ True _ _)  = Just JumpKP
getKP (RawController _ True _ _)  (RawController _ False _ _) = Just ShootKP
getKP (RawController _ _ False _) (RawController _ _ True _)  = Just PassKP
getKP _                            _                          = Nothing

