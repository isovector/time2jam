{-# LANGUAGE LambdaCase #-}

module Input where

import Data.Default
import Game.Sequoia
import Game.Sequoia.Keyboard
import Types

data Controller = Controller
  { _ctrlDir   :: Rel
  , _ctrlShoot :: Bool
  , _ctrlPass  :: Bool
  , _ctrlTurbo :: Bool
  }

instance Default Controller where
  def = Controller (rel 0 0) False False False

keyboardController :: B [Key] -> B Controller
keyboardController keys =
  Controller <$> wasd keys
             <*> isDown keys EKey
             <*> isDown keys FKey
             <*> isDown keys LeftShiftKey

getKP :: Controller -> Controller -> Maybe Keypress
getKP (Controller _ False _ _) (Controller _ True _ _) = Just ShootKP
getKP (Controller _ _ False _) (Controller _ _ True _) = Just PassKP
getKP _                        _                       = Nothing

