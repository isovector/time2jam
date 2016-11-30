module Input where

import Game.Sequoia
import Game.Sequoia.Keyboard

data Action = Jump
            | Shoot
            | Pass
            | Dunk

data Controller = Controller
  { _ctrlDir   :: Rel
  , _ctrlShoot :: Bool
  , _ctrlTurbo :: Bool
  }

keyboardController :: B [Key] -> B Controller
keyboardController keys =
  Controller <$> arrows keys
             <*> isDown keys EKey
             <*> isDown keys LeftShiftKey


