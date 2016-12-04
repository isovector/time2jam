module Input where

import Control.Monad (liftM2)
import Game.Sequoia
import Game.Sequoia.Keyboard
import Control.FRPNow.EvStream

data Keypress = ShootKP
              | PassKP
              deriving (Show, Eq, Ord)

data Action = Jump
            | Shoot
            | Pass
            | Dunk
            deriving (Show, Eq, Ord)

data Controller = Controller
  { _ctrlDir   :: Rel
  , _ctrlShoot :: Bool
  , _ctrlPass  :: Bool
  , _ctrlTurbo :: Bool
  }

keyboardController :: B [Key] -> B Controller
keyboardController keys =
  Controller <$> wasd keys
             <*> isDown keys EKey
             <*> isDown keys FKey
             <*> isDown keys LeftShiftKey

kpEvents :: B Controller -> B (E Keypress)
kpEvents = next . liftM2 merge shootEvents passEvents
  where
    over x f = (x <$) . edges . fmap f
    shootEvents = over ShootKP _ctrlShoot
    passEvents  = over PassKP  _ctrlPass

