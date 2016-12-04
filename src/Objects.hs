module Objects where

import Capsule
import Types
import Control.Monad (void)

detector :: V3
         -> Double
         -> Double
         -> (Capsule, (Capsule -> Capsule) -> IO ())
detector pos r h = (cap, void . return)
  where
    cap = Capsule
        { _capPos = pos
        , _capRadius = r
        , _capHeight = h
        , _capEphemeral = True
        }

