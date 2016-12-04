module Objects where

import Capsule
import Types
import Control.Monad (void)

detector :: Name
         -> V3
         -> Double
         -> Double
         -> (Capsule, (Capsule -> Capsule) -> IO ())
detector name pos r h = (cap, void . return)
  where
    cap = Capsule
        { _capName      = name
        , _capPos       = pos
        , _capRadius    = r
        , _capHeight    = h
        , _capEphemeral = True
        }

