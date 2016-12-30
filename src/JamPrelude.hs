module JamPrelude
  ( over, (.~), (^.), set, view, makeLenses, makePrisms, (%~), (&), has, isn't, preview, review
  , def
  , isJust
  , (*^), (^*)
  , module Game.Sequoia
  , first, second
  , join, forM, forM_, liftM2
  , liftIO
  , bool
  ) where

import Control.Arrow
import Control.Lens
import Linear.Metric (distance, norm, signorm, dot)
import Control.Monad
import Data.Default
import Data.Maybe (isJust)
import Linear.Vector ((*^), (^*))
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Game.Sequoia

