module JamPrelude
  ( over, (.~), (^.), set, view, makeLenses, makePrisms, (%~), (&), has, isn't, preview, review
  , def
  , isJust
  , (*^), (^*)
  , (<>)
  , module Game.Sequoia
  , module Types
  , first, second
  , join, forM, forM_, liftM2
  , liftIO
  , bool
  ) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Default
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Game.Sequoia
import Linear.Metric (distance, norm, signorm, dot)
import Linear.Vector ((*^), (^*))
import Types

