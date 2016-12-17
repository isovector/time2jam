-- From https://github.com/hrldcpr/Bezier.hs/blob/master/Bezier.hs
module Bezier where

import Control.Monad (zipWithM)
import Data.SG.Vector
import Types

type Point = [Float]  -- a multi-dimensional coordinate
type Parametric n a = n -> a  -- a value that varies over time

-- linear interpolation between two numbers, from t=0 to t=1
line1d :: Num n => n -> n -> Parametric n n
line1d a b = \t -> (1 - t)*a + t*b

-- line between two points is linear interpolation on each dimension
line :: Num n => [n] -> [n] -> Parametric n [n]
line p q = zipWithM line1d p q

-- bezier of one point is fixed at that point, and bezier of n points is the
-- line between bezier of first n-1 points and bezier of last n-1 points
bezier' :: Num n => [[n]] -> Parametric n [n]
bezier' [p] = return p
bezier' ps  = do p <- bezier' (init ps)
                 q <- bezier' (tail ps)
                 line p q

bezier :: [V3] -> Double -> V3
bezier = (fromComponents .) . bezier' . fmap getComponents
