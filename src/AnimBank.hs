{-# LANGUAGE OverloadedStrings #-}

module AnimBank where

import Data.Aeson (Result (Success), decode, fromJSON)
import Data.String.Conv (toS)
import JamPrelude
import System.IO.Unsafe

schema :: Schema
schema =
  let Just j = decode
             . toS
             . unsafePerformIO  -- doncha worry. it's fine.
             $ readFile "art/raw/baller.scon"
      Success s = fromJSON j
   in s

__ball :: CannedAnim
__ball = CannedAnim schema "ball" "Idle" 1 False

__bIdle :: CannedAnim
__bIdle = CannedAnim schema "baller" "Idle" 1500 False

__bPreDunk :: CannedAnim
__bPreDunk = CannedAnim schema "baller" "PreDunk" 1500 False

__bJumpWithBall :: CannedAnim
__bJumpWithBall = CannedAnim schema "baller" "JumpWithBall" 1500 False

__bDribbleRun :: CannedAnim
__bDribbleRun = CannedAnim schema "baller" "DribbleRun" 1500 True

__bRun :: CannedAnim
__bRun = CannedAnim schema "baller" "Run" 1500 True

