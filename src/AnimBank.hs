{-# LANGUAGE OverloadedStrings #-}

module AnimBank where

import  System.IO.Unsafe
import  JamPrelude
import  Data.Aeson (decode)
import  Data.String.Conv (toS)

schema :: Schema
schema =
  let Just s = decode
             . toS
             . unsafePerformIO  -- doncha worry. it's fine.
             $ readFile "art/raw/baller.scon"
   in s

__ball :: CannedAnim
__ball = CannedAnim schema "ball" "Idle" 1 False

__ballerIdle :: CannedAnim
__ballerIdle = CannedAnim schema "baller" "Idle" 1500 False

__ballerJumpWithBall :: CannedAnim
__ballerJumpWithBall = CannedAnim schema "baller" "JumpWithBall" 1500 False

__ballerDribbleRun :: CannedAnim
__ballerDribbleRun = CannedAnim schema "baller" "DribbleRun" 1500 True

__ballerRun :: CannedAnim
__ballerRun = CannedAnim schema "baller" "Run" 1500 True
