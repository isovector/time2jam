{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Baller where

import Art
import Basket
import Camera
import Capsule
import Constants
import Control.Monad.Writer
import Data.Bool (bool)
import Data.Default (def)
import Data.Maybe (isJust)
import Game.Sequoia.Color
import JamPrelude
import Motion

ballerCapsule :: Capsule
ballerCapsule = Capsule
  { _capPos      = V3 0 0 0
  , _capRadius   = 0.75
  , _capHeight   = standardBallerHeight
  , _capEthereal = False
  , _capMotion   = Nothing
  }

defaultBaller :: Schema -> Baller
defaultBaller schema = Baller
  { _bCap    = ballerCapsule
  , _bColor  = red
  , _bStats  = def
  , _bFwd    = RNet
  , _bDir    = V3 0 0 0
  , _bFacing = RNet
  , _bState  = BSDefault
  , _bArt    = Art schema "baller" "Idle" 0 1500
  }

otherBaller :: Schema -> Baller
otherBaller schema = defaultBaller schema
                   & bColor .~ rgb 0.47 0 0.67
                   & bFwd .~ LNet
                   & bFacing .~ LNet
                   & bCap.capHeight .~ 2.5

updateBaller :: Time
             -> Controller
             -> Possession
             -> Baller
             -> Writer [Action] Baller
updateBaller dt ctrl p b@Baller{..} = do
  tell actions
  newCap <- cap'
  let posDelta = _capPos newCap - _capPos _bCap
  return $
    b { _bCap = clampToGround newCap
      , _bDir = velocity
      , _bState = state'
      , _bFacing = facing $ view _x posDelta
      } & bArt.aAnim .~ animName posDelta
  where
    speed =
      let baseSpeed = _bStats ^. sSpeed
          speedMult = _bStats ^. sTurboMult
      in case (_bState, _ctrlTurbo ctrl) of
           (BSDefault, True)  -> baseSpeed * speedMult
           (BSDefault, False) -> baseSpeed
           (_, _)             -> 0

    dx = (*^) speed $ _ctrlDir ctrl
    velocity  = V3 (view _x dx) 0 (view _y dx)

    animName (V3 0 0 0) = "Idle"
    animName (V3 _ 0 _) | p == Has  = "DribbleRun"
                        | otherwise = "Run"
    animName _ = "Idle"


    facing x | x < 0     = LNet
             | x > 0     = RNet
             | otherwise = _bFacing

    hasMotion = isJust $ view capMotion _bCap
    jumpAction = bool (jump 1.5 velocity)
                      (dunk _bFwd)
                      shouldDunk
    shouldDunk = (&& _ctrlTurbo ctrl)
               . (> 0)
               $ dot velocity (netPos _bFwd - _capPos _bCap)

    clampToGround = bool (capPos._y .~ 0) id hasMotion

    cap' = updateCapsule dt
         . motion'
         $ moveCapsule (dt *^ velocity) _bCap

    kp = _ctrlAction ctrl
    canJump = kp == Just JumpKP
           && isn't _BSJumping _bState
           && not hasMotion

    canShoot = kp == Just ShootKP
            && has _BSJumping _bState
            && p == Has

    canShove = kp == Just PassKP
            && has _BSDefault _bState
            && p == Doesnt

    canPass = kp == Just PassKP
           && p == Has

    state' =
      case (_bState, p, hasMotion, canJump) of
        (BSJumping Has,    Has,    False, _)     -> BSGrounded
        (BSJumping Doesnt, Has,    _,     _)     -> BSJumping Doesnt
        (BSJumping _,      Doesnt, False, _)     -> BSDefault
        (BSGrounded,       Doesnt, False, _)     -> BSDefault
        (bs,               _,      True,  _)     -> bs
        (_,                pos,    False, True)  -> BSJumping pos
        (BSGrounded,       Has,    False, False) -> BSGrounded
        (BSDefault,        _,      False, False) -> BSDefault

    motion' = bool id jumpAction canJump
    actions = [Shoot $ shoot _bFwd
              | canShoot]
           ++ [Shove $ ShoveData (b ^. bCap.capPos) velocity 1 2
              | canShove]
           ++ [Pass
              | canPass]

jump :: Double -> V3 -> Capsule -> Capsule
jump jumpHeight velocity c@Capsule{..} =
  moveTo 1 [ _capPos
             + 2 * jumpHeight *^ unitY
             + 0.5 *^ velocity
           , _capPos + velocity
           ] c

shoot :: Net -> Capsule -> Motion
shoot net Capsule {..} = motion $ do
    let dist = norm $ (_capPos & _y .~ 0) - basketGroundPos net
    a <- velBezier ballVelocity
            [ jumpCtrlPt
            , netCtrlPt
            , netPos'
            ] _capPos
    lift $ tell [Point (otherNet net) . bool 2 3 $ dist >= courtLongRadius]
    b <- runBezier 0.2 [ netPos' & _y .~ 0 ] a
    lift $ tell [TurnOver net]
    return b
  where
    ballVelocity = 15
    -- TODO(sandy): make this less copy-paste
    dunkHeight = 3
    dunkCtrl = dunkHeight *^ unitY
    netDir = let (x, _, z) = unpackV3 $ netPos' - _capPos
              in signorm $ V3 x 0.0000001 z
    jumpCtrlPt = sum
               [ view _y netPos' *^ unitY
               , netDir
               , dunkCtrl
               , _capPos
               ]
    netCtrlPt = -netDir + dunkCtrl + netPos'
    netPos' = netPos net

dunk :: Net -> Capsule -> Capsule
dunk net c@Capsule{..} = setMotion c . motion $ do
    runBezier 0.7 [ jumpCtrlPt
                  , netCtrlPt
                  , netPos'
                  ] _capPos
     >>= runBezier 0.3 [ netPos' & _y .~ 0 ]
  where
    dunkHeight = 3
    dunkCtrl = (*^) dunkHeight unitY
    netDir = let (x, _, z) = unpackV3 $ netPos' - _capPos
              in signorm $ V3 x 0 z
    jumpCtrlPt = sum
               [ view _y netPos' *^ unitY
               , netDir
               , dunkCtrl
               , _capPos
               ]
    netCtrlPt = 2 *^ netDir + dunkCtrl + netPos'
    netPos' = netPos net



drawBaller :: Camera -> Time -> Baller -> Form
drawBaller cam now b@Baller{..} =
  group [ move shadowPos
          . traced' black
          $ oval (shadowWidth * shadowSize)
                 (shadowHeight * shadowSize)
        , move pos2d
          . group
          . return
          . scaleXY 1 (ballerHeightMult b)
          . scale size
          . scale 0.3
          . flipped
          $ drawArt _bArt (Just _bColor) now
        ]
  where
    pos = b ^. bCap.capPos
    (pos2d, size) = toScaledScreen cam pos
    (shadowPos, shadowSize) = toScaledScreen cam $ pos & _y .~ 0
    shadowWidth = 50
    shadowHeight = 20
    flipped = case _bFacing of
                RNet -> id
                LNet -> flipX

doShove :: [Shove] -> [GObject] -> [GObject]
doShove shoves objs = do
  obj <- objs
  let hits = join . forM shoves $ \shove -> do
        let (pos, dir, dist, force) = view _ShoveData shove
            dpos = obj ^. objCap.capPos - pos
        guard $ dot dpos dir > 0.75
        guard $ norm dpos <= dist
        return $ force *^ dpos

      forces = sum hits

  case (forces, obj) of
    (0, _) -> return obj

    (_, BallerObj _ _) ->
      return $ flip (over objCap) obj
             $ \c -> setMotion c . motion $ do
               let pos = c ^. capPos
               runBezier 0.15 [pos + forces] pos

    (_, BallObj _) ->
      return $ flip (over objCap) obj
            $ \c -> setMotion c . motion $ do
              let pos = c ^. capPos
              runBezier 0.15 [pos + forces] pos

ballerHeightMult :: Baller -> Double
ballerHeightMult b = view (bCap.capHeight) b / standardBallerHeight

ballerBallHeight :: Baller -> V3
ballerBallHeight b = view (bCap.capHeight) b / 2 *^ unitY

