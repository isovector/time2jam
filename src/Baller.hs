{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Baller where

import AnimBank
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

defaultBaller :: Baller
defaultBaller = Baller
  { _bCap    = ballerCapsule
  , _bColor  = red
  , _bStats  = def
  , _bFwd    = RNet
  , _bDir    = V3 0 0 0
  , _bFacing = RNet
  , _bState  = BSDefault
  , _bArt    = Art __bIdle 0
  }

otherBaller :: Baller
otherBaller = defaultBaller
            & bColor         .~ rgb 0.47 0 0.67
            & bFwd           .~ LNet
            & bFacing        .~ LNet
            & bCap.capHeight .~ 2.2

data BallerGameMode = PlayBaller Controller Possession
                    | TurnOverBaller


facing :: Net -> Double -> Net
facing f x | x < 0     = LNet
           | x > 0     = RNet
           | otherwise = f

newAnim :: Time -> CannedAnim -> Art -> Art
newAnim now name' art =
  let name = art ^. aCanned.aAnim
   in case name == view aAnim name' of
        True -> art
        False -> art & aCanned  .~ name'
                     & aStarted .~ now

updateBaller :: Time
             -> Time
             -> BallerGameMode
             -> Baller
             -> Writer [Action] Baller
updateBaller now dt TurnOverBaller b@Baller{..} = do
  newCap <- updateCapsule dt _bCap
  let posDelta = _capPos newCap - _capPos _bCap
  return $
    b & bCap    .~ (newCap & capPos._y .~ 0)
      & bState  .~ BSDefault
      & bFacing .~ (facing _bFacing $ view _x posDelta)
      & bArt    .~
          ( flip (newAnim now) _bArt
          $ case b ^. bCap.capMotion of
              Just _ -> __bRun
              Nothing -> __bIdle
          )

updateBaller now dt (PlayBaller ctrl p) b@Baller{..} = do
  tell actions
  (newCap, art') <- cap'
  let posDelta = _capPos newCap - _capPos _bCap
  return $
    b & bCap    .~ clampToGround newCap
      & bDir    .~ velocity
      & bState  .~ state'
      & bFacing .~ (facing _bFacing $ view _x posDelta)
      & bArt    .~ animName art' posDelta

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

    animName art _ | canShoot               = newAnim now __bShoot art
    animName art _ | hasMotion              = art
    animName art (V3 0 0 0) | p == Has      = newAnim now __bDribble art
                            | p == Opponent = newAnim now __bDefense art
                            | otherwise     = newAnim now __bIdle art
    animName art (V3 _ 0 _) | p == Has      = newAnim now __bDribbleRun art
                            | otherwise     = newAnim now __bRun art
    animName art _                          = art


    hasMotion = isJust $ view capMotion _bCap
    jumpAction = bool (jump 1.5 velocity)
                      (dunk _bFwd)
                      shouldDunk
    shouldDunk = (&& _ctrlTurbo ctrl)
               . (> 0)
               $ dot velocity (netPos _bFwd - _capPos _bCap)

    clampToGround = bool (capPos._y .~ 0) id hasMotion

    cap' = updateCapsuleAndAnim now dt _bArt
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

    actionType = bool None (bool Jump Dunk shouldDunk) canJump

    state' =
      case (_bState, p, hasMotion, actionType) of
        (BSJumping Has,    Has,    False, _)    -> BSGrounded
        (BSJumping Doesnt, Has,    _,     _)    -> BSJumping Doesnt
        (BSJumping _,      _, False, _)    -> BSDefault
        (BSDunking,        _,      False, _)    -> BSDefault
        (bs,               _,      True,  _)    -> bs
        (_,                _,      False, Dunk) -> BSDunking
        (_,                pos,    False, Jump) -> BSJumping pos
        (BSGrounded,       Has,    False, None) -> BSGrounded
        (BSGrounded,       _,      False, _)    -> BSDefault
        (BSDefault,        _,      False, None) -> BSDefault

    motion' = bool id jumpAction canJump
    actions = [Shoot $ shoot _bFwd
              | canShoot]
           ++ [Shove $ ShoveData (b ^. bCap.capPos) velocity 1 2
              | canShove]
           ++ [Pass
              | canPass]

data ActionType = Jump | Dunk | None deriving Eq

jump :: Double -> V3 -> Capsule -> Capsule
jump jumpHeight velocity c@Capsule{..} = setMotion c . motion $ do
  wait 0 _capPos
  emit $ PlayAnimation __bJumpWithBall
  wait 0.2 _capPos
  runBezier 1 [ _capPos
                + 2 * jumpHeight *^ unitY
                + 0.5 *^ velocity
              , _capPos + velocity
              ] _capPos

shoot :: Net -> Capsule -> Motion
shoot net Capsule {..} = motion $ do
    let dist = norm $ (_capPos & _y .~ 0) - basketGroundPos net
    wait 0.15 _capPos
    a <- velBezier ballVelocity
            [ jumpCtrlPt
            , netCtrlPt
            , netPos'
            ] $ _capPos + unitY ^* 2
    emit . Point (otherNet net) . bool 2 3 $ dist >= courtLongRadius
    b <- runBezier 0.2 [ netPos' & _y .~ 0 ] a
    emit $ ChangeGameMode $ TurnOver net False
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
    wait 0.1 _capPos
    emit $ PlayAnimation __bPreDunk2
    runBezier 0.7 [ jumpCtrlPt
                  , netCtrlPt
                  , netPos'
                  ] _capPos
     >>= runBezier 0.3 [ netPos' & _y .~ 0 ]
  where
    dunkHeight = 3
    dunkCtrl = (*^) dunkHeight unitY
    netDir = let (x, _, z) = unpackV3 $ netPos' - _capPos
              in signorm $ V3 x 0.0000001 z
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

