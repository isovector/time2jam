{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Art where

import Control.Lens
import Data.Maybe (fromJust)
import Data.Spriter.Skeleton
import Data.Spriter.Types
import Game.Sequoia.Graphics (Element (ImageElement))
import JamPrelude
import Data.Vector (Vector, (!), fromList)

makeSprites :: Schema -> Maybe Color -> Vector Form
makeSprites schema correction = fromList $ toProp
                 <$> schema ^. schemaFolder._head.folderFile
  where
    toProp File{..} = toForm . ImageElement Nothing correction
                             $ "art/raw/" <> _fileName

drawArt :: Art
        -> Maybe Color
        -> Double  -- ^ Time since start of game.
        -> Form
drawArt Art{ _aCanned = CannedAnim{..}
           , _aStarted
           } correction now =
  let Just entity = _aSchema ^. schemaEntity . at _aEntity
      Just animation = entity ^. entityAnimation . at _aAnim
      thisFrame = (now - _aStarted) * _aSpeedMult
      totalLength = animation ^. animLength
      frame = case _aRepeat || thisFrame <= totalLength of
                True -> fmod totalLength thisFrame
                False -> totalLength - 1
      sprites = makeSprites _aSchema correction
      drawBone ResultBone{..} = move (V2 _rbX $ -_rbY)
                              . rotate (-_rbAngle)
                              . group
                              . return
                              . scaleXY _rbScaleX _rbScaleY
   in case animate animation frame of
        Just (filter (not . isBone) -> objs) ->
          group $ fmap (\x -> drawBone x $ sprites ! (_boneObjFile . fromJust $ _rbObj x)) objs
        Nothing -> blank

