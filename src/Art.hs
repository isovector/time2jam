{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Art where

import Control.Lens
import Data.Aeson (decode, fromJSON, Result (Success))
import Data.Maybe (fromJust)
import Data.Spriter.Skeleton
import Data.Spriter.Types
import Data.String.Conv (toS)
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
drawArt Art{..} correction now =
  let Just entity = _aSchema ^. schemaEntity . at _aEntity
      Just animation = entity ^. entityAnimation . at _aAnim
      frame = fmod (animation ^. animLength)
                   ((now - _aStarted) * _aSpeedMult)
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

getArt :: Now Schema
getArt = do
    Just json <- liftIO $ decode . toS <$> readFile "art/raw/baller.scon"
    let Success schema = fromJSON json
    return schema

