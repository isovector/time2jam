{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecordWildCards   #-}

module Art where

import Control.Lens
import Data.Aeson (decode, fromJSON, Result (Success))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Scientific (toRealFloat)
import Data.Spriter.Skeleton
import Data.Spriter.Types
import Data.String.Conv (toS)
import Game.Sequoia.Color (black)
import JamPrelude

makeBones :: Entity -> [Form]
makeBones entity = toProp <$> view entityObjInfo entity
  where
    toProp Bone{..} = traced' black
                    $ polygon
                      [ V2 0 (toRealFloat $ _boneHeight / 2)
                      , V2 (toRealFloat _boneWidth) 0
                      , V2 0 (toRealFloat $ (-_boneHeight) / 2)
                      ]

makeSprites :: Schema -> [Form]
makeSprites schema = toProp
                 <$> schema ^. schemaFolder._head.folderFile
  where
    toProp File{..} = sprite $ "art/raw/" <> _fileName

drawArt :: Art
        -> Double  -- ^ Time since start of game.
        -> Form
drawArt Art{..} now =
  let Just entity = _aSchema ^. schemaEntity . at _aEntity
      Just animation = entity ^. entityAnimation . at _aAnim
      frame = fmod (animation ^. animLength)
                   ((now - _aStarted) * _aSpeedMult)
      bones = animate animation frame
      drawBone ResultBone{..} = move (V2 _rbX $ -_rbY)
                              . rotate (-_rbAngle)
                              . group
                              . return
                              . scaleXY _rbScaleX _rbScaleY
   in case bones of
        Just x -> group . fmap (uncurry drawBone)
                        . zip (sortBy (comparing $ (fmap . fmap) _boneObjFile _rbObj)
                                $ filter (not . isBone) x)
                        $ makeSprites _aSchema
        Nothing -> blank

getArt :: Now Schema
getArt = do
    Just json <- liftIO $ decode . toS <$> readFile "art/raw/baller.scon"
    let Success schema = fromJSON json
    return schema

