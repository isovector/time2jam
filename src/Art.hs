{-# LANGUAGE RecordWildCards #-}

module Art where

import Control.Lens (_head)
import Data.Aeson (decode, fromJSON, Result (Success))
import Data.Scientific (toRealFloat)
import Data.Spriter.Skeleton
import Data.Spriter.Types
import Data.String.Conv (toS)
import Game.Sequoia.Color (black)
import JamPrelude

makeBones :: Schema -> [Form]
makeBones schema = toProp <$> schema ^. schemaEntity._head.entityObjInfo
  where
    toProp Bone{..} = traced' black
                    $ polygon
                      [ V2 0 (toRealFloat $ _boneHeight / 2)
                      , V2 (toRealFloat _boneWidth) 0
                      , V2 0 (toRealFloat $ (-_boneHeight) / 2)
                      ]

doAnimation :: Schema -> Int -> Form
doAnimation schema frame =
  let bones = animate (head $ schema ^. schemaEntity._head.entityAnimation)
                      frame
      drawBone ResultBone{..} = move (V2 _rbX $ -_rbY)
                              . rotate (-_rbAngle)
   in case bones of
        Just x -> group . fmap (uncurry drawBone)
                        $ zip x (makeBones schema)
        Nothing -> blank

getArt :: Now Schema
getArt = do
    Just json <- liftIO $ decode . toS <$> readFile "art/skinned.scon"
    let Success schema = fromJSON json
    return schema

