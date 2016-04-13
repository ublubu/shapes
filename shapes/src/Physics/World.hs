{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.World where

import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens
import qualified Data.IntMap.Strict as IM

import Physics.World.Class
import Utils.Utils

data World a =
  World { _worldObjs :: !(IM.IntMap a)
        , _worldNextKey :: !Int } deriving (Show, Generic, NFData)
makeLenses ''World

emptyWorld :: World a
emptyWorld = World IM.empty 0
{-# INLINE emptyWorld #-}

addObj :: World a -> a -> World a
addObj w o = w & worldObjs %~ IM.insert n o & worldNextKey .~ n + 1
  where n = w ^. worldNextKey
{-# INLINE addObj #-}

fromList :: [a] -> World a
fromList = foldl addObj emptyWorld
{-# INLINE fromList #-}

instance (Contactable a) => PhysicsWorld Int (World a) a where
  wKeys = IM.keys . _worldObjs
  wObj k = worldObjs . ix k
  wPair k = worldObjs . pairix k
  wObjs = worldObjs . itraversed
