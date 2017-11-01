{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Simple data structure that can act as a physical world to simulate.
I will likely implement more interesting world data structures in the future.
-}
module Physics.World where

import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens
import qualified Data.IntMap.Strict as IM

import Physics.World.Class
import Utils.Utils

-- | A simple 'PhysicsWorld' implementation using 'IM.IntMap'
data World a =
  World { _worldObjs :: !(IM.IntMap a) -- ^ Inhabitants by unique 'Int' key
        , _worldNextKey :: !Int -- ^ Key to use for the next new inhabitant
        } deriving (Show, Generic, NFData)
makeLenses ''World

-- | A 'World' without any inhabitants.
emptyWorld :: World a
emptyWorld = World IM.empty 0
{-# INLINE emptyWorld #-}

-- | Add a new inhabitant to the 'World'
addObj :: World a -> a -> World a
addObj w = snd . addObj' w
{-# INLINE addObj #-}

-- | Add a new inhabitant to the 'World'. Also, get inhabitant's 'Int' key.
addObj' :: World a -> a -> (Int, World a)
addObj' w o = (n, w & worldObjs %~ IM.insert n o & worldNextKey .~ n + 1)
  where n = w ^. worldNextKey
{-# INLINE addObj' #-}

-- | Create a 'World' from a list of inhabitants
fromList :: [a] -- ^ Population for the new 'World'
         -> World a
fromList = foldl addObj emptyWorld
{-# INLINE fromList #-}

instance (Contactable a) => PhysicsWorld Int (World a) a where
  wKeys = IM.keys . _worldObjs
  wObj k = worldObjs . ix k
  wPair k = worldObjs . pairix k
  wObjs = worldObjs . itraversed
