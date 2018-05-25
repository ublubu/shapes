{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

{- |
Simple data structure that can act as a physical world to simulate.
I will likely implement more interesting world data structures in the future.
-}
module Physics.World where

import           GHC.Generics         (Generic)

import           Control.DeepSeq
import           Control.Lens
import qualified Data.IntMap.Strict   as IM

import           Physics.Constraint
import           Physics.World.Object
import           Utils.Utils

-- | An 'External' is a non-constraint effect (e.g. gravity) on physical objects.
type External = Double -> PhysicalObj -> PhysicalObj

-- | A simple 'PhysicsWorld' implementation using 'IM.IntMap'
data World usr = World
  { _worldObjs    :: !(IM.IntMap (WorldObj usr)) -- ^ Inhabitants by unique 'Int' key
  , _worldNextKey :: !Int -- ^ Key to use for the next new inhabitant
  } deriving (Show, Generic, NFData)
makeLenses ''World

-- | A 'World' without any inhabitants.
emptyWorld :: World usr
emptyWorld = World IM.empty 0

-- | Add a new inhabitant to the 'World'
addObj :: World usr -> WorldObj usr -> World usr
addObj w = snd . addObj' w

-- | Add a new inhabitant to the 'World'. Also, get inhabitant's 'Int' key.
addObj' :: World usr -> WorldObj usr -> (Int, World usr)
addObj' w o = (n, w & worldObjs %~ IM.insert n o & worldNextKey .~ n + 1)
  where n = w ^. worldNextKey

-- | Create a 'World' from a list of inhabitants
fromList :: [WorldObj usr] -- ^ Population for the new 'World'
         -> World usr
fromList = foldl addObj emptyWorld

-- | Keys of all the world's inhabitants
wKeys :: World usr -> [Int]
wKeys = IM.keys . _worldObjs

-- | 'Traversal' of inhabitants with a given key
wObj :: Int -> Traversal' (World usr) (WorldObj usr)
wObj k = worldObjs . ix k

-- | 'Traversal'' of pairs of inhabitants with a given pair of keys
wPair :: (Int, Int) -> Traversal' (World usr) (WorldObj usr, WorldObj usr)
wPair k = worldObjs . pairix k

-- | 'IndexedTraversal'' of all inhabitants
wObjs :: IndexedTraversal' (Int) (World usr) (WorldObj usr)
wObjs = worldObjs . itraversed

-- | Apply 'External' effects to the objects in a world.
--
-- This happens each frame before constraints are created and solved.
wApplyExternals :: [External] -> Double -> World usr -> World usr
wApplyExternals exts dt w = foldl f w exts
  where f w0 ext = w0 & wObjs.woPhys %~ ext dt

-- | Advance the physical state of the world by a given time delta
-- using each inhabitant's current velocity.
wAdvance :: Double -- ^ Time delta
         -> World usr
         -> World usr
wAdvance dt w = w & wObjs.woPhys %~ (`advanceObj` dt)
