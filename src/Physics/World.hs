{-# LANGUAGE TemplateHaskell #-}
module World where

import Control.Lens
import Data.Sequence
import Data.Sequence.Lens
import Physics.Constraint
import Linear.Vector

data World a = World { _worldObjs :: [PhysicalObj a] } deriving Show
makeLenses ''World;

type External a = PhysicalObj a -> a -> PhysicalObj a

advanceWorld :: (Num a) => World a -> a -> World a
advanceWorld w dt = w & worldObjs %~ fmap (`advanceObj` dt)
