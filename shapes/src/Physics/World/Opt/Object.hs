{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Physics.World.Opt.Object where

import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens ((&), (%~), makeLenses)
import Physics.Constraint.Opt
import Physics.Contact.Opt
import Physics.Contact.Opt.ConvexHull
import Physics.Transform.Opt

data WorldObj =
  WorldObj { _worldPhysObj :: !PhysicalObj
           , _worldShape :: !ConvexHull
           , _worldMu :: !Double
           } deriving (Generic, NFData)
makeLenses ''WorldObj

data WorldPair a = WorldPair (Int, Int) a deriving Show

instance Show WorldObj where
  show (WorldObj obj _ _) = "WorldObj { " ++ show obj ++ " }"

instance Physical WorldObj where
  physObj = worldPhysObj
  {-# INLINE physObj #-}

updateShape :: WorldObj -> WorldObj
updateShape obj =
  obj & worldShape %~ flip setHullTransform (transform t)
  where t = _physObjTransform . _worldPhysObj $ obj
{-# INLINE updateShape #-}

makeWorldObj :: PhysicalObj -> Double -> ConvexHull -> WorldObj
makeWorldObj p u s = updateShape $ WorldObj p s u
{-# INLINE makeWorldObj #-}
