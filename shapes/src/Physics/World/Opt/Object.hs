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
           , _worldObjMu :: !Double
           , _worldShape :: !ConvexHull
           } deriving (Generic, NFData)
makeLenses ''WorldObj

instance Show WorldObj where
  show (WorldObj obj _ _) = "WorldObj { " ++ show obj ++ " }"

instance Physical WorldObj where
  physObj = worldPhysObj

instance Contactable WorldObj where
  contactMu = _worldObjMu
  contactHull = _worldShape

updateShape :: WorldObj -> WorldObj
updateShape obj =
  obj & worldShape %~ flip setHullTransform (transform t)
  where t = _physObjTransform . _worldPhysObj $ obj

makeWorldObj :: PhysicalObj -> Double -> ConvexHull -> WorldObj
makeWorldObj p u s = updateShape $ WorldObj p u s
