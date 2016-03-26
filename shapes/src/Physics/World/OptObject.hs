{-# LANGUAGE TemplateHaskell #-}

module Physics.World.OptObject where

import Control.Lens ((&), (%~), makeLenses)
import Physics.Constraint.OptConstraint
import Physics.Contact.OptContact
import Physics.Contact.OptConvexHull
import Physics.Transform.OptTransform

data WorldObj =
  WorldObj { _worldPhysObj :: !PhysicalObj
           , _worldObjMu :: !Double
           , _worldShape :: !ConvexHull
           }
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
