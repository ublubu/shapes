{-# LANGUAGE PatternSynonyms, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Physics.Object where

import Control.Lens
import Linear.Epsilon
import Physics.Constraint
import Physics.Contact
import Physics.ConvexHull
import Physics.Geometry
import Physics.Transform

data WorldObj n = WorldObj { _worldPhysObj :: !(PhysicalObj n)
                           , _worldObjMu :: !n
                           , _worldShape :: !(ConvexHull n) }
makeLenses ''WorldObj

instance (Show n) => Show (WorldObj n) where
  show (WorldObj obj _ _) = "WorldObj { " ++ show obj ++ " }"

instance Physical n (WorldObj n) where
  physObj = worldPhysObj

instance (Floating n, Ord n) => Contactable n (WorldObj n) where
  contactMu = _worldObjMu
  contactHull obj =
    LocalT (_physObjTransform . _worldPhysObj $ obj) (_worldShape obj)
