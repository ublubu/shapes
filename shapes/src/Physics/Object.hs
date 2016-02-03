{-# LANGUAGE PatternSynonyms, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Physics.Object where

import Control.Lens
import Linear.Epsilon
import Physics.Constraint
import Physics.Contact
import Physics.ConvexHull
import Physics.Geometry

data WorldObj n = WorldObj { _worldPhysObj :: !(PhysicalObj n)
                           , _worldObjMu :: !n
                           , _worldShape :: !(ConvexHull n) }
makeLenses ''WorldObj

instance (Show n) => Show (WorldObj n) where
  show (WorldObj obj _ _) = "WorldObj { " ++ show obj ++ " }"

instance Physical n (WorldObj n) where
  physObj = worldPhysObj

-- TODO: Cache the contactHull between frames
instance Contactable n (WorldObj n) where
  contactMu = _worldObjMu
  contactHull = _worldShape

updateShape :: (Epsilon n, Floating n, Ord n) => WorldObj n -> WorldObj n
updateShape obj = obj & worldShape .~ physicsShape (obj ^. worldPhysObj)

makeWorldObj :: (Epsilon n, Floating n, Ord n) => PhysicalObj n -> n -> WorldObj n
makeWorldObj obj mu = WorldObj obj mu (physicsShape obj)
