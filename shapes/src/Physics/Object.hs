{-# LANGUAGE PatternSynonyms, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Physics.Object where

import Control.Lens ((&), (%~), makeLenses)
import Linear.Epsilon
import Physics.Constraint
import Physics.Contact
import Physics.ConvexHull
import Physics.Transform

data WorldObj n = WorldObj { _worldPhysObj :: !(PhysicalObj n)
                           , _worldObjMu :: !n
                           , _worldShape :: !(ConvexHull n)
                           } deriving (Eq)
makeLenses ''WorldObj

instance (Show n) => Show (WorldObj n) where
  show (WorldObj obj _ _) = "WorldObj { " ++ show obj ++ " }"

instance Physical n (WorldObj n) where
  physObj = worldPhysObj

instance (Floating n, Ord n) => Contactable n (WorldObj n) where
  contactMu = _worldObjMu
  contactHull = _worldShape

updateShape :: (Epsilon n, Floating n, Ord n) => WorldObj n -> WorldObj n
updateShape obj =
  obj & worldShape %~ flip setHullTransform (transform t)
  where t = _physObjTransform . _worldPhysObj $ obj

makeWorldObj :: (Epsilon n, Floating n, Ord n) => PhysicalObj n -> n -> ConvexHull n -> WorldObj n
makeWorldObj p u s = updateShape $ WorldObj p u s
