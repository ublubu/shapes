{-# LANGUAGE PatternSynonyms, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Physics.Object where

import Control.Lens
import Physics.Constraint
import Physics.ContactSolver

data WorldObj n = WorldObj { _worldPhysObj :: !(PhysicalObj n)
                           , _worldObjMu :: !n } deriving Show
makeLenses ''WorldObj

instance Physical n (WorldObj n) where
  physObj = worldPhysObj

instance Contactable n (WorldObj n) where
  contactMu = _worldObjMu
