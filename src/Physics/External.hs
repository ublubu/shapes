module Physics.External where

import Control.Lens
import Linear.V2
import Linear.Vector
import Physics.Constraint
import Physics.World

constantForce :: (Num a, Fractional a) => V2 a -> External a
constantForce f dt o = o & physObjVel %~ f'
  where f' v = v + (f ^* dt) ^/ m
        m = o ^. physObjMass._1

