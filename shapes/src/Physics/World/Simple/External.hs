module Physics.World.Simple.External where

import Control.Lens
import Linear.V2
import Linear.Vector
import Physics.Constraint.Simple
import Physics.World.Simple

constantForce :: (Physical n a, Num n, Fractional n) => V2 n -> External n a
constantForce f dt o = o & physObj.physObjVel %~ f'
  where f' v = v + (f ^* dt) ^* im
        im = o ^. physObj.physObjInvMass._1

constantAccel :: (Physical n a, Num n, Fractional n, Eq n) => V2 n -> External n a
constantAccel a dt o = o & physObj.physObjVel %~ f
  where f v = if isStaticLin (o ^. physObj.physObjInvMass) then v else v + (a ^* dt)
