{-# LANGUAGE MagicHash #-}

module Physics.World.External where

import GHC.Types (Double(D#))

import Control.Lens
import Physics.Constraint
import Physics.Linear
import Physics.World.Class

constantForce :: V2 -> External
constantForce f dt o = o & physObjVel %~ f'
  where f' v = v `plusV2` (f `smulV2'` dt) `smulV2'` im
        {-# INLINE f' #-}
        im = o ^. physObjInvMass.to (\x -> D# (_imLin x))
{-# INLINE constantForce #-}

constantAccel :: V2 -> External
constantAccel a dt o = o & physObjVel %~ f
  where f v = if isStaticLin (o ^. physObjInvMass)
              then v else v `plusV2` (a `smulV2'` dt)
        {-# INLINE f #-}
{-# INLINE constantAccel #-}
