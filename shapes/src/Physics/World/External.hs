{-# LANGUAGE MagicHash #-}

{- |
The physics engine can also apply impulses that aren't related to constraints.
Gravity is an example.
-}
module Physics.World.External where

import GHC.Types (Double(D#))

import Control.Lens
import Physics.Constraint
import Physics.Linear
import Physics.World

constantForce :: V2 -> External
constantForce f dt o = o & physObjVel %~ f'
  where f' v = v `plusV2` (f `smulV2'` dt) `smulV2'` im
        im = o ^. physObjInvMass.to (\x -> D# (_imLin x))

constantAccel :: V2 -> External
constantAccel a dt o = o & physObjVel %~ f
  where f v = if isStaticLin (o ^. physObjInvMass)
              then v else v `plusV2` (a `smulV2'` dt)
