{-# LANGUAGE MagicHash #-}

module Physics.World.OptExternal where

import GHC.Types (Double(D#))

import Control.Lens
import Physics.Constraint.OptConstraint
import Physics.Constraint.OptLinear
import Physics.World.OptWorld

constantForce :: (Physical a) => V2 -> External a
constantForce f dt o = o & physObj.physObjVel %~ f'
  where f' v = v `plusV2` (f `smulV2'` dt) `smulV2'` im
        im = o ^. physObj.physObjInvMass.to (\x -> D# (_imLin x))

constantAccel :: (Physical a) => V2 -> External a
constantAccel a dt o = o & physObj.physObjVel %~ f
  where f v = if isStaticLin (o ^. physObj.physObjInvMass)
              then v else v `plusV2` (a `smulV2'` dt)
