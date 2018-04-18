{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
This module is a holdover from when I had two (slow and less slow) implementations of the physics engine.
I used this class so I could run the same demos on both engines to compare them.
There's a good chance I remove this in the future.
-}
module Physics.Engine.Class where

import Data.Proxy
import Physics.World.Class

class (Fractional (PENumber e)) => PhysicsEngine e where
  type PEWorld e :: * -> *
  type PEWorldObj e :: * -> *
  type PEExternalObj e
  type PEPhysicalObj e
  type PEContactBehavior e
  type PENumber e
  type PEConvexHull e

  -- | Create a @PEPhysicalObj e@.
  makePhysicalObj :: Proxy e
                  -> (PENumber e, PENumber e)
                  -- ^ Velocity
                  -> PENumber e
                  -- ^ Rotational velocity
                  -> (PENumber e, PENumber e)
                  -- ^ Position
                  -> PENumber e
                  -- ^ Rotation
                  -> (PENumber e, PENumber e)
                  -- ^ Linear mass paired with rotational mass
                  -> PEPhysicalObj e

  -- | Create a @PEWorldObj e@
  makeWorldObj :: Proxy e
               -> PEPhysicalObj e
               -- ^ The physical body of this object.
               -> PENumber e
               -- ^ Coefficient of friction μ (mu).
               -> PEConvexHull e
               -- ^ The shape of the object.
               -> PEExternalObj e
               -- ^ Any userland piece of data from outside the simulation.
               -> PEWorldObj e (PEExternalObj e)

  makeWorld :: Proxy e -> [PEWorldObj e (PEExternalObj e)] -> PEWorld' e
  makeContactBehavior :: Proxy e -> PENumber e -> PENumber e -> PEContactBehavior e
  makeConstantAccel :: Proxy e -> (PENumber e, PENumber e) -> External
  makeHull :: Proxy e -> [(PENumber e, PENumber e)] -> PEConvexHull e
  makeRectangleHull :: Proxy e -> PENumber e -> PENumber e -> PEConvexHull e

type PEWorldObj' e = PEWorldObj e (PEExternalObj e)
type PEWorld' e = PEWorld e (PEWorldObj' e)
