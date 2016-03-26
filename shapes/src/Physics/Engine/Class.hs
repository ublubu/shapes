{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Physics.Engine.Class where

import Data.Proxy

class (Fractional (PENumber e)) => PhysicsEngine e where
  type PEWorld e :: * -> *
  type PEWorldObj e :: *
  type PEExternal e :: * -> *
  type PEPhysicalObj e :: *
  type PEContactBehavior e :: *
  type PENumber e :: *
  type PEConvexHull e :: *

  -- vel, rotvel, pos, rotpos, invmass
  makePhysicalObj :: Proxy e
                  -> (PENumber e, PENumber e)
                  -> PENumber e
                  -> (PENumber e, PENumber e)
                  -> PENumber e
                  -> (PENumber e, PENumber e)
                  -> PEPhysicalObj e
  makeWorldObj :: Proxy e
               -> PEPhysicalObj e
               -> PENumber e
               -> PEConvexHull e
               -> PEWorldObj e
  makeRectangleHull :: Proxy e -> PENumber e -> PENumber e -> PEConvexHull e
  makeWorld :: Proxy e -> [PEWorldObj e] -> PEWorld' e
  makeContactBehavior :: Proxy e -> PENumber e -> PENumber e -> PEContactBehavior e
  makeConstantAccel :: Proxy e -> (PENumber e, PENumber e) -> PEExternal' e
  makeHull :: Proxy e -> [(PENumber e, PENumber e)] -> PEConvexHull e

type PEExternal' e = PEExternal e (PEWorldObj e)
type PEWorld' e = PEWorld e (PEWorldObj e)
