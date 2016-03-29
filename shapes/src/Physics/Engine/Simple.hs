{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module Physics.Engine.Simple where

import Physics.Engine.Class
import Physics.World.Simple (World, External, fromList)
import Physics.World.Simple.Object (WorldObj)
import qualified Physics.World.Simple.Object as PO
import Physics.Constraint.Simple (PhysicalObj(..), toInvMass2)
import Physics.Contact.Simple (ContactBehavior(..))
import Physics.Contact.Simple.ConvexHull (ConvexHull, rectangleHull, listToHull)
import Physics.World.Simple.External (constantAccel)

import Linear.V2
import Linear.Affine

data SimpleEngine

pairToV2 :: (a, a) -> V2 a
pairToV2 (x, y) = V2 x y

instance PhysicsEngine SimpleEngine where
  type PEWorld SimpleEngine = World
  type PEWorldObj SimpleEngine = WorldObj Double
  type PEExternal' SimpleEngine = External Double (WorldObj Double)
  type PEPhysicalObj SimpleEngine = PhysicalObj Double
  type PEContactBehavior SimpleEngine = ContactBehavior Double
  type PENumber SimpleEngine = Double
  type PEConvexHull SimpleEngine = ConvexHull Double

  makePhysicalObj _ vel rotvel pos rotpos =
    PhysicalObj (pairToV2 vel) rotvel (pairToV2 pos) rotpos . toInvMass2
  makeWorldObj _ = PO.makeWorldObj
  makeWorld _ = fromList
  makeContactBehavior _ = ContactBehavior
  makeConstantAccel _ = constantAccel . pairToV2
  makeHull _ = listToHull . fmap (P . pairToV2)
  makeRectangleHull _ = rectangleHull
