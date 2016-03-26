{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module Physics.Engine.Simple where

import Physics.Engine.Class
import Physics.World (World, External, fromList)
import Physics.Object (WorldObj)
import qualified Physics.Object as PO
import Physics.Constraint (PhysicalObj(..))
import Physics.Contact (ContactBehavior(..))
import Physics.ConvexHull (ConvexHull, rectangleHull, listToHull)
import Physics.External (constantAccel)

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

  makePhysicalObj _ vel rotvel pos =
    PhysicalObj (pairToV2 vel) rotvel (pairToV2 pos)
  makeWorldObj _ = PO.makeWorldObj
  makeWorld _ = fromList
  makeContactBehavior _ = ContactBehavior
  makeConstantAccel _ = constantAccel . pairToV2
  makeHull _ = listToHull . fmap (P . pairToV2)
  makeRectangleHull _ = rectangleHull
