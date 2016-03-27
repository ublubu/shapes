{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}

module Physics.Engine.Opt where

import GHC.Types (Double(D#))

import Physics.Engine.Class
import Physics.World.OptWorld (World, External, fromList)
import Physics.World.OptObject (WorldObj)
import qualified Physics.World.OptObject as PO
import Physics.Constraint.OptConstraint (PhysicalObj(..), toInvMass2)
import Physics.Contact.OptContact (ContactBehavior(..))
import Physics.Contact.OptConvexHull (ConvexHull, rectangleHull, listToHull)
import Physics.World.OptExternal (constantAccel)
import Physics.Constraint.OptLinear (V2(..), P2(..))

data Engine

pairToV2 :: (Double, Double) -> V2
pairToV2 (D# x, D# y) = V2 x y

instance PhysicsEngine Engine where
  type PEWorld Engine = World
  type PEWorldObj Engine = WorldObj
  type PEExternal' Engine = External WorldObj
  type PEPhysicalObj Engine = PhysicalObj
  type PEContactBehavior Engine = ContactBehavior
  type PENumber Engine = Double
  type PEConvexHull Engine = ConvexHull

  makePhysicalObj _ vel rotvel pos rotpos =
    PhysicalObj (pairToV2 vel) rotvel (pairToV2 pos) rotpos . toInvMass2
  makeWorldObj _ = PO.makeWorldObj
  makeWorld _ = fromList
  makeContactBehavior _ = ContactBehavior
  makeConstantAccel _ = constantAccel . pairToV2
  makeHull _ = listToHull . fmap (P2 . pairToV2)
  makeRectangleHull _ (D# w) (D# h) = rectangleHull w h
