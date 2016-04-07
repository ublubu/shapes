{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}

module Physics.Engine.Opt where

import GHC.Types (Double(D#))
import Data.Proxy

import Physics.Engine.Class
import Physics.World.Opt (External)
import Physics.World.Opt.Object (WorldObj)
import qualified Physics.World.Opt.Object as PO
import Physics.Constraint.Opt (PhysicalObj(..), toInvMass2)
import Physics.Contact.Opt (ContactBehavior(..))
import Physics.Contact.Opt.ConvexHull (ConvexHull, rectangleHull, listToHull)
import Physics.World.Opt.External (constantAccel)
import Physics.Linear.Opt (V2(..), P2(..))

data Engine

engineP :: Proxy Engine
engineP = Proxy

pairToV2 :: (Double, Double) -> V2
pairToV2 (D# x, D# y) = V2 x y

instance PhysicsEngine Engine where
  type PEWorld Engine = [WorldObj]
  type PEWorldObj Engine = WorldObj
  type PEExternal' Engine = External PhysicalObj
  type PEPhysicalObj Engine = PhysicalObj
  type PEContactBehavior Engine = ContactBehavior
  type PENumber Engine = Double
  type PEConvexHull Engine = ConvexHull

  makePhysicalObj _ vel rotvel pos rotpos =
    PhysicalObj (pairToV2 vel) rotvel (pairToV2 pos) rotpos . toInvMass2
  makeWorldObj _ = PO.makeWorldObj
  makeWorld _ = id
  makeContactBehavior _ = ContactBehavior
  makeConstantAccel _ = constantAccel . pairToV2
  makeHull _ = listToHull . fmap (P2 . pairToV2)
  makeRectangleHull _ (D# w) (D# h) = rectangleHull w h
