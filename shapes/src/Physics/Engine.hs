{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE TypeFamilies   #-}

{- |
Another piece of the sample implementation of a physics engine that uses this library.
-}
module Physics.Engine where

import           Data.Proxy
import           GHC.Types                  (Double (D#))

import           Physics.Constraint         (PhysicalObj (..), toInvMass2)
import           Physics.Contact            (Shape (..))
import           Physics.Contact.Circle     (circleWithRadius)
import           Physics.Contact.ConvexHull (ConvexHull, listToHull,
                                             rectangleHull)
import           Physics.Contact.Types      (ContactBehavior (..))
import           Physics.Engine.Class
import           Physics.Linear             (P2 (..), V2 (..))
import           Physics.World              (World, fromList)
import           Physics.World.External     (constantAccel)
import           Physics.World.Object       (WorldObj)
import qualified Physics.World.Object       as PO

data Engine a

engineP :: Proxy (Engine a)
engineP = Proxy

pairToV2 :: (Double, Double) -> V2
pairToV2 (D# x, D# y) = V2 x y

instance PhysicsEngine (Engine a) where
  type PEWorld (Engine a) = World
  type PEWorldObj (Engine a) = WorldObj
  type PEPhysicalObj (Engine a) = PhysicalObj
  type PEExternalObj (Engine a) = a
  type PEContactBehavior (Engine a) = ContactBehavior
  type PENumber (Engine a) = Double
  type PEShape (Engine a) = Shape
  makePhysicalObj _ vel rotvel pos rotpos =
    PhysicalObj (pairToV2 vel) rotvel (pairToV2 pos) rotpos . toInvMass2
  makeWorldObj _ = PO.makeWorldObj
  makeWorld _ = fromList
  makeContactBehavior _ = ContactBehavior
  makeConstantAccel _ = constantAccel . pairToV2
  makeHull _ = HullShape . listToHull . fmap (P2 . pairToV2)
  makeRectangleHull _ (D# w) (D# h) = HullShape $ rectangleHull w h
  makeCircle _ = CircleShape . circleWithRadius
