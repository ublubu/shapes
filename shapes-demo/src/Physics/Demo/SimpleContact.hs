{-# LANGUAGE TypeFamilies #-}

module Physics.Demo.SimpleContact where

import Control.Lens

import Linear.V2

import Physics.Constraint.Simple
import qualified Physics.Contact.Simple as S
import Physics.Contact.Simple.ConvexHull
import qualified Physics.Contact.Simple.SAT as S
import Physics.Engine.Simple
import Physics.World.Simple.Object

import Physics.Draw.Canonical
import Physics.Draw.Simple()
import Physics.Demo.Contact (ContactDemo(..))

import Utils.Utils

instance ContactDemo SimpleEngine where
  type CDOverlap SimpleEngine = S.Overlap Double
  makeBox _ pos rot (w, h) =
    makeWorldObj phys 0.2 (rectangleHull w h)
    where phys = PhysicalObj (V2 0 0) 0 pos rot (1, 1)
  checkOverlap _ sa sb = S.minOverlap' sa sb ^? S._MinOverlap
  checkContact _ sa sb =
    (mFlipContact, ovlab ^? S._MinOverlap, ovlba ^? S._MinOverlap)
    where (mFlipResult, ovlab, ovlba) = S.contactDebug sa sb
          mFlipContact = fmap toCanonical <$> S.unwrapContactResult mFlipResult
  penetratingEdge _ = pairMap _neighborhoodCenter . S.penetratingEdge
  penetratedEdge _ = pairMap _neighborhoodCenter . S.penetratedEdge
  generateContacts _ a b = (toCanonical . flipExtractUnsafe) <$> S.generateContacts (a, b)
  objHull _ = _worldShape
