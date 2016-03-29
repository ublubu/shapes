{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}

module Physics.Demo.OptContact where

import Control.Lens

import qualified Linear.V2 as L

import Physics.Linear.Opt.Convert
import qualified Physics.Contact.Opt as S
import Physics.Contact.Opt.ConvexHull
import qualified Physics.Contact.Opt.SAT as S
import Physics.Engine.Class
import Physics.Engine.Opt
import qualified Physics.World.Opt.Object as S

import Physics.Draw.Canonical
import Physics.Draw.Opt()
import Physics.Demo.Contact (ContactDemo(..))

import Utils.Utils

instance ContactDemo Engine where
  type CDOverlap Engine = S.Overlap
  makeBox p (L.V2 x y) rot (w, h) =
    makeWorldObj p phys 0.2 (makeRectangleHull p w h)
    where phys = makePhysicalObj p (0, 0) 0 (x, y) rot (1, 1)
  checkOverlap _ sa sb = S.minOverlap' sa sb ^? S._MinOverlap
  checkContact _ sa sb =
    (mFlipContact, ovlab ^? S._MinOverlap, ovlba ^? S._MinOverlap)
    where (mFlipResult, ovlab, ovlba) = S.contactDebug sa sb
          mFlipContact = fmap toCanonical <$> S.unwrapContactResult mFlipResult
  penetratingEdge _ = fromSP . spMap (toLP2 . _neighborhoodCenter) . S.penetratingEdge
  penetratedEdge _ = fromSP . spMap (toLP2 . _neighborhoodCenter) . S.penetratedEdge
  generateContacts _ a b = (toCanonical . flipExtract) <$> S.generateContacts (a, b)
  objHull _ = S._worldShape
