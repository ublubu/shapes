{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}

module Physics.Demo.OptContact where

import Control.Lens

import qualified Linear.V2 as L

import Physics.Linear.Convert
import qualified Physics.Contact as S
import Physics.Contact.ConvexHull
import qualified Physics.Contact.SAT as S
import Physics.Engine.Class
import Physics.Engine
import Physics.World.Class
import qualified Physics.World.Object as S

import Physics.Draw.Canonical
import Physics.Draw.Opt()
import Physics.Demo.Contact (ContactDemo(..))

import Utils.Descending
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
  generateContacts _ a b =
    _descList $ toCanonical . flipExtractUnsafe . snd <$> S.generateContacts (a ^. woShape, b ^. woShape)
  objHull _ = S._worldShape
