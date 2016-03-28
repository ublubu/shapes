{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Physics.Draw.Opt where

import GHC.Types (Double(D#))

import Physics.Draw.Canonical

import Control.Lens ((^.), sequenceOf_)
import Data.Array (elems)
import Data.Either.Combinators

import qualified Linear.V2 as L
import qualified Linear.Matrix as L
import qualified SDL.Video.Renderer as R
import Physics.Draw

import Physics.Contact.OptConvexHull
import Physics.World.OptObject (WorldObj(..))
import qualified Physics.Broadphase.OptAabb as B
import qualified Physics.Contact.OptSAT as O
import qualified Physics.Contact.OptContact as O
import Physics.World.OptWorld (World(..), worldObjs)
import Physics.Constraint.OptLinear
import Physics.Constraint.ConvertLinear
import Utils.Utils

instance ToCanonical V2 where
  type Canonical V2 = V2'
  toCanonical = toLV2

instance ToCanonical P2 where
  type Canonical P2 = P2'
  toCanonical = toLP2

instance ToCanonical O.ContactPoints where
  type Canonical O.ContactPoints = ContactPoints
  toCanonical =
    mapBoth f (fromSP . spMap f)
    where f = toCanonical . _neighborhoodCenter

instance ToCanonical O.Contact where
  type Canonical O.Contact = Contact
  toCanonical O.Contact{..} =
    Contact
    (toCanonical _contactPenetrator)
    (toCanonical . _neighborhoodUnitNormal $ _contactEdge)

instance ToCanonical O.Contact' where
  type Canonical O.Contact' = Contact
  toCanonical O.Contact'{..} =
    Contact
    (Left . toCanonical . _neighborhoodCenter $ _contactPenetrator')
    (toCanonical . _neighborhoodUnitNormal $ _contactEdge')

instance ToCanonical O.Overlap where
  type Canonical O.Overlap = Overlap
  toCanonical O.Overlap{..} =
    Overlap (e0, e1) depth pen
    where e0 = toCanonical $ _neighborhoodCenter _overlapEdge
          e1 = toCanonical . _neighborhoodCenter . _neighborhoodNext $ _overlapEdge
          n = toCanonical $ _neighborhoodUnitNormal _overlapEdge
          depth = fmap (*(-_overlapDepth)) n
          pen = toCanonical $ _neighborhoodCenter _overlapPenetrator

instance ToCanonical ConvexHull where
  type Canonical ConvexHull = Polygon
  toCanonical ConvexHull{..} = toCanonical <$> elems _hullVertices

instance ToCanonical B.Aabb where
  type Canonical B.Aabb = Aabb
  toCanonical (B.Aabb (B.Bounds x0 x1) (B.Bounds y0 y1)) =
    Aabb $ L.V2 (D# x0, D# x1) (D# y0, D# y1)

drawObj :: R.Renderer -> L.M33 Double -> ConvexHull -> IO ()
drawObj r viewtrans hull =
  drawConvexHull r (transform viewtrans . toCanonical $ hull)

drawWorld :: R.Renderer -> L.M33 Double -> World WorldObj -> IO ()
drawWorld r vt w = sequenceOf_ traverse (fmap (drawObj r vt . _worldShape) (w ^. worldObjs))
