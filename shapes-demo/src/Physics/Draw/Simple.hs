{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.Draw.Simple where

import Physics.Draw.Canonical

import Control.Lens ((^.), sequenceOf_)
import Data.Array (elems)
import Data.Either.Combinators

import Linear.Matrix
import qualified SDL.Video.Renderer as R
import Physics.Draw

import Physics.Contact.Simple.ConvexHull
import Physics.World.Simple.Object (WorldObj(..))
import qualified Physics.Contact.Simple.SAT as S
import qualified Physics.Contact.Simple as S
import Physics.World.Simple (World(..), worldObjs)
import Utils.Utils

instance ToCanonical (S.ContactPoints Double) where
  type Canonical (S.ContactPoints Double) = ContactPoints
  toCanonical =
    mapBoth _neighborhoodCenter (pairMap _neighborhoodCenter)

instance ToCanonical (S.Contact Double) where
  type Canonical (S.Contact Double) = Contact
  toCanonical S.Contact{..} =
    Contact
    (toCanonical _contactPenetrator)
    (_neighborhoodUnitNormal _contactEdge)

instance ToCanonical (S.Contact' Double) where
  type Canonical (S.Contact' Double) = Contact
  toCanonical S.Contact'{..} =
    Contact
    (Left $ _neighborhoodCenter _contactPenetrator')
    (_neighborhoodUnitNormal _contactEdge')

instance ToCanonical (S.Overlap Double) where
  type Canonical (S.Overlap Double) = Overlap
  toCanonical S.Overlap{..} =
    Overlap (e0, e1) depth pen
    where e0 = _neighborhoodCenter _overlapEdge
          e1 = _neighborhoodCenter . _neighborhoodNext $ _overlapEdge
          n = _neighborhoodUnitNormal _overlapEdge
          depth = fmap (*(-_overlapDepth)) n
          pen = _neighborhoodCenter _overlapPenetrator

instance ToCanonical (ConvexHull Double) where
  type Canonical (ConvexHull Double) = Polygon
  toCanonical ConvexHull{..} = elems _hullVertices

drawObj :: R.Renderer -> M33 Double -> ConvexHull Double -> IO ()
drawObj r viewtrans hull =
  drawConvexHull r (transform viewtrans . toCanonical $ hull)

drawWorld :: R.Renderer -> M33 Double -> World (WorldObj Double) -> IO ()
drawWorld r vt w = sequenceOf_ traverse (fmap (drawObj r vt . _worldShape) (w ^. worldObjs))
