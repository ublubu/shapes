{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Physics.Draw.Opt where

import           GHC.Types                  (Double (D#))

import           Physics.Draw.Canonical

import           Control.Lens               (sequenceOf_, (^.))
import           Control.Monad.ST
import           Data.Array                 (elems)
import           Data.Either.Combinators
import qualified Data.Vector.Mutable        as V
import qualified Linear.Matrix              as L
import qualified Linear.V2                  as L
import qualified Physics.Broadphase.Aabb    as B
import           Physics.Contact            (Shape (..))
import           Physics.Contact.Circle     (Circle (..))
import           Physics.Contact.ConvexHull
import qualified Physics.Contact.SAT        as O
import qualified Physics.Contact.Types      as O
import           Physics.Draw
import           Physics.Linear
import           Physics.Linear.Convert
import           Physics.World              (World (..))
import qualified SDL.Video.Renderer         as R
import qualified Utils.EmptiesVector        as E
import           Utils.Utils

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
    (Left . toCanonical $ _contactPenetrator')
    (toCanonical _contactEdgeNormal')

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

unitCircle :: [P2]
unitCircle = fmap f [0,step..(2 - step)]
  where f = P2 . unitV2 . (*pi)
        step = 0.2

instance ToCanonical Circle where
  type Canonical Circle = Polygon
  toCanonical Circle{..} = toCanonical . t <$> unitCircle
    where t (P2 a) = _circleCenter `pplusV2` (_circleRadius `smulV2` a)

instance ToCanonical B.Aabb where
  type Canonical B.Aabb = Aabb
  toCanonical (B.Aabb (B.Bounds x0 x1) (B.Bounds y0 y1)) =
    Aabb $ L.V2 (D# x0, D# x1) (D# y0, D# y1)

drawObj :: R.Renderer -> L.M33 Double -> Shape -> IO ()
drawObj r viewtrans (HullShape hull) =
  drawPolygon r (transform viewtrans . toCanonical $ hull)
drawObj r viewtrans (CircleShape circle) =
  drawPolygon r (transform viewtrans . toCanonical $ circle)

drawWorld :: R.Renderer -> L.M33 Double -> World RealWorld () -> IO ()
drawWorld r vt World {..} = do
  shapes <- stToIO $ E.foldM f [] _wEmpties
  mapM_ (drawObj r vt) shapes
  where
    f shapes i = do
      shape <- V.read _wShapes i
      return $ shape : shapes
