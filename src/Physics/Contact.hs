{-# LANGUAGE DataKinds #-}

module Physics.Contact where

import Control.Lens
import Data.Either
import Linear.Affine
import Linear.Epsilon
import Linear.V
import Linear.V2
import Linear.Vector
import Linear.Matrix
import qualified Physics.Geometry as G
import Physics.Linear
import Physics.Constraint
import Physics.Transform
import Utils.Utils

data Contact a = Contact { contactA :: PhysicalObj a
                         , contactB :: PhysicalObj a
                         , contactPoint :: V2 a
                         , contactNormal :: V2 a } deriving Show

--generateContact :: (Epsilon a, Floating a, Ord a) => PhysicalObj a -> PhysicalObj a -> [Contact a]
--generateContact a b = case mc of Nothing -> []
                                 --Just c -> eitherFlip f c a b
  --where sa = physicsShape a
        --sb = physicsShape b
        --mc = G.contact sa sb
        --f c' a' b' = fmap g ps
          --where ps = G.flattenContactPoints cc
                --cc = iExtract c'
                --n = G.contactNormal cc
                --g p = Contact { contactA = a'
                              --, contactB = b'
                              --, contactPoint = view _Point p
                              --, contactNormal = n }

generateContacts :: (Epsilon a, Floating a, Ord a) => ConstrainedPair a -> [Flipping (Contact a)]
generateContacts cp = case mc of Nothing -> []
                                 Just c -> flipInjectF $ flipMap f c cp
  where shapes = pairMap physicsShape cp
        mc = uncurry G.contact shapes
        f c' (a', b') = fmap g ps
          where ps = G.flattenContactPoints cc
                cc = iExtract c'
                n = G.contactNormal cc
                g p = Contact { contactA = a'
                              , contactB = b'
                              , contactPoint = view _Point p
                              , contactNormal = n }

generateConstraints :: (Epsilon a, Floating a, Ord a) => ConstrainedPair a -> [Constraint a]
generateConstraints cp = fmap toConstraint (generateContacts cp)

toConstraint :: (Num a) => Flipping (Contact a) -> Constraint a
toConstraint c = flipExtractWith (id, f) (fmap toConstraint_ c)
  where f (Constraint j b) = Constraint (flip33 j) b

toConstraint_ :: (Num a) => Contact a -> Constraint a
toConstraint_ c = Constraint (jacobian c) 0

jacobian :: (Num a) => Contact a -> V6 a
jacobian (Contact a b p n) = ja `join33` jb
  where ja = (-n) `append2` ((xa - p) `cross22` n)
        jb = n `append2` ((p - xb) `cross22` n)
        xa = _physObjPos a
        xb = _physObjPos b

velocity :: Contact a -> V6 a
velocity (Contact a b _ _) = (va `append2` wa) `join33` (vb `append2` wb)
  where va = _physObjVel a
        vb = _physObjVel b
        wa = _physObjRotVel a
        wb = _physObjRotVel b

