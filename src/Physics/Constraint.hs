{-# LANGUAGE DataKinds, TemplateHaskell #-}

module Physics.Constraint where

import Control.Lens
import Data.Vector
import Data.Maybe
import Linear.Affine
import Linear.Epsilon
import Linear.V
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Linear.Matrix
import Linear.Metric
import Physics.Linear
import Physics.Geometry
import Physics.Transform
import Utils.Utils

type MassInertia2 a = (a, a)
data PhysicalObj a = PhysicalObj { _physObjVel :: V2 a
                                 , _physObjRotVel :: a
                                 , _physObjPos :: V2 a
                                 , _physObjRotPos :: a
                                 , _physObjHull :: ConvexHull a
                                 , _physObjMass :: MassInertia2 a } deriving Show

makeLenses ''PhysicalObj

_physObjVel3 :: PhysicalObj a -> V3 a
_physObjVel3 po = (_physObjVel po) `append2` (_physObjRotVel po)

physObjVel3 :: Functor f => (V3 a -> f (V3 a)) -> PhysicalObj a -> f (PhysicalObj a)
physObjVel3 f po = fmap g (f (_physObjVel3 po))
  where g v3' = po & physObjVel .~ v & physObjRotVel .~ vr
          where (v, vr) = split3 v3'

testObj = PhysicalObj (V2 1 0) 0.5 (V2 0 0) 0 (rectangleHull 2 2) (2, 1)
testPair = (testObj, testObj)

data Constraint a = Constraint (V6 a) a
type Constraint' a = ConstrainedPair a -> Constraint a
type ConstrainedPair a = (PhysicalObj a, PhysicalObj a)
type ConstraintGen a = ConstrainedPair a -> [Constraint' a]
type PhysObjChanged a = PhysicalObj a -> PhysicalObj a -> Bool

_constrainedVel6 :: ConstrainedPair a -> V6 a
_constrainedVel6 cp = uncurry join33 (pairMap (view physObjVel3) cp)

constrainedVel6 :: Functor f => (V6 a -> f (V6 a)) -> ConstrainedPair a -> f (ConstrainedPair a)
constrainedVel6 f cp = fmap g (f (_constrainedVel6 cp))
  where g v6 = pairMap h (split33 v6) `pairAp` cp
        h v3 po = po & physObjVel3 .~ v3

massM2 :: (Num a) => MassInertia2 a -> MassInertia2 a -> M66 a
massM2 (ma, ia) (mb, ib) = listToV [ listToV [ma, 0, 0, 0, 0, 0]
                                  , listToV [0, ma, 0, 0, 0, 0]
                                  , listToV [0, 0, ia, 0, 0, 0]
                                  , listToV [0, 0, 0, mb, 0, 0]
                                  , listToV [0, 0, 0, 0, mb, 0]
                                  , listToV [0, 0, 0, 0, 0, ib] ]

invMassM2 :: (Fractional a) => MassInertia2 a -> MassInertia2 a -> M66 a
invMassM2 (_ma, _ia) (_mb, _ib) = listToV [ listToV [ma, 0, 0, 0, 0, 0]
                                         , listToV [0, ma, 0, 0, 0, 0]
                                         , listToV [0, 0, ia, 0, 0, 0]
                                         , listToV [0, 0, 0, mb, 0, 0]
                                         , listToV [0, 0, 0, 0, mb, 0]
                                         , listToV [0, 0, 0, 0, 0, ib] ]
  where ma = 1 / _ma
        ia = 1 / _ia
        mb = 1 / _mb
        ib = 1 / _ib

_constrainedMassM2 :: (Fractional a) => ConstrainedPair a -> M66 a
_constrainedMassM2 cp = uncurry massM2 (pairMap (view physObjMass) cp)

_constrainedInvMassM2 :: (Fractional a) => ConstrainedPair a -> M66 a
_constrainedInvMassM2 cp = uncurry invMassM2 (pairMap (view physObjMass) cp)

_physObjTransform :: (Floating a, Ord a) => PhysicalObj a -> WorldTransform a
_physObjTransform obj = toTransform (_physObjPos obj) (_physObjRotPos obj)

physicsShape :: (Epsilon a, Floating a, Ord a) => PhysicalObj a -> ShapeInfo a
physicsShape obj = shapeInfo (LocalT (_physObjTransform obj) (_physObjHull obj))

velocity2 :: PhysicalObj a -> PhysicalObj a -> V6 a
velocity2 a b = (va `append2` wa) `join33` (vb `append2` wb)
  where va = _physObjVel a
        vb = _physObjVel b
        wa = _physObjRotVel a
        wb = _physObjRotVel b

lagrangian2 :: (Num a, Fractional a) => ConstrainedPair a -> Constraint a -> a
lagrangian2 (o1, o2)(Constraint j b) = (-(j `dot` v + b)) / mc
  where v = velocity2 o1 o2
        mc = effMassM2 j o1 o2

effMassM2 :: (Num a, Fractional a) => V6 a -> PhysicalObj a -> PhysicalObj a -> a
effMassM2 j a b = (j *! im) `dot` j
  where im = invMassM2 (_physObjMass a) (_physObjMass b)

constraintImpulse2 :: (Num a) => V6 a -> a -> V6 a
constraintImpulse2 j lagr = j ^* lagr

-- pc = impulse from constraint forces
updateVelocity2_ :: (Num a) => V6 a -> M66 a -> V6 a -> V6 a
updateVelocity2_ v im pc = v + (im !* pc)

solveConstraint :: (Num a, Fractional a) => Constraint a -> ConstrainedPair a -> ConstrainedPair a
solveConstraint c@(Constraint j b) cp = cp & constrainedVel6 %~ f
  where f v6 = updateVelocity2_ v6 im pc
        im = _constrainedInvMassM2 cp
        pc = constraintImpulse2 j lagr
        lagr = lagrangian2 cp c

advanceObj :: (Num a) => PhysicalObj a -> a -> PhysicalObj a
advanceObj obj dt = obj & physObjPos %~ f & physObjRotPos %~ g
  where f pos = (dt *^ (obj ^. physObjVel)) + pos
        g ori = (dt * (obj ^. physObjRotVel)) + ori
