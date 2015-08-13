{-# LANGUAGE DataKinds, TemplateHaskell, FunctionalDependencies, FlexibleInstances #-}

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

type InvMass2 a = (a, a)
data PhysicalObj a = PhysicalObj { _physObjVel :: V2 a
                                 , _physObjRotVel :: a
                                 , _physObjPos :: V2 a
                                 , _physObjRotPos :: a
                                 , _physObjHull :: ConvexHull a
                                 , _physObjInvMass :: InvMass2 a } deriving Show

makeLenses ''PhysicalObj

class Physical p a | p -> a where
  physObj :: Functor f => (PhysicalObj a -> f (PhysicalObj a)) -> p -> f p

instance Physical (PhysicalObj a) a where
  physObj = id

_physObjVel3 :: PhysicalObj a -> V3 a
_physObjVel3 po = (_physObjVel po) `append2` (_physObjRotVel po)

physObjVel3 :: Functor f => (V3 a -> f (V3 a)) -> PhysicalObj a -> f (PhysicalObj a)
physObjVel3 f po = fmap g (f (_physObjVel3 po))
  where g v3' = po & physObjVel .~ v & physObjRotVel .~ vr
          where (v, vr) = split3 v3'

testObj = PhysicalObj (V2 1 0) 0.5 (V2 0 0) 0 (rectangleHull 2 2) (toInvMass2 (2, 1))
testPair = (testObj, testObj)

-- TODO: between incremental solutions, jacobian is expected to remain constant?
--       otherwise, how to clamp?
data Constraint a = Constraint (V6 a) a
type Constraint' a = ConstrainedPair a -> Constraint a
type ConstrainedPair a = (PhysicalObj a, PhysicalObj a)
type ConstraintResult a = (a, Constraint a)
type PhysObjChanged a = PhysicalObj a -> PhysicalObj a -> Bool

_constrainedVel6 :: ConstrainedPair a -> V6 a
_constrainedVel6 cp = uncurry join33 (pairMap (view physObjVel3) cp)

constrainedVel6 :: Functor f => (V6 a -> f (V6 a)) -> ConstrainedPair a -> f (ConstrainedPair a)
constrainedVel6 f cp = fmap g (f (_constrainedVel6 cp))
  where g v6 = pairMap h (split33 v6) `pairAp` cp
        h v3 po = po & physObjVel3 .~ v3

invMassM2 :: (Num a) => InvMass2 a -> InvMass2 a -> M66 a
invMassM2 (ma, ia) (mb, ib) = listToV [ listToV [ma, 0, 0, 0, 0, 0]
                                  , listToV [0, ma, 0, 0, 0, 0]
                                  , listToV [0, 0, ia, 0, 0, 0]
                                  , listToV [0, 0, 0, mb, 0, 0]
                                  , listToV [0, 0, 0, 0, mb, 0]
                                  , listToV [0, 0, 0, 0, 0, ib] ]

toInvMass2 :: (Fractional a, Eq a) => (a, a) -> (a, a)
toInvMass2 = pairMap f
  where f x = if x == 0 then 0
              else 1 / x

isStatic :: (Num a, Eq a) => InvMass2 a -> Bool
isStatic = (== (0, 0))

isStaticLin :: (Num a, Eq a) => InvMass2 a -> Bool
isStaticLin = (0 ==) . fst

isStaticRot :: (Num a, Eq a) => InvMass2 a -> Bool
isStaticRot = (0 ==) . snd

_constrainedInvMassM2 :: (Fractional a) => ConstrainedPair a -> M66 a
_constrainedInvMassM2 cp = uncurry invMassM2 (pairMap (view physObjInvMass) cp)

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

lagrangian2 :: (Fractional a) => ConstrainedPair a -> Constraint a -> a
lagrangian2 (o1, o2)(Constraint j b) = (-(j `dot` v + b)) / mc
  where v = velocity2 o1 o2
        mc = effMassM2 j o1 o2

effMassM2 :: (Fractional a) => V6 a -> PhysicalObj a -> PhysicalObj a -> a
effMassM2 j a b = (j *! im) `dot` j
  where im = curry _constrainedInvMassM2 a b

constraintImpulse2 :: (Num a) => V6 a -> a -> V6 a
constraintImpulse2 j lagr = j ^* lagr

-- pc = impulse from constraint forces
updateVelocity2_ :: (Num a) => V6 a -> M66 a -> V6 a -> V6 a
updateVelocity2_ v im pc = v + (im !* pc)

applyLagrangian2 :: (Fractional a) => M66 a -> V6 a -> a -> ConstrainedPair a -> ConstrainedPair a
applyLagrangian2 im j lagr = constrainedVel6 %~ f
  where f v6 = updateVelocity2_ v6 im (constraintImpulse2 j lagr)

applyLagrangian2' :: (Fractional a) => a -> Constraint a -> ConstrainedPair a -> ConstrainedPair a
applyLagrangian2' lagr (Constraint j _) cp = applyLagrangian2 im j lagr cp
  where im = _constrainedInvMassM2 cp

solveConstraint :: (Fractional a) => Constraint a -> ConstrainedPair a -> ConstrainedPair a
solveConstraint c@(Constraint j _) cp = applyLagrangian2 im j lagr cp
  where im = _constrainedInvMassM2 cp
        lagr = lagrangian2 cp c

solveConstraint' :: (Physical a n, Fractional n) => Constraint n -> (a, a) -> (a, a)
solveConstraint' c = overWith physObj (solveConstraint c)

constraintResult :: (Physical a n, Fractional n) => Constraint' n -> (a, a) -> ConstraintResult n
constraintResult c' ab = (lagrangian2 cp c, c)
  where cp = toCP ab
        c = c' cp

applyConstraintResult :: (Physical a n, Fractional n) => ConstraintResult n -> (a, a) -> (a, a)
applyConstraintResult (lagr, Constraint j _) ab = overWith physObj f ab
  where im = cpMap _constrainedInvMassM2 ab
        f = applyLagrangian2 im j lagr

advanceObj :: (Num a) => PhysicalObj a -> a -> PhysicalObj a
advanceObj obj dt = obj & physObjPos %~ f & physObjRotPos %~ g
  where f pos = (dt *^ (obj ^. physObjVel)) + pos
        g ori = (dt * (obj ^. physObjRotVel)) + ori

toCP :: (Physical a n) => (a, a) -> ConstrainedPair n
toCP = pairMap (view physObj)

cpMap :: (Physical a n) => (ConstrainedPair n -> b) -> (a, a) -> b
cpMap f pair = f (toCP pair)
