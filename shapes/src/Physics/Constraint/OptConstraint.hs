{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Physics.Constraint.OptConstraint where

import Control.Lens hiding (transform)
import Linear.V2
import Linear.V3
import Linear.Vector
import Linear.Metric
import Physics.Linear
import Physics.Transform
import Utils.Utils

type InvMass2 a = (a, a) -- (inv mass, inv rotational inertia)
data PhysicalObj a = PhysicalObj { _physObjVel :: !(V2 a)
                                 , _physObjRotVel :: !a
                                 , _physObjPos :: !(V2 a)
                                 , _physObjRotPos :: !a
                                 , _physObjInvMass :: !(InvMass2 a)
                                 } deriving (Show, Eq)

makeLenses ''PhysicalObj

class Physical a p | p -> a where
  physObj :: Functor f => (PhysicalObj a -> f (PhysicalObj a)) -> p -> f p

instance Physical a (PhysicalObj a) where
  physObj = id

_physObjVel3 :: PhysicalObj a -> V3 a
_physObjVel3 po = _physObjVel po `append2` _physObjRotVel po
{-# INLINE _physObjVel3 #-}

physObjVel3 :: Functor f => (V3 a -> f (V3 a)) -> PhysicalObj a -> f (PhysicalObj a)
physObjVel3 f po = fmap g (f (_physObjVel3 po))
  where g v3' = po & physObjVel .~ v & physObjRotVel .~ vr
          where (v, vr) = split3 v3'
{-# INLINE physObjVel3 #-}

-- TODO: between incremental solutions, jacobian is expected to remain constant?
--       otherwise, how to clamp?
data Constraint a = Constraint !(V6 a) !a deriving Show
type Constraint' a p = (p, p) -> Constraint a
type ConstraintResult a = (a, Constraint a)
type PhysObjChanged a = PhysicalObj a -> PhysicalObj a -> Bool

_constrainedVel6 :: (PhysicalObj a, PhysicalObj a) -> V6 a
_constrainedVel6 cp = uncurry join33 (pairMap (view physObjVel3) cp)
{-# INLINE _constrainedVel6 #-}

constrainedVel6 :: (Functor f) => (V6 a -> f (V6 a)) -> (PhysicalObj a, PhysicalObj a) -> f (PhysicalObj a, PhysicalObj a)
constrainedVel6 f cp = fmap g (f (_constrainedVel6 cp))
  where g v6 = pairMap h (split33 v6) `pairAp` cp
        h v3 po = po & physObjVel3 .~ v3
{-# INLINE constrainedVel6 #-}

invMassM2 :: (Num a) => InvMass2 a -> InvMass2 a -> Diag6 a
invMassM2 (ma, ia) (mb, ib) = toDiag6 [ma, ma, ia, mb, mb, ib]
{-# INLINE invMassM2 #-}

toInvMass2 :: (Fractional a, Eq a) => (a, a) -> (a, a)
toInvMass2 = pairMap f
  where f x = if x == 0 then 0
              else 1 / x
{-# INLINE toInvMass2 #-}

isStatic :: (Num a, Eq a) => InvMass2 a -> Bool
isStatic = (== (0, 0))
{-# INLINE isStatic #-}

isStaticLin :: (Num a, Eq a) => InvMass2 a -> Bool
isStaticLin = (0 ==) . fst
{-# INLINE isStaticLin #-}

isStaticRot :: (Num a, Eq a) => InvMass2 a -> Bool
isStaticRot = (0 ==) . snd
{-# INLINE isStaticRot #-}

_constrainedInvMassM2 :: (Fractional a) => (PhysicalObj a, PhysicalObj a) -> Diag6 a
_constrainedInvMassM2 cp = uncurry invMassM2 (pairMap (view physObjInvMass) cp)
{-# INLINE _constrainedInvMassM2 #-}

_physObjTransform :: (Floating a, Ord a) => PhysicalObj a -> WorldTransform a
_physObjTransform obj = toTransform (_physObjPos obj) (_physObjRotPos obj)
{-# INLINE _physObjTransform #-}

velocity2 :: PhysicalObj a -> PhysicalObj a -> V6 a
velocity2 a b = (va `append2` wa) `join33` (vb `append2` wb)
  where va = _physObjVel a
        vb = _physObjVel b
        wa = _physObjRotVel a
        wb = _physObjRotVel b
{-# INLINE velocity2 #-}

lagrangian2 :: (Fractional a, Physical a p) => (p, p) -> Constraint a -> a
lagrangian2 os (Constraint j b) = (-(j `dot` v + b)) / mc
  where v = velocity2 o1 o2
        mc = effMassM2 j o1 o2
        (o1, o2) = _physPair os
{-# INLINE lagrangian2 #-}

effMassM2 :: (Fractional a) => V6 a -> PhysicalObj a -> PhysicalObj a -> a
effMassM2 j a b = (j `mulDiag6` im) `dot` j
  where im = curry _constrainedInvMassM2 a b
{-# INLINE effMassM2 #-}

constraintImpulse2 :: (Num a) => V6 a -> a -> V6 a
constraintImpulse2 j lagr = j ^* lagr
{-# INLINE constraintImpulse2 #-}

-- pc = impulse from constraint forces
updateVelocity2_ :: (Num a) => V6 a -> Diag6 a -> V6 a -> V6 a
updateVelocity2_ v im pc = v + (im `mulDiag6'` pc)
{-# INLINE updateVelocity2_ #-}

applyLagrangian2 :: (Fractional a) => Diag6 a -> V6 a -> a -> (PhysicalObj a, PhysicalObj a) -> (PhysicalObj a, PhysicalObj a)
applyLagrangian2 im j lagr = constrainedVel6 %~ f
  where f v6 = updateVelocity2_ v6 im (constraintImpulse2 j lagr)
{-# INLINE applyLagrangian2 #-}

applyLagrangian2' :: (Fractional a) => a -> Constraint a -> (PhysicalObj a, PhysicalObj a) -> (PhysicalObj a, PhysicalObj a)
applyLagrangian2' lagr (Constraint j _) cp = applyLagrangian2 im j lagr cp
  where im = _constrainedInvMassM2 cp
{-# INLINE applyLagrangian2' #-}

solveConstraint :: (Fractional n, Physical n a) => Constraint n -> (a, a) -> (a, a)
solveConstraint c@(Constraint j _) cp = cp & physPair %~ applyLagrangian2 im j lagr
  where im = _constrainedInvMassM2 cp'
        lagr = lagrangian2 cp' c
        cp' = _physPair cp
{-# INLINE solveConstraint #-}

constraintResult :: (Physical n a, Fractional n) => Constraint' n a -> (a, a) -> ConstraintResult n
constraintResult c' ab = (lagrangian2 ab c, c)
  where c = c' ab
{-# INLINE constraintResult #-}

applyConstraintResult :: (Physical n a, Fractional n) => ConstraintResult n -> (a, a) -> (a, a)
applyConstraintResult (lagr, Constraint j _) ab = overWith physObj f ab
  where im = physPairMap _constrainedInvMassM2 ab
        f = applyLagrangian2 im j lagr
{-# INLINE applyConstraintResult #-}

advanceObj :: (Num a) => PhysicalObj a -> a -> PhysicalObj a
advanceObj obj dt = obj & physObjPos %~ f & physObjRotPos %~ g
  where f pos = (dt *^ (obj ^. physObjVel)) + pos
        g ori = (dt * (obj ^. physObjRotVel)) + ori
{-# INLINE advanceObj #-}

_physPair :: (Physical n a) => (a, a) -> (PhysicalObj n, PhysicalObj n)
_physPair = pairMap (view physObj)
{-# INLINE _physPair #-}

physPair :: (Functor f, Physical a p) => ((PhysicalObj a, PhysicalObj a) -> f (PhysicalObj a, PhysicalObj a)) -> (p, p) -> f (p, p)
physPair f os = fmap g (f $ _physPair os)
  where g xy = pairMap (set physObj) xy `pairAp` os
{-# INLINE physPair #-}

physPairMap :: (Physical n a) => ((PhysicalObj n, PhysicalObj n) -> b) -> (a, a) -> b
physPairMap f pair = f (_physPair pair)
{-# INLINE physPairMap #-}
