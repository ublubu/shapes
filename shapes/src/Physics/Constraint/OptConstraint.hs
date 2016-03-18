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

data InvMass2 = InvMass2 { _imLin:: Double
                         , _imRot :: Double
                         } deriving (Show, Eq)
data PhysicalObj = PhysicalObj { _physObjVel :: !(V2 Double)
                               , _physObjRotVel :: !Double
                               , _physObjPos :: !(V2 Double)
                               , _physObjRotPos :: !Double
                               , _physObjInvMass :: !InvMass2
                               } deriving (Show, Eq)

makeLenses ''PhysicalObj

class Physical p where
  physObj :: Functor f => (PhysicalObj -> f PhysicalObj) -> p -> f p

instance Physical PhysicalObj where
  physObj = id

_physObjVel3 :: PhysicalObj -> V3 Double
_physObjVel3 po = _physObjVel po `append2` _physObjRotVel po

physObjVel3 :: Functor f => (V3 Double -> f (V3 Double)) -> PhysicalObj -> f PhysicalObj
physObjVel3 f po = fmap g (f (_physObjVel3 po))
  where g v3' = po & physObjVel .~ v & physObjRotVel .~ vr
          where (v, vr) = split3 v3'

-- TODO: between incremental solutions, jacobian is expected to remain constant?
--       otherwise, how to clamp?
data Constraint = Constraint !(V6 Double) !Double deriving Show
type Constraint' p = (p, p) -> Constraint
type ConstraintResult = (Double, Constraint)
type PhysObjChanged = PhysicalObj -> PhysicalObj -> Bool

_constrainedVel6 :: (PhysicalObj, PhysicalObj) -> V6 Double
_constrainedVel6 cp = uncurry join33 (pairMap (view physObjVel3) cp)

constrainedVel6 :: (Functor f) => (V6 Double -> f (V6 Double)) -> (PhysicalObj, PhysicalObj) -> f (PhysicalObj, PhysicalObj)
constrainedVel6 f cp = fmap g (f (_constrainedVel6 cp))
  where g v6 = pairMap h (split33 v6) `pairAp` cp
        h v3 po = po & physObjVel3 .~ v3

invMassM2 :: InvMass2 -> InvMass2 -> Diag6 Double
invMassM2 (InvMass2 ma ia) (InvMass2 mb ib) = toDiag6 [ma, ma, ia, mb, mb, ib]

toInvMass2 :: (Double, Double) -> InvMass2
toInvMass2 = uncurry InvMass2 . pairMap f
  where f x = if x == 0 then 0
              else 1 / x

isStatic :: InvMass2 -> Bool
isStatic = (== InvMass2 0 0)

isStaticLin :: InvMass2 -> Bool
isStaticLin = (0 ==) . _imLin

isStaticRot :: InvMass2 -> Bool
isStaticRot = (0 ==) . _imRot

_constrainedInvMassM2 :: (PhysicalObj, PhysicalObj) -> Diag6 Double
_constrainedInvMassM2 cp = uncurry invMassM2 (pairMap (view physObjInvMass) cp)

_physObjTransform :: PhysicalObj -> WorldTransform Double
_physObjTransform obj = toTransform (_physObjPos obj) (_physObjRotPos obj)

velocity2 :: PhysicalObj -> PhysicalObj -> V6 Double
velocity2 a b = (va `append2` wa) `join33` (vb `append2` wb)
  where va = _physObjVel a
        vb = _physObjVel b
        wa = _physObjRotVel a
        wb = _physObjRotVel b

lagrangian2 :: (Physical p) => (p, p) -> Constraint -> Double
lagrangian2 os (Constraint j b) = (-(j `dot` v + b)) / mc
  where v = velocity2 o1 o2
        mc = effMassM2 j o1 o2
        (o1, o2) = _physPair os

effMassM2 :: V6 Double -> PhysicalObj -> PhysicalObj -> Double
effMassM2 j a b = (j `mulDiag6` im) `dot` j
  where im = curry _constrainedInvMassM2 a b

constraintImpulse2 :: V6 Double -> Double -> V6 Double
constraintImpulse2 j lagr = j ^* lagr

-- pc = impulse from constraint forces
updateVelocity2_ :: V6 Double -> Diag6 Double -> V6 Double -> V6 Double
updateVelocity2_ v im pc = v + (im `mulDiag6'` pc)

applyLagrangian2 :: Diag6 Double -> V6 Double -> Double -> (PhysicalObj, PhysicalObj) -> (PhysicalObj, PhysicalObj)
applyLagrangian2 im j lagr = constrainedVel6 %~ f
  where f v6 = updateVelocity2_ v6 im (constraintImpulse2 j lagr)

applyLagrangian2' :: Double -> Constraint -> (PhysicalObj, PhysicalObj) -> (PhysicalObj, PhysicalObj)
applyLagrangian2' lagr (Constraint j _) cp = applyLagrangian2 im j lagr cp
  where im = _constrainedInvMassM2 cp

solveConstraint :: (Physical a) => Constraint -> (a, a) -> (a, a)
solveConstraint c@(Constraint j _) cp = cp & physPair %~ applyLagrangian2 im j lagr
  where im = _constrainedInvMassM2 cp'
        lagr = lagrangian2 cp' c
        cp' = _physPair cp

constraintResult :: (Physical a) => Constraint' a -> (a, a) -> ConstraintResult
constraintResult c' ab = (lagrangian2 ab c, c)
  where c = c' ab

applyConstraintResult :: (Physical a) => ConstraintResult -> (a, a) -> (a, a)
applyConstraintResult (lagr, Constraint j _) ab = overWith physObj f ab
  where im = physPairMap _constrainedInvMassM2 ab
        f = applyLagrangian2 im j lagr

advanceObj :: PhysicalObj -> Double -> PhysicalObj
advanceObj obj dt = obj & physObjPos %~ f & physObjRotPos %~ g
  where f pos = (dt *^ (obj ^. physObjVel)) + pos
        g ori = (dt * (obj ^. physObjRotVel)) + ori

_physPair :: (Physical a) => (a, a) -> (PhysicalObj, PhysicalObj)
_physPair = pairMap (view physObj)

physPair :: (Functor f, Physical p) => ((PhysicalObj, PhysicalObj) -> f (PhysicalObj, PhysicalObj)) -> (p, p) -> f (p, p)
physPair f os = fmap g (f $ _physPair os)
  where g xy = pairMap (set physObj) xy `pairAp` os

physPairMap :: (Physical a) => ((PhysicalObj, PhysicalObj) -> b) -> (a, a) -> b
physPairMap f pair = f (_physPair pair)
