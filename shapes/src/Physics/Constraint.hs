{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.Constraint ( module Physics.Constraint
                          , module Physics.Constraint.Types
                          ) where

import GHC.Generics (Generic)
import GHC.Prim (Double#, (==##), (/##))
import GHC.Types (Double(D#), isTrue#)

import Control.DeepSeq
import Control.Lens hiding (transform)
import Data.Vector.Unboxed.Deriving

import Physics.Constraint.Types
import Physics.Linear
import Physics.Transform
import Utils.Utils

data InvMass2 = InvMass2 { _imLin :: Double#
                         , _imRot :: Double#
                         } deriving (Show, Eq)

instance NFData InvMass2 where
  rnf (InvMass2 _ _) = ()
  {-# INLINE rnf #-}

data PhysicalObj = PhysicalObj { _physObjVel :: !V2
                               , _physObjRotVel :: !Double
                               , _physObjPos :: !V2
                               , _physObjRotPos :: !Double
                               , _physObjInvMass :: !InvMass2
                               } deriving (Show, Generic, NFData)
makeLenses ''PhysicalObj

_physObjVel3 :: PhysicalObj -> V3
_physObjVel3 po = _physObjVel po `append2` _physObjRotVel po
{-# INLINE _physObjVel3 #-}

physObjVel3 :: Functor f => (V3 -> f V3) -> PhysicalObj -> f PhysicalObj
physObjVel3 f po = fmap g (f (_physObjVel3 po))
  where g v3' = po & physObjVel .~ v & physObjRotVel .~ vr
          where !(v, vr) = split3 v3'
        {-# INLINE g #-}
{-# INLINE physObjVel3 #-}

toInvMass2 :: (Double, Double) -> InvMass2
toInvMass2 (D# ml, D# mr) = InvMass2 (invert ml) (invert mr)
  where invert m = if isTrue# (m ==## 0.0##) then 0.0## else 1.0## /## m
        {-# INLINE invert #-}
{-# INLINE toInvMass2 #-}

-- j is Jacobian, b is extra term
data Constraint = Constraint { _constraintJ :: !V6
                             , _constraintB :: !Double
                             } deriving Show
type Constraint' p = (p, p) -> Constraint
type PhysObjChanged = PhysicalObj -> PhysicalObj -> Bool

makeLenses ''Lagrangian

derivingUnbox "Lagrangian"
  [t| Lagrangian -> Double |]
  [| \(Lagrangian l) -> l |]
  [| Lagrangian |]

derivingUnbox "Constraint"
  [t| Constraint -> (V6, Double) |]
  [| \Constraint{..} -> (_constraintJ, _constraintB) |]
  [| \(j, b) -> Constraint j b |]

instance Flippable Constraint where
  flipp (Constraint j b) = Constraint (flip3v3 j) b
  {-# INLINE flipp #-}

_constrainedVel6 :: (PhysicalObj, PhysicalObj) -> V6
_constrainedVel6 cp = uncurry join3v3 (pairMap (view physObjVel3) cp)
{-# INLINE _constrainedVel6 #-}

constrainedVel6 :: (Functor f) => (V6 -> f V6) -> (PhysicalObj, PhysicalObj) -> f (PhysicalObj, PhysicalObj)
constrainedVel6 f cp = fmap g (f (_constrainedVel6 cp))
  where g v6 = pairMap h (split3v3 v6) `pairAp` cp
        h v3 po = po & physObjVel3 .~ v3
{-# INLINE constrainedVel6 #-}

invMassM2 :: InvMass2 -> InvMass2 -> Diag6
invMassM2 (InvMass2 ma ia) (InvMass2 mb ib) = Diag6 (V6 ma ma ia mb mb ib)
{-# INLINE invMassM2 #-}

isStatic :: InvMass2 -> Bool
isStatic = (== InvMass2 0.0## 0.0##)
{-# INLINE isStatic #-}

isStaticLin :: InvMass2 -> Bool
isStaticLin x = isTrue# (0.0## ==## _imLin x)
{-# INLINE isStaticLin #-}

isStaticRot :: InvMass2 -> Bool
isStaticRot x = isTrue# (0.0## ==## _imRot x)
{-# INLINE isStaticRot #-}

_constrainedInvMassM2 :: (PhysicalObj, PhysicalObj) -> Diag6
_constrainedInvMassM2 cp = uncurry invMassM2 (pairMap (view physObjInvMass) cp)
{-# INLINE _constrainedInvMassM2 #-}

_physObjTransform :: PhysicalObj -> WorldTransform
_physObjTransform obj = toTransform (_physObjPos obj) rot
  where !(D# rot) = _physObjRotPos obj
{-# INLINE _physObjTransform #-}

velocity2 :: PhysicalObj -> PhysicalObj -> V6
velocity2 a b = (va `append2` wa) `join3v3` (vb `append2` wb)
  where va = _physObjVel a
        vb = _physObjVel b
        wa = _physObjRotVel a
        wb = _physObjRotVel b
{-# INLINE velocity2 #-}

lagrangian2 :: (PhysicalObj, PhysicalObj) -> Constraint -> Lagrangian
lagrangian2 (o1, o2) (Constraint j b) =
  Lagrangian $ (-((D# (j `dotV6` v)) + b)) / mc
  where v = velocity2 o1 o2
        mc = effMassM2 j o1 o2
{-# INLINE lagrangian2 #-}

effMassM2 :: V6 -> PhysicalObj -> PhysicalObj -> Double
effMassM2 j a b = D# ((j `vmulDiag6` im) `dotV6` j)
  where im = curry _constrainedInvMassM2 a b
{-# INLINE effMassM2 #-}

constraintImpulse2 :: V6 -> Lagrangian -> V6
constraintImpulse2 j (Lagrangian l) = l `smulV6` j
{-# INLINE constraintImpulse2 #-}

-- pc = impulse from constraint forces
updateVelocity2_ :: V6 -> Diag6 -> V6 -> V6
updateVelocity2_ v im pc = v `plusV6` (im `vmulDiag6'` pc)
{-# INLINE updateVelocity2_ #-}

applyLagrangian2 :: Diag6
                 -> V6
                 -> Lagrangian
                 -> (PhysicalObj, PhysicalObj)
                 -> (PhysicalObj, PhysicalObj)
applyLagrangian2 im j lagr = constrainedVel6 %~ f
  where f v6 = updateVelocity2_ v6 im (constraintImpulse2 j lagr)
{-# INLINE applyLagrangian2 #-}

solveConstraint :: Constraint
                -> (PhysicalObj, PhysicalObj)
                -> (PhysicalObj, PhysicalObj)
solveConstraint c ab =
  applyLagrangian (lagrangian2 ab c) c ab
{-# INLINE solveConstraint #-}

applyLagrangian :: Lagrangian
                -> Constraint
                -> (PhysicalObj, PhysicalObj)
                -> (PhysicalObj, PhysicalObj)
applyLagrangian lagr (Constraint j _) ab =
  applyLagrangian2 (_constrainedInvMassM2 ab) j lagr ab
{-# INLINE applyLagrangian #-}

advanceObj :: PhysicalObj -> Double -> PhysicalObj
advanceObj obj dt = obj & physObjPos %~ f & physObjRotPos %~ g
  where f pos = (dt `smulV2` (obj ^. physObjVel)) `plusV2` pos
        g ori = (dt * (obj ^. physObjRotVel)) + ori
{-# INLINE advanceObj #-}
