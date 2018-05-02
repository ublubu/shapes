{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Types for describing the motion of physical objects.
Functions for solving constraints.
-}
module Physics.Constraint ( module Physics.Constraint
                          , module Physics.Constraint.Types
                          ) where

import           GHC.Generics                 (Generic)
import           GHC.Prim                     (Double#, (/##), (==##))
import           GHC.Types                    (Double (D#), isTrue#)

import           Control.DeepSeq
import           Control.Lens                 hiding (transform)
import           Data.Vector.Unboxed.Deriving

import           Physics.Constraint.Types
import           Physics.Linear
import           Physics.Transform
import           Utils.Utils

-- | Multiplicative inverse of linear and rotational mass
data InvMass2 = InvMass2 { _imLin :: Double#
                         , _imRot :: Double#
                         } deriving (Show, Eq)

instance NFData InvMass2 where
  rnf (InvMass2 _ _) = ()
  {-# INLINE rnf #-}

derivingUnbox "InvMass2"
  [t| InvMass2 -> (Double, Double) |]
  [| \InvMass2{..} -> (D# _imLin, D# _imRot) |]
  [| \(D# linMass, D# rotMass) -> InvMass2 linMass rotMass |]

-- | The state of motion for a physical body.
-- Rotation is measured in the Z direction (right-handed coordinates).
data PhysicalObj = PhysicalObj { _physObjVel     :: !V2
                               , _physObjRotVel  :: !Double
                               , _physObjPos     :: !V2
                               , _physObjRotPos  :: !Double
                               , _physObjInvMass :: !InvMass2
                               } deriving (Show, Generic, NFData)
makeLenses ''PhysicalObj

derivingUnbox "PhysicalObj"
  [t| PhysicalObj -> (V2, Double, V2, Double, InvMass2) |]
  [| \PhysicalObj{..} -> (_physObjVel, _physObjRotVel, _physObjPos, _physObjRotPos, _physObjInvMass) |]
  [| \(vel, rotvel, pos, rotPos, invMass) -> PhysicalObj vel rotvel pos rotPos invMass |]

_physObjVel3 :: PhysicalObj -> V3
_physObjVel3 po = _physObjVel po `append2` _physObjRotVel po
{-# INLINE _physObjVel3 #-}

-- | Lens for 3D velocity vector: (v_x, v_y, v_rot)
physObjVel3 :: Functor f => (V3 -> f V3) -> PhysicalObj -> f PhysicalObj
physObjVel3 f po = fmap g (f (_physObjVel3 po))
  where g v3' = po & physObjVel .~ v & physObjRotVel .~ vr
          where !(v, vr) = split3 v3'
        {-# INLINE g #-}
{-# INLINE physObjVel3 #-}

-- | Convert (linear mass, rotational inertia) into 'InvMass2'.
-- Use 0 for infinite mass (non-translating/non-rotating objects).
toInvMass2 :: (Double, Double) -> InvMass2
toInvMass2 (D# ml, D# mr) = InvMass2 (invert ml) (invert mr)
  where invert m = if isTrue# (m ==## 0.0##) then 0.0## else 1.0## /## m
        {-# INLINE invert #-}
{-# INLINE toInvMass2 #-}

-- | A constraint equation between two objects
-- to be solved using the objects' state of motion
data Constraint = Constraint { _constraintJ :: !V6 -- ^ Jacobian - coordinate transform to the constraint space
                             , _constraintB :: !Double -- ^ extra term
                             } deriving Show
-- | Generates a constraint equation from a pair of objects
type Constraint' p = (p, p) -> Constraint
-- | Are these two different motion states?
-- Used to determine whether the constraint solver has converged.
type PhysObjChanged = PhysicalObj -> PhysicalObj -> Bool

derivingUnbox "Constraint"
  [t| Constraint -> (V6, Double) |]
  [| \Constraint{..} -> (_constraintJ, _constraintB) |]
  [| uncurry Constraint |]

instance Flippable Constraint where
  flipp (Constraint j b) = Constraint (flip3v3 j) b
  {-# INLINE flipp #-}

-- | Get a 6D velocity vector for a pair of objects.
-- (a_vx, a_vy, a_vr, b_vx, b_vy, b_vr)
--
-- Called \"constrained\" because it's used with objects constrained together.
_constrainedVel6 :: (PhysicalObj, PhysicalObj) -> V6
_constrainedVel6 cp = uncurry join3v3 (pairMap (view physObjVel3) cp)
{-# INLINE _constrainedVel6 #-}

-- | Lens for 6D velocity vector ('_constrainedVel6')
constrainedVel6 :: (Functor f) => (V6 -> f V6) -> (PhysicalObj, PhysicalObj) -> f (PhysicalObj, PhysicalObj)
constrainedVel6 f cp = fmap g (f (_constrainedVel6 cp))
  where g v6 = pairMap h (split3v3 v6) `pairAp` cp
        h v3 po = po & physObjVel3 .~ v3
{-# INLINE constrainedVel6 #-}

-- | 6x6 diagonal matrix of inverse mass
--
-- > invMassM2 (InvMass2 ma ia) (InvMass2 mb ib) = Diag6 (V6 ma ma ia mb mb ib)
invMassM2 :: InvMass2 -> InvMass2 -> Diag6
invMassM2 (InvMass2 ma ia) (InvMass2 mb ib) = Diag6 (V6 ma ma ia mb mb ib)
{-# INLINE invMassM2 #-}

-- | Is this object completely static (unmoving)?
isStatic :: InvMass2 -> Bool
isStatic = (== InvMass2 0.0## 0.0##)
{-# INLINE isStatic #-}

-- | Is this object non-translating (no center-of-mass movement)?
isStaticLin :: InvMass2 -> Bool
isStaticLin x = isTrue# (0.0## ==## _imLin x)
{-# INLINE isStaticLin #-}

-- | Is this object non-rotating?
isStaticRot :: InvMass2 -> Bool
isStaticRot x = isTrue# (0.0## ==## _imRot x)
{-# INLINE isStaticRot #-}

-- | see 'invMassM2'
_constrainedInvMassM2 :: (PhysicalObj, PhysicalObj) -> Diag6
_constrainedInvMassM2 cp = uncurry invMassM2 (pairMap (view physObjInvMass) cp)
{-# INLINE _constrainedInvMassM2 #-}

-- | Get 'WorldTransform' from origin to the current position
-- (translation & rotation) of an object.
_physObjTransform :: PhysicalObj -> WorldTransform
_physObjTransform obj = toTransform (_physObjPos obj) rot
  where !(D# rot) = _physObjRotPos obj
{-# INLINE _physObjTransform #-}

-- TODO: dedupe this & _constrainedVel6
-- | Get a 6D velocity vector for a pair of objects.
-- Same as '_constrainedVel6'
velocity2 :: PhysicalObj -> PhysicalObj -> V6
velocity2 a b = (va `append2` wa) `join3v3` (vb `append2` wb)
  where va = _physObjVel a
        vb = _physObjVel b
        wa = _physObjRotVel a
        wb = _physObjRotVel b
{-# INLINE velocity2 #-}

-- | Use objects' current state of motion to solve their constraint equation.
--
-- The 'Lagrangian' multiplier is the (signed) magnitude
-- of the constraint impulse along the constraint axis.
lagrangian2 :: (PhysicalObj, PhysicalObj) -> Constraint -> Lagrangian
lagrangian2 (o1, o2) (Constraint j b) =
  Lagrangian $ (-(D# (j `dotV6` v) + b)) / mc
  where v = velocity2 o1 o2
        mc = effMassM2 j o1 o2
{-# INLINE lagrangian2 #-}

-- TODO: rename effMassM2 to invEffMass
-- | The inverse effective mass of a pair of objects along the constraint axis
effMassM2 :: V6 -- ^ Jacobian
          -> PhysicalObj
          -> PhysicalObj
          -> Double -- ^ Inverse of effective mass
effMassM2 j a b = D# ((j `vmulDiag6` im) `dotV6` j)
  where im = curry _constrainedInvMassM2 a b
{-# INLINE effMassM2 #-}

-- | Get the impulse that solves a constraint equation.
constraintImpulse2 :: V6 -- ^ Jacobian
                   -> Lagrangian
                   -> V6 -- ^ 6D constraint impulse vector
constraintImpulse2 j (Lagrangian l) = l `smulV6` j
{-# INLINE constraintImpulse2 #-}

-- | Apply a constraint impulse to two objects.
updateVelocity2_ :: V6 -- ^ 6D velocity for two objects
                 -> Diag6 -- ^ Inverse mass for two objects
                 -> V6 -- ^ 6D constraint impulse
                 -> V6 -- ^ New 6D velocity
updateVelocity2_ v im pc = v `plusV6` (im `vmulDiag6'` pc)
{-# INLINE updateVelocity2_ #-}

-- | Use a Lagrangian multiplier to update a pair of objects.
applyLagrangian2 :: Diag6 -- ^ Inverse mass
                 -> V6 -- ^ Jacobian
                 -> Lagrangian
                 -> (PhysicalObj, PhysicalObj)
                 -> (PhysicalObj, PhysicalObj)
applyLagrangian2 im j lagr = constrainedVel6 %~ f
  where f v6 = updateVelocity2_ v6 im (constraintImpulse2 j lagr)
{-# INLINE applyLagrangian2 #-}

-- | Solve a constraint between two objects.
solveConstraint :: Constraint -- ^ Constraint equation
                -> (PhysicalObj, PhysicalObj)
                -> (PhysicalObj, PhysicalObj) -- ^ Updated state of motion
solveConstraint c ab =
  applyLagrangian (lagrangian2 ab c) c ab
{-# INLINE solveConstraint #-}

-- | Use a Lagrangian multiplier to update a pair of objects.
applyLagrangian :: Lagrangian -- ^ Lagrangian multiplier from solving the constraint
                -> Constraint -- ^ The constraint equation
                -> (PhysicalObj, PhysicalObj)
                -> (PhysicalObj, PhysicalObj) -- ^ Updated state of motion
applyLagrangian lagr (Constraint j _) ab =
  applyLagrangian2 (_constrainedInvMassM2 ab) j lagr ab
{-# INLINE applyLagrangian #-}

-- | Advance the position (translation & rotation) of an object by
-- applying its velocity over a time delta.
advanceObj :: PhysicalObj -> Double -> PhysicalObj
advanceObj obj dt = obj & physObjPos %~ f & physObjRotPos %~ g
  where f pos = (dt `smulV2` (obj ^. physObjVel)) `plusV2` pos
        g ori = (dt * (obj ^. physObjRotVel)) + ori
{-# INLINE advanceObj #-}
