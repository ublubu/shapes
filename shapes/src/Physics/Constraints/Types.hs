{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Types used in generating and solving contact constraints.
-}
module Physics.Constraints.Types where

import           Control.Lens
import           Data.Monoid
import           Data.Vector.Unboxed          (Unbox)
import           Data.Vector.Unboxed.Deriving

import           Physics.Constraint
import           Physics.Contact.Types
import           Physics.Linear

import           Utils.Utils

-- TODO: experiment unbox/monomorphise 'Processed'
{- |
Used by "solution processors", which take a cached solution and a new solution
and decide what the new incremental solution should be.
-}
data Processed a =
  Processed { _processedToCache :: !a -- ^ the old cached solution + the new incremental solution
            , _processedToApply :: !a -- ^ the new incremental solution to apply
            }

{- |
Some 'SolutionProcessor's use contextual information.
e.g. The 'SolutionProcessor' for friction needs to know the coefficient of friction and the normal force.
(Normal force is the solution to the non-penetration constraint.)
-}
type SolutionProcessor a b = a -> b -> b -> Processed b

-- | This isn't a proper constraint but actually part of the "b" term in the nonpenetration constraint.
data RestitutionConstraint = RestitutionConstraint
  { _rcRadiusA :: V2
  , _rcRadiusB :: V2
  , _rcNormal  :: V2
  }

data ContactConstraint = ContactConstraint
  { _ccNonPen      :: Constraint
  , _ccRestitution :: RestitutionConstraint
  , _ccFriction    :: Constraint
  }

data ContactLagrangian = ContactLagrangian
  { _clNonPen   :: Lagrangian
  , _clFriction :: Lagrangian
  }

derivingUnbox
  "RestitutionConstraint"
  [t|RestitutionConstraint -> (V2, V2, V2)|]
  [|\RestitutionConstraint {..} -> (_rcRadiusA, _rcRadiusB, _rcNormal)|]
  [|\(ra, rb, n) ->
      RestitutionConstraint {_rcRadiusA = ra, _rcRadiusB = rb, _rcNormal = n}|]

derivingUnbox
  "ContactConstraint"
  [t|ContactConstraint -> (Constraint, RestitutionConstraint, Constraint)|]
  [|\ContactConstraint {..} -> (_ccNonPen, _ccRestitution, _ccFriction)|]
  [|\(nonPen, restitution, friction) ->
      ContactConstraint
      {_ccNonPen = nonPen, _ccRestitution = restitution, _ccFriction = friction}|]

derivingUnbox
  "ContactLagrangian"
  [t|ContactLagrangian -> (Lagrangian, Lagrangian)|]
  [|\ContactLagrangian {..} -> (_clNonPen, _clFriction)|]
  [|uncurry ContactLagrangian|]

instance Functor Processed where
  fmap f (Processed a b) = Processed (f a) (f b)
  {-# INLINE fmap #-}

instance Applicative Processed where
  pure x = Processed x x
  Processed f g <*> Processed x y = Processed (f x) (g y)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
