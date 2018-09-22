{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Generate and solve all contact constraints for pairs of colliding objects.
-}
module Physics.Constraints.Contact where

import           GHC.Generics                               (Generic)

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Vector.Unboxed                        as V
import           Data.Vector.Unboxed.Deriving

import           Physics.Constraint
import qualified Physics.Constraints.Contact.Friction       as F
import qualified Physics.Constraints.Contact.NonPenetration as NP
import qualified Physics.Constraints.Contact.Restitution    as R
import           Physics.Constraints.Types
import           Physics.Contact
import           Physics.Contact.Types
import           Utils.Descending
import           Utils.Utils

{- |
Indicates a specific pair of "features" on a specific pair of objects that are touching.
This is how we check if we can reuse a cached solution from the previous frame.
(We can reuse the cached solution if it has the same 'ObjectFeatureKey')
-}
data ObjectFeatureKey k = ObjectFeatureKey
  { _ofkObjKeys  :: (k, k) -- ^ (first shape's key, second shape's key)
  , _ofkFeatKeys :: (Int, Int) -- ^ (first shape's feature's key, second shape's feature's key)
  } deriving (Generic, Show, NFData, Eq, Ord)
makeLenses ''ObjectFeatureKey
derivingUnbox
  "ObjectFeatureKey"
  [t|forall k. (V.Unbox k) =>
                 ObjectFeatureKey k -> ((k, k), (Int, Int))|]
  [|\ObjectFeatureKey {..} -> (_ofkObjKeys, _ofkFeatKeys)|]
  [|uncurry ObjectFeatureKey|]

-- | Calculate all contacts between a pair of shapes.
keyedContacts ::
     (k, k)
  -> (Shape, Shape)
  -> Descending (ObjectFeatureKey k, Flipping Contact)
keyedContacts ij ab = fmap f contacts
  where contacts = generateContacts ab
        f (featKeys, contact) = (ObjectFeatureKey ij featKeys, contact)
        {-# INLINE f #-}
{-# INLINE keyedContacts #-}

-- | Build a constraint from a pair of shapes and a contact between them.
constraintGen ::
     ContactBehavior
  -> Double
  -> Flipping Contact
  -> (PhysicalObj, PhysicalObj)
  -> ContactConstraint
constraintGen beh dt fContact ab =
  ContactConstraint
  { _ccNonPen = NP.constraintGen beh dt fContact ab
  , _ccRestitution = R.constraintGen fContact ab
  , _ccFriction = F.constraintGen fContact ab
  }
{-# INLINE constraintGen #-}

nonPenWithRestitution ::
     (Double, Double) -- ^ bounciness
  -> ContactConstraint
  -> (PhysicalObj, PhysicalObj)
  -> Constraint
nonPenWithRestitution bounciness ContactConstraint {..} ab =
  Constraint
  {_constraintJ = _constraintJ, _constraintB = _constraintB + bounceB_}
  where
    Constraint {..} = _ccNonPen
    bounceB_ = R.bounceB bounciness _ccRestitution ab

contactLagrangian ::
     (Double, Double) -- ^ bounciness
  -> ContactConstraint
  -> (PhysicalObj, PhysicalObj)
  -> ContactLagrangian
contactLagrangian bounciness cc@ContactConstraint {..} ab =
  ContactLagrangian
  {_clNonPen = lagrangian2 ab nonPen, _clFriction = lagrangian2 ab _ccFriction}
  where
    nonPen = nonPenWithRestitution bounciness cc ab

{- |
Given an already-applied Lagrangian and the newly-calculated Lagrangian,
figure out what portion of the newly-calculated Lagrangian should actually be applied.
-}
solutionProcessor ::
     (Double, Double) -- ^ coefficients of friction for a pair of shapes (a, b)
  -> ContactLagrangian -- ^ cached solution
  -> ContactLagrangian -- ^ new incremental solution
  -> Processed (ContactLagrangian) -- ^ 1. incremental solution to actually apply, 2. new cached solution
solutionProcessor mu_ab (ContactLagrangian npCached fCached) (ContactLagrangian npNew fNew) =
  ContactLagrangian <$> npProcessed <*> fProcessed
  where npProcessed = NP.solutionProcessor npCached npNew
        fProcessed = F.solutionProcessor mu_ab (_processedToCache npProcessed) fCached fNew
{-# INLINE solutionProcessor #-}
