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
  -> Descending (ObjectFeatureKey k, Flipping Contact')
keyedContacts ij ab = fmap f contacts
  where contacts = generateContacts ab
        f (featKeys, contact) = (ObjectFeatureKey ij featKeys, contact)
        {-# INLINE f #-}
{-# INLINE keyedContacts #-}

-- | Build a constraint from a pair of shapes and a contact between them.
constraintGen ::
     ContactBehavior
  -> Double
  -> Flipping Contact'
  -> (PhysicalObj, PhysicalObj)
  -> ContactResult Constraint
constraintGen beh dt fContact ab =
  ContactResult { _crNonPen = NP.constraintGen beh dt fContact ab
                , _crFriction = F.constraintGen fContact ab }
{-# INLINE constraintGen #-}

{- |
Given an already-applied Lagrangian and the newly-calculated Lagrangian,
figure out what portion of the newly-calculated Lagrangian should actually be applied.
-}
solutionProcessor ::
     (Double, Double) -- ^ coefficients of friction for a pair of shapes (a, b)
  -> ContactResult Lagrangian -- ^ cached solution
  -> ContactResult Lagrangian -- ^ new incremental solution
  -> Processed (ContactResult Lagrangian) -- ^ 1. incremental solution to actually apply, 2. new cached solution
solutionProcessor mu_ab (ContactResult npCached fCached) (ContactResult npNew fNew) =
  ContactResult <$> npProcessed <*> fProcessed
  where npProcessed = NP.solutionProcessor npCached npNew
        fProcessed = F.solutionProcessor mu_ab (_processedToCache npProcessed) fCached fNew
{-# INLINE solutionProcessor #-}
