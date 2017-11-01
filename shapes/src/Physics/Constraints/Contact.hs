{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

{- |
Generate and solve all contact constraints for pairs of colliding objects.
-}
module Physics.Constraints.Contact where

import GHC.Generics (Generic)

import Control.Lens
import Control.DeepSeq
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V

import Physics.Constraint
import Physics.Constraints.Types
import Physics.Contact
import Physics.Contact.ConvexHull
import qualified Physics.Constraints.Contact.Friction as F
import qualified Physics.Constraints.Contact.NonPenetration as NP
import Utils.Descending
import Utils.Utils

data ObjectFeatureKey k =
  ObjectFeatureKey { _ofkObjKeys :: (k, k)
                   , _ofkFeatKeys :: (Int, Int)
                   } deriving (Generic, Show, NFData, Eq, Ord)
makeLenses ''ObjectFeatureKey
derivingUnbox "ObjectFeatureKey"
  [t| forall k. (V.Unbox k) => ObjectFeatureKey k -> ((k, k), (Int, Int)) |]
  [| \ObjectFeatureKey{..} -> (_ofkObjKeys, _ofkFeatKeys) |]
  [| uncurry ObjectFeatureKey |]

keyedContacts :: (k, k)
              -> (ConvexHull, ConvexHull)
              -> Descending (ObjectFeatureKey k, Flipping Contact')
keyedContacts ij ab = fmap f contacts
  where contacts = generateContacts ab
        f (featKeys, contact) = (ObjectFeatureKey ij featKeys, contact)
        {-# INLINE f #-}
{-# INLINE keyedContacts #-}

constraintGen :: ContactBehavior
              -> Double
              -> Flipping Contact'
              -> (PhysicalObj, PhysicalObj)
              -> ContactResult Constraint
constraintGen beh dt fContact ab =
  ContactResult { _crNonPen = NP.constraintGen beh dt fContact ab
                , _crFriction = F.constraintGen fContact ab }
{-# INLINE constraintGen #-}

solutionProcessor :: (Double, Double)
                  -> ContactResult Lagrangian
                  -> ContactResult Lagrangian
                  -> Processed (ContactResult Lagrangian)
solutionProcessor mu_ab (ContactResult npCached fCached) (ContactResult npNew fNew) =
  ContactResult <$> npProcessed <*> fProcessed
  where npProcessed = NP.solutionProcessor npCached npNew
        fProcessed = F.solutionProcessor mu_ab (_processedToCache npProcessed) fCached fNew
{-# INLINE solutionProcessor #-}
