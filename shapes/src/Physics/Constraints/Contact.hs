{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.Constraints.Contact where

import GHC.Generics (Generic)

import Control.Lens
import Control.DeepSeq
import Data.Vector.Unboxed.Deriving

import Physics.Constraint
import Physics.Constraints.Types
import Physics.Contact
import qualified Physics.Constraints.Contact.Friction as F
import qualified Physics.Constraints.Contact.NonPenetration as NP
import Utils.Descending
import Utils.Utils

data ObjectFeatureKey =
  ObjectFeatureKey { _ofkObjKeys :: !(SP Int Int)
                   , _ofkFeatKeys :: !(SP Int Int)
                   } deriving (Generic, Show, NFData, Eq, Ord)
makeLenses ''ObjectFeatureKey
derivingUnbox "ObjectFeatureKey"
  [t| ObjectFeatureKey -> SP (SP Int Int) (SP Int Int) |]
  [| \ObjectFeatureKey{..} -> SP _ofkObjKeys _ofkFeatKeys |]
  [| \SP{..} -> ObjectFeatureKey _spFst _spSnd |]

keyedContacts :: (Contactable a)
              => (Int, Int)
              -> (a, a)
              -> Descending (ObjectFeatureKey, Flipping Contact')
keyedContacts ij ab = fmap f contacts
  where contacts = generateContacts ab
        f (featKeys, contact) = (ObjectFeatureKey (toSP ij) (toSP featKeys), contact)
        {-# INLINE f #-}
{-# INLINE keyedContacts #-}

constraintGen :: (Contactable a)
              => ContactBehavior
              -> Double
              -> Flipping Contact'
              -> (a, a)
              -> ContactResult Constraint
constraintGen beh dt fContact ab =
  ContactResult { _crNonPen = NP.constraintGen beh dt fContact ab
                , _crFriction = F.constraintGen fContact ab }
{-# INLINE constraintGen #-}

solutionProcessor :: (Contactable a)
                  => (a, a)
                  -> ContactResult Lagrangian
                  -> ContactResult Lagrangian
                  -> Processed (ContactResult Lagrangian)
solutionProcessor ab (ContactResult npCached fCached) (ContactResult npNew fNew) =
  ContactResult <$> npProcessed <*> fProcessed
  where npProcessed = NP.solutionProcessor npCached npNew
        fProcessed = F.solutionProcessor ab (_processedToCache npProcessed) fCached fNew
{-# INLINE solutionProcessor #-}
