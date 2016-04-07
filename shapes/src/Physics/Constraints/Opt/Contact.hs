{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.Constraints.Opt.Contact where

import GHC.Generics (Generic)

import Control.Lens
import Control.DeepSeq
import Data.Vector.Unboxed.Deriving

import Physics.Constraint.Opt
import Physics.Contact.Opt
import Physics.Contact.Opt.ConvexHull
import qualified Physics.Constraints.Opt.Friction as F
import qualified Physics.Constraints.Opt.NonPenetration as NP
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

keyedContacts :: (Int, Int)
              -> (ConvexHull, ConvexHull)
              -> Descending (ObjectFeatureKey, Flipping Contact')
keyedContacts ij ab = fmap f contacts
  where contacts = generateContacts ab
        f (featKeys, contact) = (ObjectFeatureKey (toSP ij) (toSP featKeys), contact)
        {-# INLINE f #-}
{-# INLINE keyedContacts #-}

data ContactSolution =
  ContactSolution { _contactNonPen :: ConstraintResult
                  , _contactFriction :: ConstraintResult
                  } deriving (Show)
makeLenses ''ContactSolution

derivingUnbox "ContactSolution"
  [t| ContactSolution -> (ConstraintResult, ConstraintResult) |]
  [| \ContactSolution{..} -> (_contactNonPen, _contactFriction) |]
  [| uncurry ContactSolution |]

solveContact :: ContactBehavior
             -> Double
             -> (PhysicalObj, PhysicalObj)
             -> Flipping Contact'
             -> ContactSolution
solveContact beh dt ab fContact =
  ContactSolution { _contactNonPen = constraintResult nonpen ab
                  , _contactFriction = constraintResult friction ab
                  }
  where nonpen = flipExtract $ flipMap (NP.toConstraint beh dt) fContact ab
        friction = flipExtract $ flipMap (F.toConstraint beh dt) fContact ab
{-# INLINE solveContact #-}

getContactConstraint :: ContactBehavior
                     -> Double
                     -> (PhysicalObj, PhysicalObj)
                     -> Flipping Contact'
                     -> ContactSolution
getContactConstraint beh dt ab fContact =
  ContactSolution { _contactNonPen = (0, nonpen)
                  , _contactFriction = (0, friction)
                  }
  where nonpen = flipExtract $ flipMap (NP.toConstraint beh dt) fContact ab
        friction = flipExtract $ flipMap (F.toConstraint beh dt) fContact ab
{-# INLINE getContactConstraint #-}

updateContactSln :: ContactBehavior
                 -> Double
                 -> ContactSolution
                 -> (PhysicalObj, PhysicalObj)
                 -> Flipping Contact'
                 -> ContactSolution
updateContactSln beh dt sln@ContactSolution{..} ab fContact =
  sln & contactNonPen._2 .~ nonpen & contactFriction._2 .~ friction
  where nonpen = flipExtract $ flipMap (NP.toConstraint beh dt) fContact ab'
        friction = flipExtract $ flipMap (F.toConstraint beh dt) fContact ab'
        ab' = ab & each %~ view physObj
{-# INLINE updateContactSln #-}

emptyContactSln :: ContactSolution -> ContactSolution
emptyContactSln ContactSolution{..} =
  ContactSolution (_contactNonPen & _1 .~ 0) (_contactFriction & _1 .~ 0)
{-# INLINE emptyContactSln #-}

solveContactAgain :: ContactSolution
                  -> (PhysicalObj, PhysicalObj)
                  -> ContactSolution
solveContactAgain ContactSolution{..} ab =
  ContactSolution { _contactNonPen = constraintResult (snd _contactNonPen) ab
                  , _contactFriction = constraintResult (snd _contactFriction) ab
                  }
{-# INLINE solveContactAgain #-}
