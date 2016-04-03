{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Physics.Constraints.Opt.Contact where

import GHC.Generics (Generic)

import Control.Lens
import Control.DeepSeq
import Data.Hashable

import Physics.Constraint.Opt
import Physics.Linear.Opt
import Physics.Contact.Opt
import qualified Physics.Constraints.Opt.Friction as F
import qualified Physics.Constraints.Opt.NonPenetration as NP
import Utils.Utils

flipConstraint :: Flipping Constraint -> Constraint
flipConstraint = flipExtractWith (id, f)
  where f (Constraint j b) = Constraint (flip3v3 j) b

data ObjectFeatureKey =
  ObjectFeatureKey { _ofkObjKeys :: !(SP Int Int)
                   , _ofkFeatKeys :: !(SP Int Int)
                   } deriving (Generic, Show, Hashable, NFData, Eq)
makeLenses ''ObjectFeatureKey

keyedContacts :: (Contactable a)
              => (Int, Int)
              -> (a, a)
              -> [(ObjectFeatureKey, Contact')]
keyedContacts ij ab = fmap (flipExtractUnsafe . flip (flipMap f) ij) fContacts
  where fContacts = generateContacts ab
        f (featKeys, contact) objKeys = (ObjectFeatureKey (toSP objKeys) (toSP featKeys), contact)

data ContactSolution =
  ContactSolution { _contactNonPen :: ConstraintResult
                  , _contactFriction :: ConstraintResult
                  } deriving (Show)

solveContact :: (Contactable a)
             => ContactBehavior
             -> Double
             -> (a, a)
             -> Contact'
             -> ContactSolution
solveContact beh dt ab contact =
  ContactSolution { _contactNonPen = constraintResult nonpen ab'
                  , _contactFriction = constraintResult friction ab'
                  }
  where nonpen = NP.toConstraint beh dt contact ab'
        friction = F.toConstraint beh dt contact ab'
        ab' = ab & each %~ view physObj
