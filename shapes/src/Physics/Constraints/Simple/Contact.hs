{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Constraints.Simple.Contact where

import Control.Lens
import Linear.Epsilon
import Physics.Constraint.Simple
import Physics.Solver.Simple.Contact
import Physics.Contact.Simple
import qualified Physics.Constraints.Simple.Friction as F
import Physics.Linear.Simple
import qualified Physics.Constraints.Simple.NonPenetration as NP
import Physics.Contact.Simple.OptimizedSAT
import Utils.PairMap
import Utils.Utils

generator :: (Contactable n a, Epsilon n, Floating n, Ord n) => ConstraintGen' n a
generator = getGenerator defaultContactBehavior

getGenerator :: forall n a . (Contactable n a, Epsilon n, Floating n, Ord n)
             => ContactBehavior n
             -> ConstraintGen' n a
getGenerator beh = ConstraintGen' gen
  where gen dt pairKey ab = (fmap f (generateContacts ab), getGenerator beh)
          where f c = (flipExtractPair contactIndex c, ContactResult (np c) (fr c))
                np c _ = flipConstraint $ flipMap (NP.toConstraint beh dt) c ab'
                fr c _ = flipConstraint $ flipMap (F.toConstraint beh dt) c ab'
                ab' :: (PhysicalObj n, PhysicalObj n)
                ab' = pairMap (view physObj) ab

getOptGenerator :: forall n a . (Contactable n a, Epsilon n, Floating n, Ord n)
                => ContactBehavior n
                -> PairMap (Flipping SATCache)
                -> ConstraintGen' n a
getOptGenerator beh cache = ConstraintGen' gen
  where gen dt pairKey ab = (fmap f contacts, getOptGenerator beh cache')
          where f c = (flipExtractPair contactIndex c, ContactResult (np c) (fr c))
                np c _ = flipConstraint $ flipMap (NP.toConstraint beh dt) c ab'
                fr c _ = flipConstraint $ flipMap (F.toConstraint beh dt) c ab'
                ab' :: (PhysicalObj n, PhysicalObj n)
                ab' = pairMap (view physObj) ab
                (contacts, mCacheEntry) = optGenerateContacts (lookupPair pairKey cache) ab
                cache' = maybe cache (\entry -> insertPair pairKey entry cache) mCacheEntry

flipConstraint :: Flipping (Constraint a) -> Constraint a
flipConstraint = flipExtractWith (id, f)
  where f (Constraint j b) = Constraint (flip33 j) b
