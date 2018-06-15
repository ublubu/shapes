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

import           Utils.Utils

type ContactConstraintGen a = Flipping Contact -> (a, a) -> Constraint

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

{- |
Used in the constraint solver to cache solutions ('ContactResult Lagrangian')
and constraints ('ContactResult Constraint') in unboxed vectors.

Constraints are calculated from contacts, and Lagrangians are calculated from these constraints,
which makes both types "results" of a contact.
-}
data ContactResult a = ContactResult
  { _crNonPen   :: a -- ^ "result" related to the non-penetration constraint
  , _crFriction :: a -- ^ "result" related to the friction constraint
  }

derivingUnbox "ContactResult"
  [t| forall a. (Unbox a) => ContactResult a -> (a, a) |]
  [| \ContactResult{..} -> (_crNonPen, _crFriction) |]
  [| uncurry ContactResult |]

makeLenses ''ContactResult

instance Functor ContactResult where
  fmap f (ContactResult a b) = ContactResult (f a) (f b)
  {-# INLINE fmap #-}

instance Applicative ContactResult where
  pure x = ContactResult x x
  ContactResult f g <*> ContactResult x y = ContactResult (f x) (g y)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- do nonpen before friction
instance Foldable ContactResult where
  foldMap f (ContactResult a b) = f a <> f b

instance Functor Processed where
  fmap f (Processed a b) = Processed (f a) (f b)
  {-# INLINE fmap #-}

instance Applicative Processed where
  pure x = Processed x x
  Processed f g <*> Processed x y = Processed (f x) (g y)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
