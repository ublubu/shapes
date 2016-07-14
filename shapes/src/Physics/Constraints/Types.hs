{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Physics.Constraints.Types where

import Control.Lens
import Data.Monoid
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)

import Physics.Constraint
import Physics.Contact

import Utils.Utils

type ContactConstraintGen a = Flipping Contact' -> (a, a) -> Constraint

data Processed a =
  Processed { _processedToCache :: !a
            , _processedToApply :: !a
            }

type SolutionProcessor a b = a -> b -> b -> Processed b

data ContactResult a =
  ContactResult { _crNonPen :: a
                , _crFriction :: a
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
