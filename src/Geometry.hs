module Geometry where

import Control.Applicative
import Data.Foldable
import Data.Monoid

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure f = Pair f f
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Num a => Num (Pair a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger a = pure (fromInteger a)

instance Foldable Pair where
  foldMap f (Pair x y) = mappend (f x) (f y)

cross :: Num a => Pair a -> Pair a -> a
cross (Pair ax ay) (Pair bx by) = (ax * by) - (bx * ay)

dot :: Num a => Pair a -> Pair a -> a
dot (Pair ax ay) (Pair bx by) = (ax * bx) + (ay * by)

zero :: Num a => Pair a
zero = (Pair 0 0)

pairToTuple :: Pair a -> (a, a)
pairToTuple (Pair a b) = (a, b)

tupleToPair :: (a, a) -> Pair a
tupleToPair (a, b) = Pair a b

data Transform a = Transform { transformScale :: Pair a
                             , transformOrigin :: Pair a } deriving (Eq, Show)

