module SideScroller.WorldUnit where

import Control.Applicative

data WorldPos a = WorldPos a
data WorldTime a = WorldTime a
data WorldRate a = WorldRate a

type WorldSpeed a = WorldRate (WorldPos a)
type WorldAccel a = WorldRate (WorldSpeed a)

instance Functor WorldPos where
  fmap f (WorldPos a) = WorldPos (f a)

instance Functor WorldTime where
  fmap f (WorldTime a) = WorldTime (f a)

instance Functor WorldRate where
  fmap f (WorldRate a) = WorldRate (f a)

instance Applicative WorldPos where
  pure a = WorldPos a
  (WorldPos f) <*> (WorldPos x) = WorldPos (f x)

instance Applicative WorldTime where
  pure a = WorldTime a
  (WorldTime f) <*> (WorldTime t) = WorldTime (f t)

instance Applicative WorldRate where
  pure a = WorldRate a
  (WorldRate f) <*> (WorldRate r) = WorldRate (f r)

integral :: (Functor b, Num a) => WorldRate (b a) -> WorldTime a -> b a
integral (WorldRate x) (WorldTime dt) = fmap (*dt) x

speed :: (Fractional a) => WorldPos a -> WorldTime a -> WorldRate (WorldPos a)
speed (WorldPos x) (WorldTime t) = WorldRate (WorldPos (x / t))

translate :: Num a => WorldPos a -> WorldPos a -> WorldPos a
translate (WorldPos x) (WorldPos dx) = WorldPos (x + dx)

derivative :: (Fractional a) => WorldRate a -> WorldTime a -> WorldRate (WorldRate a)
derivative (WorldRate a) (WorldTime t) = WorldRate (WorldRate (a / t))

rescale :: (Functor t, Num a) => a -> t a -> t a
rescale scale x = fmap (*scale) x

instance Num a => Num (WorldPos a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger x = WorldPos (fromInteger x)

instance Num a => Num (WorldTime a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger x = WorldTime (fromInteger x)

instance Num a => Num (WorldRate a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger x = WorldRate (fromInteger x)

