{-# LANGUAGE FlexibleInstances #-}

module FreezableT where

import Data.Monoid
import Data.Foldable

-- used to terminate foldMaps early
data FreezableT a = Freezable a | Frozen a

instance Monoid a => Monoid (FreezableT a) where
  mempty = Freezable mempty
  mappend x@(Frozen _) _ = x
  mappend (Freezable x) (Frozen y) = Frozen (mappend x y)
  mappend (Freezable x) (Freezable y) = Freezable (mappend x y)

foldMapUntil :: (Foldable t, Monoid m) => (a -> Bool) -> (a -> m) -> t a -> m
foldMapUntil test f = unfreezer . foldMap f'
  where f' x = if test x then Frozen mempty
               else Freezable $ f x

data TerminatingFold b = TerminatingFold (b -> (Bool, b))

instance Monoid (TerminatingFold b) where
  mempty = TerminatingFold (\x -> (False, x))
  mappend (TerminatingFold f) (TerminatingFold f') =
    TerminatingFold (\b -> let (t, b') = f b in if t
                           then (True, b')
                           else f' b')

evalWith :: b -> TerminatingFold b -> b
evalWith x (TerminatingFold f) = snd (f x)

foldlUntil_ :: Foldable t => (b -> a -> (Bool, b)) -> b -> t a -> b
foldlUntil_ f a0 = (evalWith a0) . (foldMap f')
  where f' x = TerminatingFold (`f` x)

foldlUntil :: Foldable t => (b -> a -> Bool) -> (b -> a -> b) -> b -> t a -> b
foldlUntil test f a0 = (evalWith a0) . (foldMap f')
  where f' x = TerminatingFold (\b -> ((b `test` x), (b `f` x)))

unfreezer :: FreezableT a -> a
unfreezer (Freezable x) = x
unfreezer (Frozen x) = x

