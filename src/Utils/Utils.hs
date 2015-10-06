{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Utils.Utils where

import Control.Lens
import Data.Maybe
import Data.Tuple
import qualified Data.IntMap.Strict as IM

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

pairAp :: (a -> b, c -> d) -> (a, c) -> (b, d)
pairAp (f, g) (x, y) = (f x, g y)

pairFold :: Monoid m => (m, m) -> m
pairFold (a, b) = mappend a b

maybeChange :: a -> (a -> Maybe a) -> a
maybeChange x f = fromMaybe x (f x)

toMaybe :: Bool -> a -> Maybe a
toMaybe b x = if b then Just x else Nothing

maybeBranch :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe (Either a a)
maybeBranch _ Nothing Nothing = Nothing
maybeBranch _ Nothing (Just x) = Just $ Right x
maybeBranch _ (Just x) Nothing = Just $ Left x
maybeBranch useLeft (Just x) (Just y) = if useLeft x y then Just $ Left x
                                        else Just $ Right y

takeIfAll :: (a -> Bool) -> [a] -> Maybe [a]
takeIfAll p [] = Just []
takeIfAll p (x:xs)
  | p x = fmap (x:) (takeIfAll p xs)
  | otherwise = Nothing

cycles :: [a] -> [[a]]
cycles xs = folds tail (cycle xs) xs

data Loop a = Loop { loopPrev :: Loop a
                   , loopVal :: a
                   , loopNext :: Loop a } deriving Show

loopify :: [a] -> Loop a
loopify [] = error "can't have an empty loop"
loopify v = let (first, last) = f last v first
            in first
  where f :: Loop a -> [a] -> Loop a -> (Loop a, Loop a)
        f prev [] next = (next, prev)
        f prev (x:xs) next = let (next', last') = f this xs next
                                 this = Loop prev x next'
                             in (this, last')

takeNext :: Int -> Loop a -> [Loop a]
takeNext = takeDir loopNext

takePrev :: Int -> Loop a -> [Loop a]
takePrev = takeDir loopPrev

takeDir :: (Loop a -> Loop a) -> Int -> Loop a -> [Loop a]
takeDir dir n x
  | n > 0 = x : takeDir dir (n - 1) (dir x)
  | n == 0 = []
  | otherwise = error "cannot take fewer than 0"

folds :: (b -> b) -> b -> [a] -> [b]
folds _ _ [] = []
folds f a0 (_:xs) = a0 : folds f (f a0) xs

data Flipping a = Same a | Flip a deriving Show

-- TODO: write an iso for Flipping and Either
flipAsEither :: Flipping a -> Either a a
flipAsEither (Same x) = Left x
flipAsEither (Flip x) = Right x

flipWrap :: Flipping a -> b -> Flipping b
flipWrap (Same _) = Same
flipWrap (Flip _) = Flip

flipFlip :: (a -> (b, b) -> c) -> Flipping a -> (b, b) -> c
flipFlip f (Same x) = f x
flipFlip f (Flip x) = f x . swap

flipMap :: (a -> (b, b) -> c) -> Flipping a -> (b, b) -> Flipping c
flipMap f x = flipWrap x . flipFlip f x

flipExtractWith :: (a -> b, a -> b) -> Flipping a -> b
flipExtractWith (f, _) (Same x) = f x
flipExtractWith (_, f) (Flip x) = f x

flipExtractPair :: (a -> (b, b)) -> Flipping a -> (b, b)
flipExtractPair f = flipExtractWith (f, swap . f)

instance Functor Flipping where
  fmap f (Same x) = Same (f x)
  fmap f (Flip x) = Flip (f x)

flipExtract :: Flipping a -> a
flipExtract (Same x) = x
flipExtract (Flip x) = x

flipInjectF :: Functor f => Flipping (f a) -> f (Flipping a)
flipInjectF x = fmap (flipWrap x) . flipExtract $ x

-- TODO: pull out the stuff that depends on lens.

ixZipWith :: (Ixed s, TraversableWithIndex (Index s) t) => (a -> Maybe (IxValue s) -> b) -> t a -> s -> t b
ixZipWith f xs ys = xs & itraversed %@~ g
  where g i x = f x (ys ^? ix i)

overWith :: Lens' s a -> ((a, a) -> (a, a)) -> (s, s) -> (s, s)
overWith l f (x, y) = (x & l .~ a, y & l .~ b)
  where (a, b) = f (x ^. l, y ^. l)

findOrInsert :: IM.Key -> a -> IM.IntMap a -> (Maybe a, IM.IntMap a)
findOrInsert = IM.insertLookupWithKey (\_ _ a -> a)

findOrInsert' :: IM.Key -> a -> IM.IntMap a -> (a, IM.IntMap a)
findOrInsert' k x t = (fromMaybe x mx, t')
  where (mx, t') = findOrInsert k x t

posMod :: (Integral a) => a -> a -> a
posMod x n = if res < 0 then res + n else res
  where res = x `mod` n

pairiix :: (Ixed m) => (Index m, Index m) -> IndexedTraversal' (Index m, Index m) m (IxValue m, IxValue m)
pairiix ij f = pairix ij (indexed f ij)

-- Applicative f => (IxValue m -> f (IxValue m)) -> m -> f m
pairix :: (Ixed m) => (Index m, Index m) -> Traversal' m (IxValue m, IxValue m)
pairix ij@(i, j) f t = maybe (pure t) change pair
  where pair = do
          a <- t ^? ix i
          b <- t ^? ix j
          return (a, b)
        change pair' = uncurry g <$> indexed f ij pair'
          where g a b = set (ix j) b . set (ix i) a $ t
