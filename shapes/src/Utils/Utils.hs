{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Utils.Utils where

import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens
import Data.Hashable
import Data.Maybe
import Data.Tuple
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving

data SP a b = SP { _spFst :: !a
                 , _spSnd :: !b
                 } deriving (Show, Eq, Generic, NFData, Hashable, Ord)
makeLenses ''SP
derivingUnbox "SP"
  [t| forall a b. (V.Unbox a, V.Unbox b) => SP a b -> (a, b) |]
  [| \SP{..} -> (_spFst, _spSnd) |]
  [| uncurry SP |]

type SP' a = SP a a

toSP :: (a, b) -> SP a b
toSP (x, y) = SP x y

fromSP :: SP a b -> (a, b)
fromSP (SP x y) = (x, y)

spMap :: (a -> b) -> SP a a -> SP b b
spMap f (SP x y) = SP (f x) (f y)

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

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

maybeBranch :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe (Either a a)
maybeBranch _ Nothing Nothing = Nothing
maybeBranch _ Nothing (Just x) = Just $ Right x
maybeBranch _ (Just x) Nothing = Just $ Left x
maybeBranch useLeft (Just x) (Just y) = if useLeft x y then Just $ Left x
                                        else Just $ Right y

maybeBranchBoth :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe (Either a a)
maybeBranchBoth _ Nothing _ = Nothing
maybeBranchBoth _ _ Nothing = Nothing
maybeBranchBoth useLeft (Just x) (Just y) =
  if useLeft x y then Just $ Left x
  else Just $ Right y

takeIfAll :: (a -> Bool) -> [a] -> Maybe [a]
takeIfAll p [] = Just []
takeIfAll p (x:xs)
  | p x = fmap (x:) (takeIfAll p xs)
  | otherwise = Nothing

cycles :: [a] -> [[a]]
cycles xs = folds tail (cycle xs) xs

data Loop a = Loop { loopPrev :: Loop a
                   , loopVal :: !a
                   , loopNext :: Loop a }

-- TODO: for debugging only
instance (Show a, Eq a) => Show (Loop a) where
  show l = "loopify [" ++ f l [] ++ "]"
    where f x seen = if loopVal y `elem` seen' then show val
                     else show val ++ ", " ++ f y seen'
            where y = loopNext x
                  seen' = val:seen
                  val = loopVal x

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

data Flipping a = Same !a | Flip !a deriving Show

flipToTuple :: Flipping a -> (Bool, a)
flipToTuple (Same x) = (True, x)
flipToTuple (Flip x) = (False, x)

derivingUnbox "Flipping"
  [t| forall a. (V.Unbox a) => Flipping a -> (Bool, a) |]
  [| flipToTuple |]
  [| \(isSame, x) -> if isSame then Same x else Flip x |]

-- TODO: write an iso for Flipping and Either
flipAsEither :: Flipping a -> Either a a
flipAsEither (Same x) = Left x
flipAsEither (Flip x) = Right x

flipWrap :: Flipping a -> b -> Flipping b
flipWrap (Same _) = Same
flipWrap (Flip _) = Flip

flipUnsafe :: (a -> (b, b) -> c) -> Flipping a -> (b, b) -> c
flipUnsafe f (Same x) = f x
flipUnsafe f (Flip x) = f x . swap

flipMap :: (a -> (b, b) -> c) -> Flipping a -> (b, b) -> Flipping c
flipMap f x = flipWrap x . flipUnsafe f x

flipExtractWith :: (a -> b, a -> b) -> Flipping a -> b
flipExtractWith (f, _) (Same x) = f x
flipExtractWith (_, f) (Flip x) = f x

flipExtractPair :: (a -> (b, b)) -> Flipping a -> (b, b)
flipExtractPair f = flipExtractWith (f, swap . f)

flipJoin :: Flipping (Flipping a) -> Flipping a
flipJoin (Same (Same x)) = Same x
flipJoin (Flip (Same x)) = Flip x
flipJoin (Same (Flip x)) = Flip x
flipJoin (Flip (Flip x)) = Same x

instance Functor Flipping where
  fmap f (Same x) = Same (f x)
  fmap f (Flip x) = Flip (f x)

class Flippable f where
  flipp :: f -> f

instance Flippable (x, x) where
  flipp = swap

instance Flippable (Flipping x) where
  flipp (Same x) = Flip x
  flipp (Flip x) = Same x

flipExtract :: (Flippable a) => Flipping a -> a
flipExtract (Same x) = x
flipExtract (Flip x) = flipp x

flipExtractUnsafe :: Flipping a -> a
flipExtractUnsafe (Same x) = x
flipExtractUnsafe (Flip x) = x

flipInjectF :: Functor f => Flipping (f a) -> f (Flipping a)
flipInjectF x = fmap (flipWrap x) . flipExtractUnsafe $ x

eitherBranchBoth :: (b -> b -> Bool) -> Either a b -> Either a b -> Flipping (Either a b)
eitherBranchBoth _ x@(Left _) _ = Same x
eitherBranchBoth _ _ x@(Left _) = Flip x
eitherBranchBoth useLeft x@(Right a) y@(Right b) =
  if useLeft a b then Same x else Flip y

liftRightMaybe :: Either a (Maybe b) -> Maybe (Either a b)
liftRightMaybe (Right Nothing) = Nothing
liftRightMaybe (Right (Just x)) = Just $ Right x
liftRightMaybe (Left x) = Just $ Left x

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

newtype Descending a =
  Descending { _descList :: [a] } deriving (Generic, NFData)
makeLenses ''Descending

instance Functor Descending where
  fmap f (Descending xs) = Descending $ fmap f xs

instance Applicative Descending where
  pure = Descending . pure
  (Descending fs) <*> (Descending xs) = Descending (fs <*> xs)

instance Monad Descending where
  (Descending xs) >>= f = Descending (xs >>= _descList . f)

instance Foldable Descending where
  foldMap f (Descending xs) = foldMap f xs
