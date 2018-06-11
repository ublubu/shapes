{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Keep track of lists that are in some kind of descending order.
Doesn't do anything fancy to actually enforce this, though.
-}
module Utils.Descending where

import           GHC.Generics                (Generic)

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed         as V

newtype Descending a =
  Descending { _descList :: [a] } deriving (Generic, NFData)
makeLenses ''Descending

instance Functor Descending where
  fmap f (Descending xs) = Descending $ fmap f xs
  {-# INLINE fmap #-}

instance Applicative Descending where
  pure = Descending . pure
  (Descending fs) <*> (Descending xs) = Descending (fs <*> xs)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Descending where
  (Descending xs) >>= f = Descending (xs >>= _descList . f)
  {-# INLINE (>>=) #-}

instance Foldable Descending where
  foldMap f (Descending xs) = foldMap f xs
  {-# INLINE foldMap #-}

descZipVector ::
     forall a b c m k. (Ord k, MV.MVector V.MVector b, PrimMonad m)
  => (a -> k)
  -> (b -> k)
  -> (c -> a -> b -> m c)
  -> (c -> a -> m c)
  -> c
  -> Descending a
  -> V.MVector (PrimState m) b
  -> m c
descZipVector getThisKey getThatKey accumBoth accumThis accum0 these those =
  let f :: (Int, c) -> a -> m (Int, c)
      f (that_i, accum) this
        | that_i < thatCount = do
          that <- MV.read those that_i
          let thatKey = getThatKey that
          if thisKey < thatKey
            then f (that_i + 1, accum) this -- keep looking
            else if thisKey == thatKey
                   then (,) (that_i + 1) <$> accumBoth accum this that
                   else (,) that_i <$> accumThis accum this
        | otherwise = (,) that_i <$> accumThis accum this
        where
          thatCount = MV.length those
          thisKey = getThisKey this
  in snd <$> foldM f (0, accum0) these
{-# INLINE descZipVector #-}
