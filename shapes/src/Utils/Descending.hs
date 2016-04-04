{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Descending where

import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV

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

descZipVector :: forall a b c s k. (Ord k, MV.MVector V.MVector b)
              => (a -> k)
              -> (b -> k)
              -> (c -> a -> b -> ST s c)
              -> (c -> a -> ST s c)
              -> c
              -> Descending a
              -> V.MVector s b
              -> ST s c
descZipVector getThisKey getThatKey accumBoth accumThis accum0 these those =
  let f :: (Int, c) -> a -> ST s (Int, c)
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
        where thatCount = MV.length those
              thisKey = getThisKey this
  in snd <$> foldM f (0, accum0) these
{-# INLINE descZipVector #-}
