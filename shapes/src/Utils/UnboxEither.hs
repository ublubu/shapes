{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Utils.UnboxEither where

import           Control.Monad
import           Control.Monad.ST

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

-- 'True' means 'a'. 'False' means 'b'.
data instance U.MVector s (Either a b) =
  MV_Either {-# UNPACK #-} !Int !(U.MVector s Bool)
                                !(U.MVector s a)
                                !(U.MVector s b)

data instance U.Vector (Either a b) =
  V_Either {-# UNPACK #-} !Int !(U.Vector Bool)
                               !(U.Vector a)
                               !(U.Vector b)

instance (U.Unbox a, U.Unbox b) => U.Unbox (Either a b)
instance (U.Unbox a, U.Unbox b) => M.MVector U.MVector (Either a b) where
  basicLength (MV_Either n_ _ _ _) = n_
  basicUnsafeSlice i_ m_ (MV_Either _ fs as bs) =
    MV_Either
      m_
      (M.basicUnsafeSlice i_ m_ fs)
      (M.basicUnsafeSlice i_ m_ as)
      (M.basicUnsafeSlice i_ m_ bs)
  basicOverlaps (MV_Either _ fs1 as1 bs1) (MV_Either _ fs2 as2 bs2) =
    M.basicOverlaps fs1 fs2 ||
    M.basicOverlaps as1 as2 || M.basicOverlaps bs1 bs2
  basicUnsafeNew n_ = do
    fs <- M.basicUnsafeNew n_
    as <- M.basicUnsafeNew n_
    bs <- M.basicUnsafeNew n_
    return $ MV_Either n_ fs as bs
  basicInitialize (MV_Either _ fs as bs) = do
    M.basicInitialize fs
    M.basicInitialize as
    M.basicInitialize bs
  basicUnsafeReplicate n_ (Left a) = do
    fs <- M.basicUnsafeReplicate n_ True
    as <- M.basicUnsafeReplicate n_ a
    bs <- M.basicUnsafeNew n_
    return $ MV_Either n_ fs as bs
  basicUnsafeReplicate n_ (Right b) = do
    fs <- M.basicUnsafeReplicate n_ False
    as <- M.basicUnsafeNew n_
    bs <- M.basicUnsafeReplicate n_ b
    return $ MV_Either n_ fs as bs
  basicUnsafeRead (MV_Either _ fs as bs) i_ = do
    f <- M.basicUnsafeRead fs i_
    if f
      then Left <$> M.basicUnsafeRead as i_
      else Right <$> M.basicUnsafeRead bs i_
  basicUnsafeWrite (MV_Either _ fs as _) i_ (Left a) = do
    M.basicUnsafeWrite fs i_ True
    M.basicUnsafeWrite as i_ a
  basicUnsafeWrite (MV_Either _ fs _ bs) i_ (Right b) = do
    M.basicUnsafeWrite fs i_ False
    M.basicUnsafeWrite bs i_ b
  basicClear (MV_Either _ fs as bs) = do
    M.basicClear fs
    M.basicClear as
    M.basicClear bs
  basicSet (MV_Either _ fs as _) (Left a) = do
    M.basicSet fs True
    M.basicSet as a
  basicSet (MV_Either _ fs _ bs) (Right b) = do
    M.basicSet fs False
    M.basicSet bs b
  basicUnsafeCopy (MV_Either _ fs1 as1 bs1) (MV_Either _ fs2 as2 bs2) = do
    M.basicUnsafeCopy fs1 fs2
    M.basicUnsafeCopy as1 as2
    M.basicUnsafeCopy bs1 bs2
  basicUnsafeMove (MV_Either _ fs1 as1 bs1) (MV_Either _ fs2 as2 bs2) = do
    M.basicUnsafeMove fs1 fs2
    M.basicUnsafeMove as1 as2
    M.basicUnsafeMove bs1 bs2
  basicUnsafeGrow (MV_Either n_ fs as bs) m_ = do
    fs' <- M.basicUnsafeGrow fs m_
    as' <- M.basicUnsafeGrow as m_
    bs' <- M.basicUnsafeGrow bs m_
    return $ MV_Either (m_ + n_) fs' as' bs'
instance (U.Unbox a, U.Unbox b) => G.Vector U.Vector (Either a b) where
  basicUnsafeFreeze (MV_Either n_ fs as bs) = do
    fs' <- G.basicUnsafeFreeze fs
    as' <- G.basicUnsafeFreeze as
    bs' <- G.basicUnsafeFreeze bs
    return $ V_Either n_ fs' as' bs'
  basicUnsafeThaw (V_Either n_ fs as bs) = do
    fs' <- G.basicUnsafeThaw fs
    as' <- G.basicUnsafeThaw as
    bs' <- G.basicUnsafeThaw bs
    return $ MV_Either n_ fs' as' bs'
  basicLength (V_Either n_ _ _ _) = n_
  basicUnsafeSlice i_ m_ (V_Either _ fs as bs) =
    V_Either
      m_
      (G.basicUnsafeSlice i_ m_ fs)
      (G.basicUnsafeSlice i_ m_ as)
      (G.basicUnsafeSlice i_ m_ bs)
  basicUnsafeIndexM (V_Either _ fs as bs) i_ = do
    f <- G.basicUnsafeIndexM fs i_
    if f
      then Left <$> G.basicUnsafeIndexM as i_
      else Right <$> G.basicUnsafeIndexM bs i_
  basicUnsafeCopy (MV_Either _ fs1 as1 bs1) (V_Either _ fs2 as2 bs2) = do
    G.basicUnsafeCopy fs1 fs2
    G.basicUnsafeCopy as1 as2
    G.basicUnsafeCopy bs1 bs2
  elemseq _ (Left a) = G.elemseq (undefined :: U.Vector a) a
  elemseq _ (Right b) = G.elemseq (undefined :: U.Vector b) b
