{-# LANGUAGE RecordWildCards #-}

module Utils.EmptiesVector where

import           Prelude                     hiding (length, map, mapM, mapM_,
                                              read)
import qualified Prelude                     as P

import           Control.Monad.Primitive
import           Data.Monoid
import           Data.STRef
import qualified Data.Vector.Unboxed.Mutable as V

{- |
Keeps track of 'empty' slots.
-}
data EmptiesVector s =
  EmptiesVector
  { _evVector    :: V.MVector s Int
  , _evEmptyHead :: STRef s Int
  , _evFilled    :: STRef s Int
  }

sentinel :: Int
sentinel = -1

new ::
     (PrimMonad m)
  => Int -- ^ capacity
  -> m (EmptiesVector (PrimState m))
new n = do
  vec <- V.unsafeNew n
  emptyHead <- stToPrim $ newSTRef 0
  filled <- stToPrim $ newSTRef 0
  let loop i
        | i < n =
          let j = i + 1
          in V.write vec i j >> loop j
        | otherwise = return ()
  loop 0
  return
    EmptiesVector
    {_evVector = vec, _evEmptyHead = emptyHead, _evFilled = filled}

-- | Assumes this slot is not already empty. No error if this assumption is false.
delete :: (PrimMonad m) => EmptiesVector (PrimState m) -> Int -> m ()
delete EmptiesVector {..} i = do
  emptyHead <- stToPrim $ readSTRef _evEmptyHead
  -- 'i' is the new emptyHead
  -- _evEmptyHead -> emptyHead -> ...
  --     becomes
  -- _evEmptyHead -> i -> emptyHead -> ...
  stToPrim $ writeSTRef _evEmptyHead i
  V.write _evVector i emptyHead
  stToPrim $ modifySTRef _evFilled (subtract 1)

-- | Assumes this slot is not already empty. Errors if this assumption is false.
safeDelete :: (PrimMonad m) => EmptiesVector (PrimState m) -> Int -> m ()
safeDelete EmptiesVector {..} i = do
  val <- V.read _evVector i
  if val == sentinel
    then error $ "i=" <> show i <> " points at an empty slot"
    else do
      emptyHead <- stToPrim $ readSTRef _evEmptyHead
      -- 'i' is the new emptyHead
      stToPrim $ writeSTRef _evEmptyHead i
      V.write _evVector i emptyHead
      stToPrim $ modifySTRef _evFilled (subtract 1)

-- | Assumes the 'SparseVector' is not full.
append ::
     (PrimMonad m)
  => EmptiesVector (PrimState m)
  -> m Int -- ^ the index of the newly-added object
append EmptiesVector {..} = do
  emptyHead <- stToPrim $ readSTRef _evEmptyHead
  emptyHead' <- V.read _evVector emptyHead
  if emptyHead' == sentinel
    then error $ "emptyHead=" <> show emptyHead <> " points at a filled slot"
      -- Write to the old emptyHead. Update emptyHead to the next empty slot.
      -- _evEmptyHead -> emptyHead -> emptyHead' -> ...
      --     becomes
      -- _evEmptyHead -> emptyHead' -> ...
    else do
      V.write _evVector emptyHead sentinel
      stToPrim $ writeSTRef _evEmptyHead emptyHead'
      stToPrim $ modifySTRef _evFilled (+ 1)
      return emptyHead


-- | Is this slot occupied?
read :: (PrimMonad m) => EmptiesVector (PrimState m) -> Int -> m Bool
read EmptiesVector {..} i = do
  val <- V.read _evVector i
  return $ val == sentinel

-- | Errors if the 'EmptiesVector' is messed up.
validate :: (PrimMonad m) => EmptiesVector (PrimState m) -> m ()
validate EmptiesVector {..} = do
  empties <-
    (P.length . filter (/= sentinel)) <$>
    P.mapM (V.read _evVector) [0 .. (n - 1)]
  emptyHead <- stToPrim $ readSTRef _evEmptyHead
  let loop emptyHead =
        if emptyHead == n
          then return 1
          else do
            emptyHead' <- V.read _evVector emptyHead
            if emptyHead' == sentinel
              then error $
                   "empty slot points to filled slot i=" <> show emptyHead
              else (1 +) <$> loop emptyHead'
  empties' <- loop emptyHead
  if empties /= empties'
    then error $
         "stack contains " <> show empties' <> " slots, but there are " <>
         show empties <>
         " empty slots"
    else return ()
  where
    n = V.length _evVector

length :: EmptiesVector s -> Int
length EmptiesVector {..} = V.length _evVector

filled :: (PrimMonad m) => EmptiesVector (PrimState m) -> m Int
filled EmptiesVector {..} = stToPrim $ readSTRef _evFilled

-- | Perform some action for the index of each filled slot.
mapM_ :: (PrimMonad m) => (Int -> m ()) -> EmptiesVector (PrimState m) -> m ()
mapM_ f_ empties = P.mapM_ f [0 .. (length empties - 1)]
  where
    f i = do
      nonempty <- read empties i
      if nonempty
        then f_ i
        else return ()

-- | Fold the indices of filled slots into a single value.
foldM ::
     (PrimMonad m)
  => (b -> Int -> m b)
  -> b
  -> EmptiesVector (PrimState m)
  -> m b
foldM f accum0 empties = loop 0 accum0
  where
    loop i accum
      | i < n = do
        nonempty <- read empties i
        if nonempty
          then loop (i + 1) =<< f accum i
          else loop (i + 1) accum
      | otherwise = return accum
    n = length empties

-- | Map the indices of filled slots into a new vector.
mapM :: (V.Unbox a, PrimMonad m)
  => (Int -> m a)
  -> EmptiesVector (PrimState m)
  -> m (V.MVector (PrimState m) a)
mapM f_ empties = do
  n <- filled empties
  vec <- V.new n
  let f vec_i empties_i = do
        val <- f_ empties_i
        V.write vec vec_i val
        return $ vec_i + 1
  foldM f 0 empties
  return vec
