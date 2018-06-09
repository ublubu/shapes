{-# LANGUAGE RecordWildCards #-}

module Utils.SparseVector where

import           Control.Monad.ST
import           Data.Either                  (isLeft)
import           Data.Monoid
import           Data.STRef
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable  as V

import           Utils.UnboxEither

type Sparse a = Either Int a

{- |
Side-by-side vectors of actual values and a stack of free slots.

Using this as a sort of fixed-capacity IntMap.
-}
data SparseVector s a =
  SparseVector
  { _svVector    :: V.MVector s (Sparse a)
  , _svEmptyHead :: STRef s Int
  }

new :: (V.Unbox a)
  => Int -- ^ capacity
  -> ST s (SparseVector s a)
new n = do
  vec <- V.unsafeNew n
  emptyHead <- newSTRef 0
  let f i = V.write vec i $ Left (i+1)
  mapM_ f [0..n]
  return SparseVector {_svVector = vec, _svEmptyHead = emptyHead}

-- | Assumes this slot already contained an 'a'.
update :: (V.Unbox a)
  => SparseVector s a
  -> Int
  -> a
  -> ST s ()
update SparseVector {..} i val = V.write _svVector i (Right val)

-- | Assumes this slot contains an 'a'. No error if this assumption is false.
delete :: (V.Unbox a)
  => SparseVector s a
  -> Int
  -> ST s ()
delete SparseVector {..} i = do
  emptyHead <- readSTRef _svEmptyHead
  writeSTRef _svEmptyHead i
  V.write _svVector i (Left emptyHead)

-- | Assumes this slot contains an 'a'. Errors if this assumption is false.
safeDelete :: (V.Unbox a)
  => SparseVector s a
  -> Int
  -> ST s ()
safeDelete SparseVector {..} i = do
  val <- V.read _svVector i
  case val of
    Left _ -> error $ "i=" <> show i <> " points at an empty slot"
    Right _ -> do
      emptyHead <- readSTRef _svEmptyHead
      writeSTRef _svEmptyHead i
      V.write _svVector i (Left emptyHead)

-- | Assumes the 'SparseVector' is not full.
append :: (V.Unbox a)
  => SparseVector s a
  -> a
  -> ST s Int -- ^ the index of the newly-added object
append SparseVector {..} val = do
  emptyHead <- readSTRef _svEmptyHead
  emptyHead' <- V.read _svVector emptyHead
  case emptyHead' of
    Right _ -> error $ "emptyHead=" <> show emptyHead <> " points at a filled slot"
    Left emptyHead' -> do
      -- Write to the old emptyHead. Update emptyHead to the next empty slot.
      V.write _svVector emptyHead $ Right val
      writeSTRef _svEmptyHead emptyHead'
      return emptyHead

-- | Assumes this slot contains an 'a'.
read :: (V.Unbox a)
  => SparseVector s a
  -> Int
  -> ST s a
read SparseVector {..} i = do
  val <- V.read _svVector i
  case val of
    Left _    -> error $ "i=" <> show i <> " points at an empty slot."
    Right val -> return val

-- | Errors if the 'SparseVector' is messed up.
validate :: (V.Unbox a)
  => SparseVector s a
  -> ST s ()
validate SparseVector {..} = do
  empties <- (length . filter isLeft) <$> mapM (V.read _svVector) [0 .. (n - 1)]
  emptyHead <- readSTRef _svEmptyHead
  let loop emptyHead =
        if emptyHead == n
          then return 1
          else do
            emptyHead' <- V.read _svVector emptyHead
            case emptyHead' of
              Left emptyHead' -> (1 +) <$> loop emptyHead'
              Right _ ->
                error $ "empty slot points to filled slot i=" <> show emptyHead
  empties' <- loop emptyHead
  if empties /= empties'
    then error $
         "stack contains " <> show empties' <> " slots, but there are " <>
         show empties <>
         " empty slots"
    else return ()
  where
    n = V.length _svVector
