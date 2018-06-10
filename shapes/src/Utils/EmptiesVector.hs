{-# LANGUAGE RecordWildCards #-}

module Utils.EmptiesVector where

import           Prelude                     hiding (length, read)
import qualified Prelude as P

import           Control.Monad.ST
import           Data.Monoid
import           Data.STRef
import qualified Data.Vector.Unboxed.Mutable as V

import           Utils.UnboxEither

{- |
Keeps track of 'empty' slots.
-}
data EmptiesVector s =
  EmptiesVector
  { _evVector    :: V.MVector s Int
  , _evEmptyHead :: STRef s Int
  }

sentinel :: Int
sentinel = -1

new ::
     Int -- ^ capacity
  -> ST s (EmptiesVector s)
new n = do
  vec <- V.unsafeNew n
  emptyHead <- newSTRef 0
  let f i =
        let j = i + 1
        in V.write vec i $
           if j < n
             then j
             else sentinel
  mapM_ f [0 .. n]
  return EmptiesVector {_evVector = vec, _evEmptyHead = emptyHead}

-- | Assumes this slot is not already empty. No error if this assumption is false.
delete :: EmptiesVector s -> Int -> ST s ()
delete EmptiesVector {..} i = do
  emptyHead <- readSTRef _evEmptyHead
  -- 'i' is the new emptyHead
  writeSTRef _evEmptyHead i
  V.write _evVector i emptyHead

-- | Assumes this slot is not already empty. Errors if this assumption is false.
safeDelete :: EmptiesVector s -> Int -> ST s ()
safeDelete EmptiesVector {..} i = do
  val <- V.read _evVector i
  if val == sentinel
    then error $ "i=" <> show i <> " points at an empty slot"
    else do
      emptyHead <- readSTRef _evEmptyHead
      -- 'i' is the new emptyHead
      writeSTRef _evEmptyHead i
      V.write _evVector i emptyHead

-- | Assumes the 'SparseVector' is not full.
append :: EmptiesVector s -> ST s Int -- ^ the index of the newly-added object
append EmptiesVector {..} = do
  emptyHead <- readSTRef _evEmptyHead
  emptyHead' <- V.read _evVector emptyHead
  if emptyHead' == sentinel
    then error $ "emptyHead=" <> show emptyHead <> " points at a filled slot"
      -- Write to the old emptyHead. Update emptyHead to the next empty slot.
    else do
      V.write _evVector emptyHead sentinel
      writeSTRef _evEmptyHead emptyHead'
      return emptyHead

-- | Is this slot occupied?
read :: EmptiesVector s
  -> Int
  -> ST s Bool
read EmptiesVector {..} i = do
  val <- V.read _evVector i
  return $ val == sentinel

-- | Errors if the 'EmptiesVector' is messed up.
validate :: EmptiesVector s -> ST s ()
validate EmptiesVector {..} = do
  empties <-
    (P.length . filter (/= sentinel)) <$> mapM (V.read _evVector) [0 .. (n - 1)]
  emptyHead <- readSTRef _evEmptyHead
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

forEach :: EmptiesVector s
  -> (Int -> ST s ())
  -> ST s ()
forEach empties@EmptiesVector {..} f_ = mapM_ f [0 .. (length empties - 1)]
  where
    f i = do
      nonempty <- read empties i
      if nonempty
        then f i
        else return ()
