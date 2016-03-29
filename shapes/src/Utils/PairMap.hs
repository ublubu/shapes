module Utils.PairMap where

import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Utils.Utils

type Key = (Int, Int)
type PairMap a = IM.IntMap (IM.IntMap a)

findOrInsertPair :: Key -> a -> PairMap a -> (Maybe a, PairMap a)
findOrInsertPair (i, j) x t = (mx', t')
  where mtt = IM.lookup i t
        (mx', tt') = case mtt of Just tt -> findOrInsert j x tt
                                 Nothing -> (Nothing, IM.insert j x IM.empty)
        t' = case mx' of Just _ -> t -- x was not inserted
                         Nothing -> IM.insert i tt' t -- x was inserted, insert updated tt'

findOrInsertPair' :: Key -> a -> PairMap a -> (a, PairMap a)
findOrInsertPair' ij x t = (fromMaybe x mx, t')
  where (mx, t') = findOrInsertPair ij x t

insertPair :: Key -> a -> PairMap a -> PairMap a
insertPair (i, j) x t = IM.insert i tt' t
  where mtt = IM.lookup i t
        tt' = case mtt of Just tt -> IM.insert j x tt
                          Nothing -> IM.insert j x IM.empty

lookupPair :: Key -> PairMap a -> Maybe a
lookupPair (i, j) t = do
  tt <- IM.lookup i t
  IM.lookup j tt

keys :: PairMap a -> [Key]
keys t = foldl f [] (fmap (over _2 IM.keys) (IM.toList t))
  where f ks (a, bs) = foldl (g a) ks bs
        g a ks b = (a, b):ks
