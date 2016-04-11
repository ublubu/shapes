{-# LANGUAGE MagicHash #-}

module Physics.Linear.Opt.Convert where

import GHC.Types (Double(D#))

import qualified Linear.Affine as L
import qualified Linear.V2 as L

import Physics.Linear.Opt

toLV2 :: V2 -> L.V2 Double
toLV2 = (\[x, y] -> L.V2 x y) . toListV2

toLP2 :: P2 -> L.Point L.V2 Double
toLP2 (P2 v) = L.P . toLV2 $ v

fromLV2 :: L.V2 Double -> V2
fromLV2 (L.V2 (D# x) (D# y)) = V2 x y
