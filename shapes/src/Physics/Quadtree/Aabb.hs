{-# LANGUAGE MagicHash #-}

module Physics.Quadtree.Aabb where

import GHC.Prim (Double#, (>##), (<##))
import GHC.Types (isTrue#)

import Physics.Broadphase.Aabb
import Physics.Linear
import Physics.Quadtree

data BoundsCompare = Below | Within | Above

boundsCompare :: Double# -> Bounds -> BoundsCompare
boundsCompare x (Bounds xmin xmax)
  | isTrue# (xmin >## x) = Below
  | isTrue# (xmax <## x) = Above
  | otherwise = Within

instance QObj Aabb where
  qTest (V2 x y) (Aabb xBounds yBounds) =
    f xc yc
    where xc = boundsCompare x xBounds
          yc = boundsCompare y yBounds

          f Below Below = qChild Q1 -- center is Below/Below the Aabb
          f Above Below = qChild Q2
          f Above Above = qChild Q3
          f Below Above = qChild Q4
          f _ _ = QParent ()

testBox = makeAabb 0.1 0.9 0.1 0.9

testQTree = emptyQTree (makeV2 1 0, makeV2 1 1)

testResult = qContained testBox testQTree
