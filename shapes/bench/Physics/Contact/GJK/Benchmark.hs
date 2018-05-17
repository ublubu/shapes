{-# LANGUAGE MagicHash #-}

module Physics.Contact.GJK.Benchmark where

import Physics.Linear
import Physics.Contact.GJK
import Physics.Contact.ConvexHull
import Physics.Contact.Benchmark (makeOptBox)

testSimplex1 =
  closestSimplex (makeOptBox 0 0 1.9 1.9) (P2 $ V2 1.0## 1.0##)

testSimplex2 =
  closestSimplex (makeOptBox 0 0 1.9 1.9) (P2 $ V2 0.0## 1.0##)

testSimplex3 =
  closestSimplex (makeOptBox 0 0 1.9 1.9) (P2 $ V2 0.0## 0.9##)
