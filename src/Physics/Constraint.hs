{-# LANGUAGE DataKinds #-}

module Physics.Constraint where

import Data.Vector
import Data.Maybe
import Linear.V
import Linear.V4
import Linear.Vector
import Linear.Matrix
import Linear.Metric
import Physics.Linear

type MassInertia a = (a, a)

massM :: (Num a) => MassInertia a -> MassInertia a -> M66 a
massM (ma, ia) (mb, ib) = listToV [ listToV [ma, 0, 0, 0, 0, 0]
                                  , listToV [0, ma, 0, 0, 0, 0]
                                  , listToV [0, 0, ia, 0, 0, 0]
                                  , listToV [0, 0, 0, mb, 0, 0]
                                  , listToV [0, 0, 0, 0, mb, 0]
                                  , listToV [0, 0, 0, 0, 0, ib] ]

invMassM :: (Fractional a) => MassInertia a -> MassInertia a -> M66 a
invMassM (_ma, _ia) (_mb, _ib) = listToV [ listToV [ma, 0, 0, 0, 0, 0]
                                         , listToV [0, ma, 0, 0, 0, 0]
                                         , listToV [0, 0, ia, 0, 0, 0]
                                         , listToV [0, 0, 0, mb, 0, 0]
                                         , listToV [0, 0, 0, 0, mb, 0]
                                         , listToV [0, 0, 0, 0, 0, ib] ]
  where ma = 1 / _ma
        ia = 1 / _ia
        mb = 1 / _mb
        ib = 1 / _ib

lagrangian :: (Num a, Fractional a) => V6 a -> V6 a -> a -> M66 a -> a
lagrangian j v b im = (-(j `dot` v + b)) / ((j *! im) `dot` j)
