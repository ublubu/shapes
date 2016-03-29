module Physics.Constraint.Simple.Error where

import Control.Lens
import Linear.Metric
import Physics.Constraint.Simple
type ErrorMetric a = PhysicalObj a -> PhysicalObj a -> a
type ErrorGone a = PhysicalObj a -> PhysicalObj a -> Bool

lensDifference :: Getting a s a -> (a -> a -> t) -> s -> s -> t
lensDifference l diff o o' = diff (o ^. l) (o' ^. l)

absDiff a b = abs (a - b)

normDiff a b = norm (a - b)

sqLinVelChange :: (Num a) => ErrorMetric a
sqLinVelChange = lensDifference physObjVel qd

linVelChange :: (Floating a) => ErrorMetric a
linVelChange = lensDifference physObjVel (\a b -> norm (a - b))

rotVelChange :: (Num a) => ErrorMetric a
rotVelChange = lensDifference physObjRotVel (\a b -> abs (a - b))

energyChange :: (Fractional a) => ErrorMetric a
energyChange a b = absDiff (energy a) (energy b)

energy :: (Fractional a) => PhysicalObj a -> a
energy (PhysicalObj v w _ _ (m, i)) = e m (quadrance v) + e i (w ^ 2)
  where e m' sq_v = (m' * sq_v) / 2

velocityThreshold :: (Floating a, Ord a) => a -> a -> ErrorGone a
velocityThreshold lin rot a b = (linVelChange a b < lin) && (rotVelChange a b < rot)
