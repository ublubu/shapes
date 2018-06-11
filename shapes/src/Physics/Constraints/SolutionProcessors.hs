{- |
SolutionProcessors take incremental and accumulated constraint solutions
and use rules to determine what incremental impulse (constraint solution) to apply to an object.

For example, a solution processor might enforce that the total accumulated impulse is nonnegative.
-}
module Physics.Constraints.SolutionProcessors where

import           Physics.Constraint
import           Physics.Constraints.Types

wrapProcessor :: (Lagrangian -> Lagrangian -> Lagrangian)
              -> Lagrangian
              -> Lagrangian
              -> Processed Lagrangian
wrapProcessor f cached_l new_l =
  Processed {_processedToCache = cached_l', _processedToApply = apply_l}
  where
    cached_l' = cached_l + apply_l
    apply_l = f cached_l new_l

-- | Apply the entire newly-calculated Lagrangian.
simple :: Lagrangian -> Lagrangian -> Processed Lagrangian
simple = wrapProcessor (flip const)

{- |
Ensure that the sum of the applied Lagrangians is always positive.
This is useful if a constraint should only apply impulse in one direction.
e.g. Non-penetration should resist penetration but have no effect on separation.
-}
positive :: Lagrangian -> Lagrangian -> Processed Lagrangian
positive = wrapProcessor (\cached_l new_l -> max new_l (-cached_l))

{- |
Ensure that the magnitude of the sum of the applied Lagrangians never exceeds a threshold.
This is useful if there's a limit to the force a constraint can apply.
e.g. Friction resists sliding motion, but this force is limited.
-}
clampAbs :: Lagrangian -> Lagrangian -> Lagrangian -> Processed Lagrangian
clampAbs maxThresh cached new =
  Processed {_processedToCache = accum_l', _processedToApply = apply_l}
  where
    accum_l = cached + new
    accum_l'
      | accum_l > maxThresh = maxThresh
      | accum_l < minThresh = minThresh
      | otherwise = accum_l
    apply_l = accum_l' - cached
    minThresh = -maxThresh
