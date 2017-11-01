{- |
SolutionProcessors take incremental and accumulated constraint solutions
and use rules to determine what incremental impulse (constraint solution) to apply to an object.

For example, a solution processor might enforce that the total accumulated impulse is nonnegative.
-}
module Physics.Constraints.SolutionProcessors where

import Physics.Constraint
import Physics.Constraints.Types

wrapProcessor :: (Lagrangian -> Lagrangian -> Lagrangian)
              -> Lagrangian
              -> Lagrangian
              -> Processed Lagrangian
wrapProcessor f cached_l new_l =
  Processed { _processedToCache = cached_l'
            , _processedToApply = apply_l }
  where cached_l' = cached_l + apply_l
        apply_l = f cached_l new_l
{-# INLINE wrapProcessor #-}

simple :: Lagrangian -> Lagrangian -> Processed Lagrangian
simple = wrapProcessor (flip const)
{-# INLINE simple #-}

positive :: Lagrangian -> Lagrangian -> Processed Lagrangian
positive = wrapProcessor (\cached_l new_l -> max new_l (-cached_l))
{-# INLINE positive #-}

clampAbs :: Lagrangian -> Lagrangian -> Lagrangian -> Processed Lagrangian
clampAbs maxThresh cached new =
  Processed { _processedToCache = accum_l'
            , _processedToApply = apply_l }
  where accum_l = cached + new
        accum_l' | accum_l > maxThresh = maxThresh
                 | accum_l < minThresh = minThresh
                 | otherwise = accum_l
        apply_l = accum_l' - cached
        minThresh = -maxThresh
{-# INLINE clampAbs #-}
