module Physics.Solvers.Opt.SolutionProcessors where

import Physics.Constraint.Opt
import Physics.Constraints.Opt.Contact
import Physics.Contact.Opt
import Physics.Constraints.Opt.Friction

-- apply rules to total constraint impulse
--
-- (previously accumulated impulse) ->
-- (current iteration's sln) ->
-- (objects) ->
-- (new accumulated impulse, incremental sln to apply)
--
type SolutionProcessor = ContactSolution -> ContactSolution -> (Double, Double) -> (ContactSolution, ContactSolution)
type SolutionProcessor' = ConstraintResult -> ConstraintResult -> (ConstraintResult, ConstraintResult)
type SolutionProcessor'' = Double -> ConstraintResult -> (Double, ConstraintResult)


simple' :: SolutionProcessor''
simple' sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = lagr
{-# INLINE simple' #-}

positive' :: SolutionProcessor''
positive' sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = max lagr (-sln)
{-# INLINE positive' #-}

wrapSlnProc' :: SolutionProcessor''
             -> SolutionProcessor'
wrapSlnProc' f (lagrCache, _) slnApply =
  ((lagrCache', jApply'), slnApply')
  where (lagrCache', slnApply'@(_, jApply')) = f lagrCache slnApply
{-# INLINE wrapSlnProc' #-}

contactSlnProc :: SolutionProcessor
contactSlnProc (ContactSolution snp sfr) (ContactSolution rnp rfr) ab =
  (ContactSolution snp' sfr', ContactSolution rnp' rfr')
  where (snp', rnp') = wrapSlnProc' positive' snp rnp
        (sfr', rfr') = wrapSlnProc' (clampAbs (u * fst snp')) sfr rfr
        u = pairMu ab
{-# INLINE contactSlnProc #-}

clampAbs :: Double -> Double -> ConstraintResult -> (Double, ConstraintResult)
clampAbs maxThresh sln (lagr, c) = (sln'', (lagr', c))
  where minThresh = -maxThresh
        sln'' | sln' > maxThresh = maxThresh
              | sln' < minThresh = minThresh
              | otherwise = sln'
        sln' = sln + lagr
        lagr' = sln'' - sln
{-# INLINE clampAbs #-}
