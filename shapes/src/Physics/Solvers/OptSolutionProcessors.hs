module Physics.Solvers.OptSolutionProcessors where

import Physics.Constraint.OptConstraint
import Physics.Contact.OptContact
import Physics.Solver.OptContactSolver
import Physics.Constraints.OptFriction

type SolutionProcessor' = Double -> ConstraintResult -> (Double, ConstraintResult)

simple' :: SolutionProcessor'
simple' sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = lagr

positive' :: SolutionProcessor'
positive' sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = max lagr (-sln)

toSP :: SolutionProcessor' -> SolutionProcessor a
toSP f sln r _ = (fst <$> x, snd <$> x)
  where x = f <$> sln <*> r

simple :: SolutionProcessor a
simple = toSP simple'

positive ::  SolutionProcessor a
positive = toSP positive'

contact :: (Contactable a) => SolutionProcessor a
contact (ContactResult snp sfr) (ContactResult rnp rfr) ab = (ContactResult snp' sfr', ContactResult rnp' rfr')
  where (snp', rnp') = positive' snp rnp
        (sfr', rfr') = clampAbs (u * snp') sfr rfr
        u = pairMu ab

clampAbs :: Double -> Double -> ConstraintResult -> (Double, ConstraintResult)
clampAbs maxThresh sln (lagr, c) = (sln'', (lagr', c))
  where minThresh = -maxThresh
        sln'' | sln' > maxThresh = maxThresh
              | sln' < minThresh = minThresh
              | otherwise = sln'
        sln' = sln + lagr
        lagr' = sln'' - sln

