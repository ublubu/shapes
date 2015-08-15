module Physics.SolutionProcessors where

import Control.Applicative
import Physics.Constraint
import Physics.ContactSolver
import Physics.Friction

type SolutionProcessor' n = n -> ConstraintResult n -> (n, ConstraintResult n)

simple' :: (Fractional n) => SolutionProcessor' n
simple' sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = lagr

positive' :: (Fractional n, Ord n) => SolutionProcessor' n
positive' sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = max lagr (-sln)

toSP :: SolutionProcessor' n -> SolutionProcessor n a
toSP f sln r _ = (fst <$> x, snd <$> x)
  where x = f <$> sln <*> r

simple :: (Fractional n) => SolutionProcessor n a
simple = toSP simple'

positive :: (Fractional n, Ord n) => SolutionProcessor n a
positive = toSP positive'

contact :: (Contactable n a, Fractional n, Ord n) => SolutionProcessor n a
contact (ContactResult snp sfr) (ContactResult rnp rfr) ab = (ContactResult snp' sfr', ContactResult rnp' rfr')
  where (snp', rnp') = positive' snp rnp
        (sfr', rfr') = clampAbs (u * snp') sfr rfr
        u = pairMu ab

clampAbs :: (Num n, Ord n) => n -> n -> ConstraintResult n -> (n, ConstraintResult n)
clampAbs maxThresh sln (lagr, c) = (sln'', (lagr', c))
  where minThresh = -maxThresh
        sln'' | sln' > maxThresh = maxThresh
              | sln' < minThresh = minThresh
              | otherwise = sln'
        sln' = sln + lagr
        lagr' = sln'' - sln

