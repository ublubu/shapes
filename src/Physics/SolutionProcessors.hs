module Physics.SolutionProcessors where

import Control.Applicative
import Physics.Constraint
import Physics.ContactSolver

type SolutionProcessor' n = n -> ConstraintResult n -> (n, ConstraintResult n)

simple' :: (Fractional n) => SolutionProcessor' n
simple' sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = lagr

positive' :: (Fractional n, Ord n) => SolutionProcessor' n
positive' sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = max lagr (-sln)

toSP :: SolutionProcessor' n -> SolutionProcessor n
toSP f sln r = (fst <$> x, snd <$> x)
  where x = f <$> sln <*> r

simple :: (Fractional n) => SolutionProcessor n
simple = toSP simple'

positive :: (Fractional n, Ord n) => SolutionProcessor n
positive = toSP positive'
