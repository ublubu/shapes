module Physics.SolutionProcessors where

import Physics.ConstraintSolver

simple :: (Fractional n) => SolutionProcessor n
simple sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = lagr

positive :: (Fractional n, Ord n) => SolutionProcessor n
positive sln (lagr, c) = (sln', (lagr', c))
  where sln' = sln + lagr'
        lagr' = max lagr (-sln)
