{-# LANGUAGE PatternSynonyms, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Main where

import Control.Monad
import Control.Lens
import Criterion.Main
import GHC.Word
import Linear.Epsilon
import Linear.V2
import Physics.Broadphase
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.ContactSolver
import Physics.Contact
import Physics.Object
import qualified Physics.Solvers as S
import Physics.Transform
import Physics.World hiding (testWorld)
import Physics.WorldSolver
import Utils.Utils
import qualified Physics.TestBenchGeometry as TBG

import Physics.Scenes.Scene
import Physics.Scenes.Stacks

updateWorld :: (Contactable n a, Epsilon n, Floating n, Ord n) => Scene n a -> n -> (World a, State n (Cache n a)) -> (World a, State n (Cache n a))
updateWorld scene dt (w, s) = (advanceWorld dt w', s')
  where w1 = applyExternals (scene ^. scExts) dt w
        maxSolverIterations = 2
        worldChanged = const . const $ True
        solver = S.contactSolver' (scene ^. scContactBeh)
        (w', s') = wsolve' solver worldChanged maxSolverIterations (culledKeys w1) worldPair w1 dt s

worlds :: [(World (WorldObj Double), State Double (Cache Double (WorldObj Double)))]
worlds = iterate (updateWorld scene'' 0.01) ((scene'' :: Scene Double (WorldObj Double)) ^. scWorld, emptyState)

main :: IO ()
main = defaultMain [TBG.benchy]
  where (world, _) = (worlds !! 1000)
