{-# LANGUAGE PatternSynonyms, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Main where

import Control.Monad
import Control.Lens
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
import Debug.Trace

import Physics.Scenes.Scene
import Physics.Scenes.Stacks

import qualified Physics.BenchGeometry as BG

updateWorld :: (Epsilon n, Floating n, Ord n) => Scene n (WorldObj n) -> n -> (World (WorldObj n), State n (Cache n (WorldObj n))) -> (World (WorldObj n), State n (Cache n (WorldObj n)))
updateWorld scene dt (w, s) = (w''', s')
  where w1 = applyExternals (scene ^. scExts) dt w
        maxSolverIterations = 2
        worldChanged = const . const $ True
        solver = S.contactSolver' (scene ^. scContactBeh)
        ks = culledKeys w1
        (w', s') = wsolve' solver worldChanged maxSolverIterations ks worldPair w1 dt s
        w'' = advanceWorld dt w'
        w''' = w'' & worldObjs %~ fmap updateShape

worlds :: [(World (WorldObj Double), State Double (Cache Double (WorldObj Double)))]
worlds = iterate (updateWorld scene'' 0.01) ((scene'' :: Scene Double (WorldObj Double)) ^. scWorld, emptyState)

main :: IO ()
main = print world
  where (world, _) = (worlds !! 1000)
