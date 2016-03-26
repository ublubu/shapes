{-# LANGUAGE PatternSynonyms, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}

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
import Debug.Trace

import Physics.Scenes.Scene
import Physics.Scenes.Stacks

import qualified Physics.BenchGeometry as BG
import qualified BenchLinear as BL
import qualified Physics.Constraint.Benchmark as BC
import qualified Physics.Contact.Benchmark as BC'
import qualified Physics.Broadphase.Benchmark as BB

type EngineState n = SP (World (WorldObj n)) (S.ContactSolverState n (WorldObj n))

fst' :: SP a b -> a
fst' (SP x _) = x

updateWorld :: (Epsilon n, Floating n, Ord n) => Scene n (WorldObj n) -> n -> EngineState n -> EngineState n
updateWorld scene dt (SP w s) = SP w''' s'
  where w1 = applyExternals (scene ^. scExts) dt w
        maxSolverIterations = 3
        worldChanged = const . const $ True
        ks = culledKeys w1
        (w', s') = wsolve' S.contactSolver' worldChanged maxSolverIterations ks worldPair w1 dt s
        w'' = advanceWorld dt w'
        w''' = w'' & worldObjs %~ fmap updateShape

stepWorld :: Int -> EngineState Double -> EngineState Double
stepWorld 0 !s = s
stepWorld !x !s = stepWorld (x - 1) $ updateWorld scene'' 0.01 s

initialState :: EngineState Double
initialState =
  SP (scene ^. scWorld) (S.emptyContactSolverState (scene ^. scContactBeh))
  where scene = scene''' :: Scene Double (WorldObj Double)

initialStateOpt :: EngineState Double
initialStateOpt =
  SP (scene ^. scWorld) (S.emptyOptContactSolverState (scene ^. scContactBeh))
  where scene = scene''' :: Scene Double (WorldObj Double)

main :: IO ()
-- 1 frame: 7.2ms (is this a misevaluation due to laziness?)
-- 10 frames: 219ms
-- 100 frames: 2.0s
-- 400 frames: 8.2s
--main = defaultMain [ bench "updateWorld 10" $ whnf (show . fst' . stepWorld 10) s0 ]
  --where s0 = stepWorld 10 initialState
--main = do
--  (SP x y) <- return $ stepWorld 200 initialState
--  return ()
main = BB.main
