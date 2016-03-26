{-# LANGUAGE BangPatterns #-}

module Physics.Engine.SimpleMain where

import Control.Lens
import Data.Proxy
import Physics.Broadphase
import Physics.Object
import qualified Physics.Solvers as S
import Physics.World
import Physics.WorldSolver
import Utils.Utils

import Physics.Engine.Simple
import Physics.Scenes.Scene

engine :: Proxy SimpleEngine
engine = Proxy

type EngineState = SP (World (WorldObj Double)) (S.ContactSolverState Double (WorldObj Double))

updateWorld :: Scene SimpleEngine -> Double -> EngineState -> EngineState
updateWorld scene dt (SP w s) = SP w''' s'
  where w1 = applyExternals (scene ^. scExts) dt w
        maxSolverIterations = 3
        worldChanged = const . const $ True
        ks = culledKeys w1
        (w', s') = wsolve' S.contactSolver' worldChanged maxSolverIterations ks worldPair w1 dt s
        w'' = advanceWorld dt w'
        w''' = w'' & worldObjs %~ fmap updateShape

stepWorld :: Scene SimpleEngine -> Int -> EngineState -> EngineState
stepWorld _ 0 !s = s
stepWorld scene !x !s = stepWorld scene (x - 1) $ updateWorld scene 0.01 s

defaultInitialState :: Scene SimpleEngine -> EngineState
defaultInitialState scene =
  SP (scene ^. scWorld) (S.emptyContactSolverState (scene ^. scContactBeh))

defaultInitialStateOpt :: Scene SimpleEngine -> EngineState
defaultInitialStateOpt scene =
  SP (scene ^. scWorld) (S.emptyOptContactSolverState (scene ^. scContactBeh))
