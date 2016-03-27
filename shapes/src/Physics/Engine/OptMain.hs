{-# LANGUAGE BangPatterns #-}

module Physics.Engine.OptMain where

import Control.Lens
import Data.Proxy
import Physics.Broadphase.OptAabb
import Physics.World.OptObject
import qualified Physics.Solvers.OptSolvers as S
import Physics.World.OptWorld
import Physics.WorldSolver
import Utils.Utils

import Physics.Engine.Opt
import Physics.Scenes.Scene

engine :: Proxy Engine
engine = Proxy

type EngineState = SP (World WorldObj) (S.ContactSolverState WorldObj)

updateWorld :: Scene Engine -> Double -> EngineState -> EngineState
updateWorld scene dt (SP w s) = SP w''' s'
  where w1 = applyExternals (scene ^. scExts) dt w
        maxSolverIterations = 3
        worldChanged = const . const $ True
        ks = culledKeys w1
        (w', s') = wsolve' S.contactSolver' worldChanged maxSolverIterations ks worldPair w1 dt s
        w'' = advanceWorld dt w'
        w''' = w'' & worldObjs %~ fmap updateShape

stepWorld :: Scene Engine -> Int -> EngineState -> EngineState
stepWorld _ 0 !s = s
stepWorld scene !x !s = stepWorld scene (x - 1) $ updateWorld scene 0.01 s

defaultInitialState :: Scene Engine -> EngineState
defaultInitialState scene =
  SP (scene ^. scWorld) (S.emptyContactSolverState (scene ^. scContactBeh))
