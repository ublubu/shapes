{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Physics.Engine.Simple.Main ( module Physics.Engine.Simple.Main
                                  , module Physics.Engine.Simple
                                  ) where

import Control.Lens

import Physics.Broadphase.Simple.Aabb
import Physics.World.Simple.Object
import Physics.Contact.Simple (ContactBehavior(..))
import qualified Physics.Solvers.Simple as S
import Physics.World.Simple
import Physics.Solver.World

import Physics.Engine.Simple
import Physics.Scenes.Scene

data EngineState = EngineState { _esWorld :: !(World (WorldObj Double))
                               , _esBeh :: !(ContactBehavior Double)
                               , _esExts :: ![External Double (WorldObj Double)]
                               , _esSolver :: !(S.ContactSolverState Double (WorldObj Double))
                               }
makeLenses ''EngineState

updateWorld :: Double -> EngineState -> EngineState
updateWorld dt eState@EngineState{..} = eState { _esWorld = w''', _esSolver = s' }
  where w1 = applyExternals _esExts dt _esWorld
        maxSolverIterations = 3
        worldChanged = const . const $ True
        ks = culledKeys w1
        (w', s') = wsolve' S.contactSolver' worldChanged maxSolverIterations ks worldPair w1 dt _esSolver
        w'' = advanceWorld dt w'
        w''' = w'' & worldObjs %~ fmap updateShape

stepWorld :: Int -> EngineState -> EngineState
stepWorld 0 !s = s
stepWorld !x !s = stepWorld (x - 1) $ updateWorld 0.01 s

defaultInitialState :: Scene SimpleEngine -> EngineState
defaultInitialState Scene{..} =
  EngineState _scWorld _scContactBeh _scExts (S.emptyContactSolverState _scContactBeh)

defaultInitialStateOpt :: Scene SimpleEngine -> EngineState
defaultInitialStateOpt Scene{..} =
  EngineState _scWorld _scContactBeh _scExts (S.emptyOptContactSolverState _scContactBeh)

runWorld :: Scene SimpleEngine -> Int -> World (WorldObj Double)
runWorld scene steps =
  _esWorld . stepWorld steps $ defaultInitialState scene
