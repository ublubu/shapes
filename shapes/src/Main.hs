module Main where

import Criterion.Main
import Utils.Utils
import qualified Physics.Scenes.Stacks as Stacks

import qualified Physics.BenchGeometry as BG
import qualified BenchLinear as BL
import qualified Physics.Constraint.Benchmark as BC
import qualified Physics.Contact.Benchmark as BC'
import qualified Physics.Broadphase.Benchmark as BB

import qualified Physics.Engine.SimpleMain as SM

-- TODO: use something other than show to ensure evaluation of the world
main :: IO ()
main = defaultMain [ bench "updateWorld 10" $ whnf (show . _spFst . f) s0 ]
  where s0 = SM.stepWorld scene 10 $ SM.defaultInitialState scene
        f = SM.stepWorld scene 10
        scene = Stacks.scene''' SM.engine
--main = do
--  (SP x y) <- return $ stepWorld 200 initialState
--  return ()
--main = BB.main
