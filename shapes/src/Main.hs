module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Proxy
import Utils.Utils
import qualified Physics.Scenes.Stacks as Stacks

import qualified Physics.Constraint.Benchmark as BC
import qualified Physics.Contact.Benchmark as BC'
import qualified Physics.Broadphase.Benchmark as BB

import qualified Physics.Engine.Simple.Main as SM
import qualified Physics.Engine.Opt.Main as OM

benchy :: (Num n, NFData world)
       => String
       -> Proxy e
       -> (Proxy e -> scene)
       -> (scene -> SP world cache)
       -> (scene -> n -> SP world cache -> SP world cache)
       -> Benchmark
benchy prefix p sceneGen stateGen stepGen =
  bench (prefix ++ " updateWorld 10") $ nf (_spFst . f) s0
  where s0 = stepGen scene 100 $ stateGen scene
        f = stepGen scene 10
        scene = sceneGen p

-- TODO: use something other than show to ensure evaluation of the world
main :: IO ()
main = defaultMain [
  benchy "simple" SM.engine (Stacks.makeScene (10, 10)) SM.defaultInitialState SM.stepWorld,
  benchy "opt" OM.engine (Stacks.makeScene (10, 10)) OM.defaultInitialState OM.stepWorld ]
--main = do
--  (SP x y) <- return $ stepWorld 200 initialState
--  return ()
--main = BB.main
