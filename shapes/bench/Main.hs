{- |
Main. I just use this for benchmarking and profiling different scenes and solver configurations.
-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Proxy
import Utils.Utils
import qualified Physics.Scenes.Stacks as Stacks

import qualified Physics.Constraint.Benchmark as BC
import qualified Physics.Contact.Benchmark as BC'
import qualified Physics.Broadphase.Benchmark as BB

import qualified Physics.Engine.Main as OM

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
-- 10 frames 15x15
-- 770ms simple 3x
-- 310ms opt+hashtable 3x
--  80ms opt+vector 2x
main = defaultMain [bench "opt updateWorld 10" $ nf (OM.runWorld 0.01 (Stacks.makeScene (30, 30) 0 OM.engineP ())) 10]
--main = print . rnf $ OM.runWorld 0.01 (Stacks.makeScene (15, 15) 0 OM.engineP) 200
--main = BB.main
