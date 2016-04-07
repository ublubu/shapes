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
-- 10 frames 15x15
-- 770ms simple 3x
-- 310ms opt+hashtable 3x
--  80ms opt+vector 2x
--main = defaultMain [
  --bench "simple updateWorld 10" $ nf (SM.runWorld (Stacks.makeScene (15, 15) 0 SM.engineP)) 100,
  --bench "opt updateWorld 10" $ nf (OM.runWorld (Stacks.makeScene (15, 15) 0 OM.engineP)) 100]
main = print . rnf $ OM.runWorld (Stacks.makeScene (15, 15) 0 OM.engineP) 200
--main = BB.main
