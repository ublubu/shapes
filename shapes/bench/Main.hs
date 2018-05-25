{- |
Main. I just use this for benchmarking and profiling different scenes and solver configurations.
-}
module Main where

import           Control.DeepSeq
import           Criterion.Main
import           Data.Proxy
import qualified Physics.Scenes.Stacks        as Stacks
import           Utils.Utils

import qualified Physics.Broadphase.Benchmark as BB
import qualified Physics.Constraint.Benchmark as BC
import qualified Physics.Contact.Benchmark    as BC'

import qualified Physics.Engine.Main          as OM

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

main :: IO ()
main =
  defaultMain
    [ bench "opt updateWorld 10" $
      nf (OM.runWorld 0.01 (Stacks.makeScene (30, 30) 0 ())) 10
    ]
--main = print . rnf $ OM.runWorld 0.01 (Stacks.makeScene (15, 15) 0 OM.engineP) 200
--main = BB.main
