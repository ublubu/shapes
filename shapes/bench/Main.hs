{- |
Main. I just use this for benchmarking and profiling different scenes and solver configurations.
-}
module Main where

import           Control.DeepSeq
import           Control.Monad.ST
import           Criterion.Main
import           Data.Proxy
import qualified Physics.Scenes.Stacks        as Stacks
import           Utils.Utils

import qualified Physics.Broadphase.Benchmark as BB
import qualified Physics.Constraint.Benchmark as BC
import qualified Physics.Contact.Benchmark    as BC'

import qualified Physics.Engine.Main          as OM

bench0 :: ST s ()
bench0 = do
  s <- Stacks.makeScene (30, 30) 0 ()
  OM.runWorld 0.01 s 10

-- | 228ms
main :: IO ()
main = defaultMain [ bench "opt updateWorld 10" $ nfIO (stToIO bench0)]
--main = BB.main
