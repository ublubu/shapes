{-# LANGUAGE MagicHash #-}

module Physics.Contact.Benchmark where

import GHC.Types (Double(D#))

import Criterion.Main
import Data.Vector (toList)
import Linear.V2
import Linear.V

import qualified Physics.ConvexHull as C
import qualified Physics.SAT as S
import qualified Physics.Contact.OptConvexHull as OC
import qualified Physics.Contact.OptSAT as OS
import qualified Physics.Linear as L
import qualified Physics.Constraint.OptLinear as OL
import qualified Physics.Transform as T

import Utils.Utils

makeBox :: Double -> Double -> Double -> Double -> C.ConvexHull Double
makeBox x y w h =
  C.listToHull $ T.transform (T.translateTransform (V2 x y)) vertices
  where vertices = C.rectangleVertices w h

benchy :: Benchmark
benchy = bench "contact" $ whnf (uncurry S.contact) (makeBox 0 0 4 4, makeBox 1 3 2 2)

main :: IO ()
main = defaultMain [benchy]
