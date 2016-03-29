{-# LANGUAGE MagicHash #-}

module Physics.Contact.Benchmark where

import GHC.Types (Double(D#))

import Criterion.Main
import Linear.V2

import qualified Physics.Contact.Simple.ConvexHull as C
import qualified Physics.Contact.Simple.SAT as S
import qualified Physics.Contact.Opt.ConvexHull as OC
import qualified Physics.Contact.Opt.SAT as OS
import qualified Physics.Linear.Opt as OL
import qualified Physics.Transform.Simple as T
import qualified Physics.Transform.Opt as OT

import Utils.Utils

makeBox :: Double -> Double -> Double -> Double -> C.ConvexHull Double
makeBox x y w h =
  C.listToHull $ T.transform (T.translateTransform (V2 x y)) vertices
  where vertices = C.rectangleVertices w h

makeOptBox :: Double -> Double -> Double -> Double -> OC.ConvexHull
makeOptBox (D# x) (D# y) (D# w) (D# h) =
  OC.listToHull $ OT.transform (OT.translateTransform (OL.V2 x y)) vertices
  where vertices = OC.rectangleVertices w h

testBoxes :: (C.ConvexHull Double, C.ConvexHull Double)
testBoxes = (makeBox 0 0 4 4, makeBox 1 3 2 2)

testOptBoxes :: (OC.ConvexHull, OC.ConvexHull)
testOptBoxes = (makeOptBox 0 0 4 4, makeOptBox 1 3 2 2)

benchy :: Benchmark
benchy = bench "contact" $ whnf (evalContact . uncurry S.contact) testBoxes

benchy' :: Benchmark
benchy' = bench "opt contact" $ whnf (evalOptContact . uncurry OS.contact) testOptBoxes

evalContact :: Maybe (Flipping (Either (C.Neighborhood Double) (S.Contact Double))) -> S.Contact Double
evalContact (Just (Flip (Right c))) = c
evalContact (Just (Same (Right c))) = c
evalContact _ = error "unexpected non-contact"

evalOptContact :: Maybe (Flipping (Either OC.Neighborhood OS.Contact )) -> OS.Contact
evalOptContact (Just (Flip (Right c))) = c
evalOptContact (Just (Same (Right c))) = c
evalOptContact _ = error "unexpected non-contact"

main :: IO ()
main = do
  print . evalContact . uncurry S.contact $ testBoxes
  print . evalOptContact . uncurry OS.contact $ testOptBoxes
  defaultMain [benchy, benchy']
