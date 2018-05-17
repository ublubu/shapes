{-# LANGUAGE MagicHash #-}

module Physics.Contact.Benchmark where

import           GHC.Types                  (Double (D#))

import           Criterion.Main

import qualified Physics.Contact.ConvexHull as OC
import qualified Physics.Contact.SAT        as OS
import qualified Physics.Linear             as OL
import qualified Physics.Transform          as OT

import           Utils.Utils

makeOptBox ::
     Double -- ^ center x
  -> Double -- ^ center y
  -> Double -- ^ width
  -> Double -- ^ height
  -> OC.ConvexHull
makeOptBox (D# x) (D# y) (D# w) (D# h) =
  OC.listToHull $ OT.transform (OT.translateTransform (OL.V2 x y)) vertices
  where vertices = OC.rectangleVertices w h

testOptBoxes :: (OC.ConvexHull, OC.ConvexHull)
testOptBoxes = (makeOptBox 0 0 4 4, makeOptBox 1 3 2 2)

benchy' :: Benchmark
benchy' = bench "opt contact" $ whnf (evalOptContact . uncurry OS.contact) testOptBoxes

evalOptContact :: Maybe (Flipping (Either OC.Neighborhood OS.Contact )) -> OS.Contact
evalOptContact (Just (Flip (Right c))) = c
evalOptContact (Just (Same (Right c))) = c
evalOptContact _                       = error "unexpected non-contact"

main :: IO ()
main = do
  print . evalOptContact . uncurry OS.contact $ testOptBoxes
  defaultMain [benchy']
