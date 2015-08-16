module Physics.Scenes.Scenes where

import Linear.Epsilon
import Physics.Constraint
import Utils.Utils (posMod)
import Physics.Scenes.Scene
import qualified Physics.Scenes.TwoFlyingBoxes as S0
import qualified Physics.Scenes.FourBoxesTwoStatic as S1
import qualified Physics.Scenes.Rolling as S2
import qualified Physics.Scenes.Stacks as S3

scenes :: (Physical a p, Epsilon a, Floating a, Ord a, Eq a) => [Scene a p]
scenes = [S0.scene, S1.scene, S2.scene, S3.scene, S3.scene']

nextScene :: Int -> [Scene a p] -> (Int, Scene a p)
nextScene i ss = (i', ss !! i')
  where i' = posMod (i + 1) (length ss)
