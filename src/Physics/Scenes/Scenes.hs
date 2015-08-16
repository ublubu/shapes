module Physics.Scenes.Scenes where

import Linear.Epsilon
import Physics.Constraint
import Utils.Utils (posMod)
import Physics.Scenes.Scene
import qualified Physics.Scenes.TwoFlyingBoxes as S0
import qualified Physics.Scenes.FourBoxesTwoStatic as S1
import qualified Physics.Scenes.Rolling as S2

scenes :: (Fractional a, Eq a, Physical b p, Epsilon b, Floating b, Ord b, Floating c) => [Scene p a b c]
scenes = [S0.scene, S1.scene, S2.scene]

nextScene :: Int -> [Scene p a b c] -> (Int, Scene p a b c)
nextScene i ss = (i', ss !! i')
  where i' = posMod (i + 1) (length ss)
