module Physics.Scenes.Scenes where

import Data.Proxy

import Physics.Scenes.Scene
import Physics.Engine.Class
import Utils.Utils (posMod)

import qualified Physics.Scenes.TwoFlyingBoxes as S0
import qualified Physics.Scenes.FourBoxesTwoStatic as S1
import qualified Physics.Scenes.Rolling as S2
import qualified Physics.Scenes.Stacks as S3

scenes :: (PhysicsEngine e) => Proxy e -> [Scene e]
scenes p =
  [S0.scene, S1.scene, S2.scene, S3.scene, S3.scene', S3.scene'', S3.scene'''] <*> pure p

nextScene :: Int -> [a] -> (Int, a)
nextScene i ss = (i', ss !! i')
  where i' = posMod (i + 1) (length ss)
