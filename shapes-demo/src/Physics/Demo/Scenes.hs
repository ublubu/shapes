{-# LANGUAGE TypeFamilies #-}
module Physics.Demo.Scenes where

import           Control.Monad.ST
import           Physics.Scenes.Scene
import           Utils.Utils                       (posMod)

import qualified Physics.Scenes.Balls              as S4
import qualified Physics.Scenes.FourBoxesTwoStatic as S1
import qualified Physics.Scenes.Rolling            as S2
import qualified Physics.Scenes.Stacks             as S3
import qualified Physics.Scenes.TwoFlyingBoxes     as S0

scenes_ :: [ST s (Scene s ())]
scenes_ =
  [ S4.makeScene (10, 10) 1 1 ()
  , S4.makeScene' (30, 30) 0.2 1 ()
  , S3.makeScene (30, 30) 1 ()
  , S1.scene () () () ()
  , S1.scene' () () () ()
  , S2.scene () ()
  , S3.scene ()
  , S3.scene' ()
  , S3.scene'' ()
  , S3.scene''' ()
  , S0.scene () ()
  , S4.circleAndBox () ()
  , S4.twoCircles () ()
  ]

sceneCount = length scenes_

scenes :: Int -> ST s (Scene s ())
scenes i = scenes_ !! i

nextScene :: Int -> ST s (Int, Scene s ())
nextScene i = do
  s <- scenes i'
  return (i', s)
  where i' = posMod (i + 1) sceneCount
