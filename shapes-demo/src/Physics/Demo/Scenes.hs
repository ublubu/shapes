{-# LANGUAGE TypeFamilies #-}
module Physics.Demo.Scenes where

import           Data.Proxy

import           Physics.Scenes.Scene
import           Utils.Utils                       (posMod)

import qualified Physics.Scenes.Balls              as S4
import qualified Physics.Scenes.FourBoxesTwoStatic as S1
import qualified Physics.Scenes.Rolling            as S2
import qualified Physics.Scenes.Stacks             as S3
import qualified Physics.Scenes.TwoFlyingBoxes     as S0

scenes :: [Scene ()]
scenes =
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

nextScene :: Int -> [a] -> (Int, a)
nextScene i ss = (i', ss !! i')
  where i' = posMod (i + 1) (length ss)
