{-# LANGUAGE TypeFamilies #-}
module Physics.Demo.Scenes where

import Data.Proxy

import Physics.Scenes.Scene
import Physics.Engine.Class
import Utils.Utils (posMod)

import qualified Physics.Scenes.TwoFlyingBoxes as S0
import qualified Physics.Scenes.FourBoxesTwoStatic as S1
import qualified Physics.Scenes.Rolling as S2
import qualified Physics.Scenes.Stacks as S3
import qualified Physics.Scenes.Balls as S4

scenes :: (PhysicsEngine e, PEExternalObj e ~ ()) => Proxy e -> [Scene e]
scenes p = [ S4.makeScene (10, 10) 1 1 p ()
           , S4.makeScene' (30, 30) 0.2 1 p ()
           , S3.makeScene (30, 30) 1 p ()
           , S1.scene p () () () ()
           , S1.scene' p () () () ()
           , S2.scene p () ()
           , S3.scene p ()
           , S3.scene' p ()
           , S3.scene'' p ()
           , S3.scene''' p ()
           , S0.scene p () ()
           , S4.circleAndBox p () ()
           , S4.twoCircles p () ()
           ]

nextScene :: Int -> [a] -> (Int, a)
nextScene i ss = (i', ss !! i')
  where i' = posMod (i + 1) (length ss)
