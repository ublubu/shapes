{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.QuadtreeSpec where

import GHC.Prim ((+##))
import GHC.Types (Double(..))

import Control.DeepSeq
import Control.Monad

import qualified Data.IntMap.Strict as IM

import Control.Lens

import Physics.Broadphase.Aabb
import Physics.Quadtree.Aabb()
import Physics.Quadtree
import Physics.Linear

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  it "sizes add up" $ property qInsertProp
  it "expandTree on opposite corners" $ property qExpandProp
  it "qTest Parent" $ qTest (V2 0.0## 0.0##) box `shouldBe` QParent ()
  it "qTest Q1" $ qTest (V2 -0.1## -0.1##) box `shouldBe` qChild Q1
  it "qTest Q2" $ qTest (V2 1.1## -0.1##) box `shouldBe` qChild Q2
  it "qTest Q3" $ qTest (V2 1.1## 1.1##) box `shouldBe` qChild Q3
  it "qTest Q4" $ qTest (V2 -0.1## 1.1##) box `shouldBe` qChild Q4
  it "insert" $ rnf (insertObjs 1 [kBox, kBox'] empty') `shouldBe` ()
  it "expandTree" $
    let expanded = expandTree [kBox] Q1 empty
    in (_qCenter expanded, expanded ^. qNodes . qLens Q3)
       `shouldBe` (V2 1.0## 1.0##, Just empty)
  where kBox = (0, box)
        box = Aabb (Bounds 0.0## 1.0##) (Bounds 0.0## 1.0##)
        kBox' = (1, box')
        box' = Aabb (Bounds 0.0## 1.0##) (Bounds 0.0## 1.0##)
        centerDim = (V2 0.0## 0.0##, V2 1.0## 1.0##)
        empty = emptyQTree centerDim
        centerDim' = (V2 -10.0## -10.0##, V2 10.0## 10.0##)
        empty' = emptyQTree centerDim'

-- TODO: test that the four corners add up to the full area?
--       granted, that's part of what this property says
qExpandProp :: (QTestAabbs, QTestCenterDim) -> Bool
qExpandProp (QTestAabbs boxes, QTestCenterDim centerDim) =
  qAllContained kObjs tree
  where kObjs = zip [0..] boxes
        tree = expandTree kObjs Q1 . expandTree kObjs Q3 $ emptyQTree centerDim

qInsertProp :: (QTestCutoff, QTestAabbs, QTestCenterDim) -> Bool
qInsertProp (QTestCutoff cutoff, QTestAabbs boxes, QTestCenterDim centerDim) =
  verifiedSizes && numObjs == numBoxes
  where kObjs = zip [0..] boxes
        numBoxes = length boxes
        tree = insertObjs cutoff kObjs $ emptyQTree centerDim
        (numObjs, verifiedSizes) = verifySizes tree

verifySizes :: QTree Aabb -> (Int, Bool)
verifySizes QTree{..} =
  (_qSize, _qSize == childrenTotal + IM.size _qObjs && childrenGood)
  where f :: (Int, Bool) -> Maybe (QTree Aabb) -> (Int, Bool)
        f accum Nothing = accum
        f (total, good) (Just child) = (total + childTotal, good && childGood)
          where (childTotal, childGood) = verifySizes child
        (childrenTotal, childrenGood) = foldl f (0, True) _qNodes

newtype QTestAabb =
  QTestAabb { _qtAabb :: Aabb } deriving Show

instance Arbitrary QTestAabb where
  arbitrary = do
    (D# xmin) <- choose (0, 100)
    (D# xdim) <- choose (0, 100)
    (D# ymin) <- choose (0, 100)
    (D# ydim) <- choose (0, 100)
    return . QTestAabb
      $ Aabb (Bounds xmin (xmin +## xdim)) (Bounds ymin (ymin +## ydim))

newtype QTestAabbs = QTestAabbs [Aabb] deriving Show

instance Arbitrary QTestAabbs where
  arbitrary = do
    count <- choose (0, 100)
    QTestAabbs . fmap _qtAabb <$> replicateM count arbitrary

newtype QTestCenterDim =
  QTestCenterDim { _qtCenterDim :: (V2, V2) } deriving Show

instance Arbitrary QTestCenterDim where
  arbitrary = do
    (D# x) <- choose (-50, 150)
    (D# y) <- choose (-50, 150)
    (D# xdim) <- choose (1, 200)
    (D# ydim) <- choose (1, 200)
    return . QTestCenterDim $ (V2 x y, V2 xdim ydim)

newtype QTestCutoff = QTestCutoff Int deriving Show

instance Arbitrary QTestCutoff where
  arbitrary =
    QTestCutoff <$> frequency [(8, choose (1, 10)), (1, choose (11, 100))]
    --QTestCutoff <$> choose (150, 200)
