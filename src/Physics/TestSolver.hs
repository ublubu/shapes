{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Physics.TestSolver where

import Control.Lens
import qualified Data.IntMap.Strict as IM
import Physics.Solver
import Utils.Utils

testData :: (Num a) => IM.IntMap a
testData = IM.fromList [(0, 0), (1, 1), (2, 2), (3, 3)]

data TestWorldType a = TestWorldType { _testWorldObjs :: IM.IntMap a } deriving Show
makeLenses ''TestWorldType


testData' :: (Num a) => TestWorldType a
testData' = TestWorldType testData

--testData :: (Num a) => (a, a)
--testData = (1, 2)

testSolver :: (Num a) => Solver a Int a
testSolver = Solver f
  where f b _ a = (a + b, testSolver)

testPairSolver :: (Num a) => PairSolver a Int a
testPairSolver = Solver f
  where f b _ a = (pairMap (+b) a, testPairSolver)

testRunSolver :: (Int, Int) -> PairSolver a Int a -> a -> TestWorldType a -> (TestWorldType a, PairSolver a Int a)
testRunSolver ij = solve' (testWorldObjs . pairiix ij)

testResult = solve' (iix 1) testSolver 1 testData

testPairResult = solve' (pairiix (1, 2)) testPairSolver 1 testData
testPairResult' = solve' (testWorldObjs . pairiix (1, 2)) testPairSolver 1 testData'

testRunResult = testRunSolver (1, 2) testPairSolver 1 testData'
