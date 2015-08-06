module Physics.TestConstraintSolver where

import qualified Data.IntMap.Strict as IM
import Physics.ConstraintSolver
import Utils.Utils

testMap :: IM.IntMap Double
testMap = IM.empty

testMapResult = findOrInsert 0 0.5 testMap

testMapResult' = findOrInsert 0 1.5 (snd testMapResult)

testPairMap :: PairMap Double
testPairMap = IM.empty

testResult0 = findOrInsertPair (0, 0) 1 testPairMap

testPairMap' = snd testResult0

testResult1 = findOrInsertPair (0, 1) 2 testPairMap'

testPairMap'' = snd testResult1

testResult2 = findOrInsertPair (0, 1) 3 testPairMap''
