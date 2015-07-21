module World where

import Control.Lens
import Control.Lens.At
import Control.Lens.Traversal
import Control.Lens.Indexed
import Control.Zipper
import Data.Sequence
import Data.Sequence.Lens

testWorld :: Num a => Seq a
testWorld = fromList [1, 2, 3, 4, 5]

testZipper = zipper testWorld

secondItem = testZipper & downward viewL & fromWithin traverse & tug rightward & view focus

printWorld = imapM_ (\i a -> putStrLn (show i ++ " " ++ show a)) testWorld
