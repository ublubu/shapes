module Physics.Broadphase.Opt.AabbSpec where

import Test.Hspec
import Test.QuickCheck

import Physics.Broadphase.Opt.Aabb

spec :: Spec
spec = do
  it "|unorderedPairs n| = n * n-1 / 2" $ property $
    \(ItemCount n) -> length (unorderedPairs n) == (n * (n-1) `quot` 2)

newtype ItemCount = ItemCount Int deriving Show

instance Arbitrary ItemCount where
  arbitrary = ItemCount <$> choose (0, 30)
