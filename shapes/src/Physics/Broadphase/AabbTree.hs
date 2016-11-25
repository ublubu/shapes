{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Physics.Broadphase.AabbTree where

import GHC.Prim (Double#, (-##))
import GHC.Types (Double(D#))

import Control.Lens
import Data.Foldable (toList)
import Data.Maybe
import Data.Monoid

import qualified Data.IntMap.Strict as IM

import Physics.Broadphase.Aabb

data StrictEither a b = SLeft a | SRight b
makeLenses ''StrictEither

data StrictOneOrTwo a = SOne a | STwo a a
makeLenses ''StrictOneOrTwo

instance Functor StrictOneOrTwo where
  fmap f (SOne x) = SOne $ f x
  fmap f (STwo x y) = STwo (f x) (f y)

instance Foldable StrictOneOrTwo where
  foldMap f (SOne x) = f x
  foldMap f (STwo x y) = f x <> f y

data LeafNode = Leaf Int
data SpineNode = Spine { _spineHeight :: !Int
                       , _spineChildren :: !(StrictOneOrTwo Node)
                       }
data Node = Node { _nodeBox :: Aabb, _nodeContents :: !(StrictEither LeafNode SpineNode) }
makeLenses ''LeafNode
makeLenses ''SpineNode
makeLenses ''Node

spineNode :: StrictOneOrTwo Node -> Node
spineNode children = Node box $ SRight (Spine height children)
  where height = 1 + (maxOneOrTwo $ nodeHeight <$> children)
        box = foldOneOrTwo mergeAabb $ _nodeBox <$> children

leafNode :: Aabb -> Int -> Node
leafNode box leafKey = Node box $ SLeft (Leaf leafKey)

colliders :: Aabb -> Node -> [Int]
colliders queryBox (Node targetBox (SLeft (Leaf key)))
  | aabbCheck queryBox targetBox =
      [key] -- collided with a leaf
  | otherwise = []
colliders queryBox (Node targetBox (SRight (Spine _ children)))
  | not $ aabbCheck queryBox targetBox = []
  | otherwise = mconcat $ colliders queryBox <$> toList children

boundsLength :: Bounds -> Double
boundsLength Bounds{..} = D# (_bmax -## _bmin)

aabbArea :: Aabb -> Double
aabbArea Aabb{..} = boundsLength _aabbx * boundsLength _aabby

areaIncrease :: Aabb -> Aabb -> Double
areaIncrease box totalBox = aabbArea totalBox' - aabbArea totalBox
  where totalBox' = mergeAabb box totalBox

justInsert :: Int -> Aabb -> Node -> Node
-- attempt to insert at a leaf: they will be siblings
justInsert key box sibling@(Node siblingBox (SLeft (Leaf _))) =
  spineNode $ STwo sibling $ leafNode box key
-- attempt to insert at a spine: choose a side, try to insert there, update height
justInsert key box (Node _ (SRight (Spine _ (SOne leftNode))))
  | areaIncrease box leftBox < aabbArea box = -- insert in leftNode
    spineNode . SOne $ justInsert key box leftNode
  | otherwise = -- insert as rightNode (TODO(opt): return merged box from areaIncrease?)
    spineNode $ STwo leftNode newNode
  where leftBox = leftNode ^. nodeBox
        newNode = leafNode box key

maxOneOrTwo :: Ord a => StrictOneOrTwo a -> a
maxOneOrTwo = foldOneOrTwo max

foldOneOrTwo :: (a -> a -> a) -> StrictOneOrTwo a -> a
foldOneOrTwo _ (SOne x) = x
foldOneOrTwo f (STwo x y) = f x y

nodeHeight :: Node -> Int
nodeHeight (Node _ (SLeft (Leaf _))) = 0
nodeHeight (Node _ (SRight (Spine height _))) = height
