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

import Utils.Utils
import Physics.Broadphase.Aabb

data StrictEither a b = SLeft a | SRight b
makeLenses ''StrictEither

data LeafNode = Leaf Int
data SpineNode = Spine { _spineHeight :: !Int
                       , _spineChildren :: !(SP' Node)
                       }
data Node = Node { _nodeBox :: Aabb, _nodeContents :: !(StrictEither LeafNode SpineNode) }
makeLenses ''LeafNode
makeLenses ''SpineNode
makeLenses ''Node

spineNode :: Node -> Node -> Node
spineNode fstChild sndChild = Node box $ SRight (Spine height $ SP fstChild sndChild)
  where height = 1 + (max (nodeHeight fstChild) (nodeHeight sndChild))
        box = mergeAabb (_nodeBox fstChild) (_nodeBox sndChild)

leafNode :: Aabb -> Int -> Node
leafNode box leafKey = Node box $ SLeft (Leaf leafKey)

colliders :: Aabb -> Node -> [Int]
colliders queryBox (Node targetBox (SLeft (Leaf key)))
  | aabbCheck queryBox targetBox =
      [key] -- collided with a leaf
  | otherwise = []
colliders queryBox (Node targetBox (SRight (Spine _ childNodes)))
  | not $ aabbCheck queryBox targetBox = []
  | otherwise = spFold $ colliders queryBox `spMap` childNodes

boundsLength :: Bounds -> Double
boundsLength Bounds{..} = D# (_bmax -## _bmin)

aabbArea :: Aabb -> Double
aabbArea Aabb{..} = boundsLength _aabbx * boundsLength _aabby

areaIncrease :: Aabb -> Aabb -> Double
areaIncrease box totalBox = aabbArea totalBox' - aabbArea totalBox
  where totalBox' = mergeAabb box totalBox

insert :: Int -> Aabb -> Node -> Node
-- attempt to insert at a leaf: they will be siblings
insert key box sibling@(Node _ (SLeft (Leaf _))) =
  spineNode sibling (leafNode box key)
-- attempt to insert at a spine: choose a side, try to insert there, update height
insert key box (Node _ (SRight (Spine _ (SP fstNode sndNode))))
  | areaIncrease box (_nodeBox fstNode) < areaIncrease box (_nodeBox sndNode) =
      spineNode (insert key box fstNode) sndNode -- insert in fstNode
  | otherwise = spineNode fstNode (insert key box sndNode)

nodeHeight :: Node -> Int
nodeHeight (Node _ (SLeft (Leaf _))) = 0
nodeHeight (Node _ (SRight (Spine height _))) = height

treeFromList :: [(Int, Aabb)] -> Maybe Node
treeFromList [] = Nothing
treeFromList pairs = foldl f Nothing pairs
  where f Nothing (key, box) = Just $ leafNode box key
        f (Just node) (key, box) = Just $ insert key box node
