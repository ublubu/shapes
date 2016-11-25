{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Broadphase.AvlTree where

import Control.Lens
import Data.Function

data Node a = Node { _nodeHeight :: Int
                   , _nodeValue :: a
                   , _nodeLeft :: Maybe (Node a)
                   , _nodeRight :: Maybe (Node a) }
makeLenses ''Node

justInsert :: forall a. (a -> Maybe (Node a) -> Maybe (Node a) -> Bool) -> a -> Node a -> Node a
justInsert chooseFirst val root@Node{..}
  | chooseFirst val _nodeLeft _nodeRight = insertWith nodeLeft
  | otherwise = insertWith nodeRight
  where setWith branchLens newNode = setHeight (root & branchLens .~ Just newNode)
        insertWith :: Lens' (Node a) (Maybe (Node a)) -> Node a
        insertWith branchLens = case root ^. branchLens of
          Nothing -> setWith branchLens $ newLeaf val
          Just branchNode -> setWith branchLens $ justInsert chooseFirst val branchNode

newLeaf :: a -> Node a
newLeaf val = Node 1 val Nothing Nothing

getHeight :: Maybe (Node a) -> Int
getHeight Nothing = 0
getHeight (Just node) = _nodeHeight node

setHeight :: Node a -> Node a
setHeight root@Node { .. } = root & nodeHeight .~ ((max `on` getHeight) _nodeLeft _nodeRight + 1)
