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

type UpdateNodeVal a = a -> Maybe a -> Maybe a -> a
type ChooseNodeLeft a = a -> Maybe a -> Maybe a -> Bool

justInsert :: forall a. ChooseNodeLeft a
                     -> UpdateNodeVal a
                     -> a
                     -> Node a
                     -> Node a
justInsert chooseLeft updateVal val root@Node { .. }
  | chooseLeftWith chooseLeft root = insertWith nodeLeft
  | otherwise = insertWith nodeRight
  where
    setWith branchLens newNode = updateValWith updateVal . setHeight $ (root & branchLens .~ Just newNode)
    insertWith :: Lens' (Node a) (Maybe (Node a)) -> Node a
    insertWith branchLens =
      case root ^. branchLens of
        Nothing         -> setWith branchLens $ newLeaf val
        Just branchNode -> setWith branchLens $ justInsert chooseFirst updateVal val branchNode

{- |

 a           b
  \         / \
   b   ->  a   c
    \
     c
-}
leftRotate :: UpdateNodeVal a -> Node a -> Node a
leftRotate updateVal a@Node { .. } =
 _

newLeaf :: a -> Node a
newLeaf val = Node 1 val Nothing Nothing

getHeight :: Maybe (Node a) -> Int
getHeight Nothing = 0
getHeight (Just node) = _nodeHeight node

chooseLeftWith :: ChooseNodeLeft a -> Node a -> Bool
chooseLeftWith chooseLeft Node { .. } = chooseLeft _nodeValue (f _nodeLeft) (f _nodeRight)
  where f = fmap $ view nodeValue

updateValWith :: UpdateNodeVal a -> Node a -> Node a
updateValWith updateVal node@Node { .. } =
  node & nodeValue .~ updateVal _nodeValue (f _nodeLeft) (f _nodeRight)
  where f = fmap $ view nodeValue


setHeight :: Node a -> Node a
setHeight root@Node { .. } = root & nodeHeight .~ ((max `on` getHeight) _nodeLeft _nodeRight + 1)
