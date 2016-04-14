{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Physics.Quadtree where

import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens hiding (children)
import qualified Data.IntMap.Strict as IM
import Data.Monoid

import Physics.Linear

data QIx =
  Q1 | Q2 | Q3 | Q4
  deriving (Show, Eq, Generic, NFData)
makePrisms ''QIx

data QIxed' x =
  QParent x | QChild QIx x
  deriving (Show, Eq, Generic, NFData)

data QOriented x =
  QOriented { _q1 :: x
            , _q2 :: x
            , _q3 :: x
            , _q4 :: x
            } deriving (Show, Eq, Generic, NFData)
makeLenses ''QOriented

data QOriented' x =
  QOriented' { _qp' :: x
             , _q1' :: x
             , _q2' :: x
             , _q3' :: x
             , _q4' :: x
             } deriving (Show, Eq, Generic, NFData)
makeLenses ''QOriented'

class QObj o where
  qTest :: V2 -> o -> QIxed' ()

-- dim = distance from center to corner
data QTree o =
  QTree { _qNodes :: QOriented (Maybe (QTree o))
        , _qObjs :: IM.IntMap o
        , _qCenter :: V2
        , _qDim :: V2
        , _qSize :: Int -- number of objects (recursive)
        } deriving (Show, Eq, Generic, NFData)
makeLenses ''QTree

qChild :: QIx -> QIxed' ()
qChild i = QChild i ()

qLens :: QIx -> Lens' (QOriented x) x
qLens Q1 = q1
qLens Q2 = q2
qLens Q3 = q3
qLens Q4 = q4

qLens' :: QIx -> Lens' (QOriented' x) x
qLens' Q1 = q1'
qLens' Q2 = q2'
qLens' Q3 = q3'
qLens' Q4 = q4'

qOp :: QIx -> QIx
qOp Q1 = Q3
qOp Q2 = Q4
qOp Q3 = Q1
qOp Q4 = Q2

qIxes :: QOriented QIx
qIxes = QOriented Q1 Q2 Q3 Q4

qCons :: QIxed' x -> QOriented' [x] -> QOriented' [x]
qCons (QParent x) xs = xs & qp' %~ (x:)
qCons (QChild i x) xs = xs & qLens' i %~ (x:)

_qc' :: QOriented' x -> QOriented x
_qc' (QOriented' _ a b c d)  = QOriented a b c d

emptyQTree :: (V2, V2) -> QTree o
emptyQTree (center, dim) =
  QTree { _qNodes = pure Nothing
        , _qObjs = IM.empty
        , _qCenter = center
        , _qDim = dim
        , _qSize = 0
        }

-- quadrant index -> dim -> center
qAddDim :: QIx -> V2 -> V2 -> V2
qAddDim Q1 = plusV2
qAddDim Q2 = plusV2 . negxV2
qAddDim Q3 = flip minusV2
qAddDim Q4 = plusV2 . negyV2

qChildCenterDim :: V2 -> V2 -> QIx -> (V2, V2)
qChildCenterDim center dim i = (qAddDim i dim' center, dim')
  where dim' = dim `smulV2'` 0.5

-- assumes all new Int keys
-- assumes nodes with < cutoff elems have no child nodes
--   ^ this assumption doesn't seem particularly dangerous
-- 'cutoff' is the smallest node you can split
-- TODO: more complex heuristic than 'cutoff'? (e.g. increasing cutoff with tree depth)
insertObjs :: forall o. (Show o, QObj o)
           => Int
           -> [(Int, o)]
           -> QTree o
           -> QTree o
--insertObjs _ [] = id
insertObjs cutoff kObjs =
  insertObjs' cutoff kObjs . expandTree kObjs Q1 . expandTree kObjs Q3

-- expands the tree along one direction until it will fit all objs on that side
-- doesn't actually insert any objects
expandTree :: forall o. (QObj o)
           => [(Int, o)]
           -> QIx
           -> QTree o
           -> QTree o
expandTree kObjs i tree@QTree{..} =
  if all contained kObjs
  then tree
  else expandTree kObjs i $ emptyQTree (corner, dim2)
       & qSize .~ _qSize
       & qNodes . qLens (qOp i) .~ Just tree
  where dim2 = _qDim `smulV2'` 2
        corner = qAddDim i _qDim _qCenter
        contained (_, obj) = qTest corner obj == qChild (qOp i)

qContained :: (QObj o)
           => o
           -> QTree o
           -> Bool
qContained obj QTree{..} =
  and cornerChecks
  where corners = (\i -> qAddDim i _qDim _qCenter) <$> qIxes
        isInsideCorner i corner = qTest corner obj == qChild (qOp i)
        cornerChecks = isInsideCorner <$> qIxes <*> corners

qAllContained :: (QObj o)
              => [(Int, o)]
              -> QTree o
              -> Bool
qAllContained kObjs tree = and $ fmap (flip qContained tree . snd) kObjs

-- assumes new objs fit inside the QTree's bounds
insertObjs' :: forall o. (Show o, QObj o)
            => Int
            -> [(Int, o)]
            -> QTree o
            -> QTree o
--insertObjs' _ [] tree = tree
insertObjs' cutoff kObjs tree@QTree{..} =
  if length kObjs + _qSize < cutoff
  then shallowInsertObjs kObjs tree
  else deepInsertObjs cutoff kObjs tree

-- only inserts objects at the top-level node
shallowInsertObjs :: forall o. (Show o, QObj o)
                  => [(Int, o)]
                  -> QTree o
                  -> QTree o
shallowInsertObjs kObjs tree@QTree{..} =
  tree
  & qObjs %~ (\objs -> foldr (uncurry IM.insert) objs kObjs)
  & qSize +~ length kObjs

-- inserts objects as deeply as necessary
-- makes no changes on its own -> uses shallowInsert and insertChild
deepInsertObjs :: forall o. (Show o, QObj o)
               => Int
               -> [(Int, o)]
               -> QTree o
               -> QTree o
deepInsertObjs cutoff kObjs tree@QTree{..} =
  insertChildObjs cutoff (_qc' ixedObjs) $ shallowInsertObjs (_qp' ixedObjs) tree
  where ixedObjs :: QOriented' [(Int, o)]
        ixedObjs = foldr qCons (pure []) (f <$> kObjs)
        f :: (Int, o) -> QIxed' (Int, o)
        f kObj@(_, obj) = const kObj <$> qTest _qCenter obj

insertChildObjs :: forall o. (Show o, QObj o)
                => Int
                -> QOriented [(Int, o)]
                -> QTree o
                -> QTree o
insertChildObjs cutoff childObjs tree@QTree{..} =
  tree
  & qNodes %~ (\children -> f <$> qIxes <*> childObjs <*> children)
  & qSize +~ sum (length <$> childObjs)
  where f :: QIx -> [(Int, o)] -> Maybe (QTree o) -> Maybe (QTree o)
        f _ [] child = child
        f i objs Nothing =
          Just $ newNode cutoff objs (qChildCenterDim _qCenter _qDim i)
        f _ objs (Just child) = Just $ insertObjs' cutoff objs child

newNode :: forall o. (Show o, QObj o)
        => Int
        -> [(Int, o)]
        -> (V2, V2)
        -> QTree o
newNode cutoff kObjs centerDim =
  insertObjs' (cutoff) kObjs $ emptyQTree centerDim

instance Functor QIxed' where
  fmap f (QParent x) = QParent (f x)
  fmap f (QChild i x) = QChild i (f x)

instance Functor QOriented where
  fmap f (QOriented w x y z) = QOriented (f w) (f x) (f y) (f z)

instance Applicative QOriented where
  pure x = QOriented x x x x
  (QOriented f g h i) <*> (QOriented w x y z) = QOriented (f w) (g x) (h y) (i z)

instance Foldable QOriented where
  foldMap f (QOriented a b c d) = f a <> f b <> f c <> f d

instance Functor QOriented' where
  fmap f (QOriented' p w x y z) = QOriented' (f p) (f w) (f x) (f y) (f z)

instance Applicative QOriented' where
  pure x = QOriented' x x x x x
  (QOriented' e f g h i) <*> (QOriented' v w x y z) = QOriented' (e v) (f w) (g x) (h y) (i z)
