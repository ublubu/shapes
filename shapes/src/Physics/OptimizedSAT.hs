{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.OptimizedSAT where

import Control.Lens ((^.), (%~), (&), (.~), view, makeLenses, makePrisms)
import Control.Monad
import Data.Monoid
import Linear.Epsilon
import Physics.ConvexHull
import Physics.SAT
import Utils.Utils

data Penetration =
  Penetration { _pEdge :: Int
              , _pPenetrator :: (Int, Int)
              } deriving (Show, Eq)
makeLenses ''Penetration

data SATCache = SepCache Int | PenCache Penetration
              deriving (Show, Eq)
makePrisms ''SATCache

checkSeparatingAxis :: (Num a, Ord a)
                    => ConvexHull a
                    -> Int
                    -> ConvexHull a
                    -> Maybe (Neighborhood a)
checkSeparatingAxis sEdge edgeIndex sPen =
  case overlap sEdge edge sPen of
  Nothing -> Just edge
  Just _ -> Nothing
  where edge = _hullNeighborhood edgeIndex sEdge

regenerateContact :: (Epsilon a, Floating a, Ord a)
                  => ConvexHull a
                  -> Int
                  -> ConvexHull a
                  -> (Int, Int)
                  -> Maybe (Contact a)
regenerateContact sEdge edgeIndex sPen penIndices =
  fmap f $ clipEdge (edge, _neighborhoodNext edge) n pen
  where edge = _hullNeighborhood edgeIndex sEdge
        pen = pairMap _hullNeighborhood penIndices & pairMap ($ sPen)
        n = _neighborhoodUnitNormal edge
        f contactPoints = Contact edge contactPoints pen

optContact :: forall a . (Epsilon a, Floating a, Ord a)
           => ConvexHull a
           -> ConvexHull a
           -> Flipping (SATCache)
           -> Maybe (Flipping (Either (Neighborhood a) (Contact a), SATCache))
optContact a b fCache =
  fmap flipJoin . flipInjectF . flipMap f fCache $ (a, b)
  where f :: SATCache
          -> (ConvexHull a, ConvexHull a)
          -> Maybe (Flipping (Either (Neighborhood a) (Contact a), SATCache))
        f = flip . uncurry $ optContact_

optContact_ :: forall a . (Epsilon a, Floating a, Ord a)
           => ConvexHull a
           -> ConvexHull a
           -> SATCache
           -> Maybe (Flipping (Either (Neighborhood a) (Contact a), SATCache))
optContact_ a b cache@(SepCache separatorIndex) = do
  optContact__ a b cache . fmap Left $ checkSeparatingAxis a separatorIndex b
optContact_ a b cache@(PenCache (Penetration {..})) =
  optContact__ a b cache . fmap Right $ regenerateContact a _pEdge b _pPenetrator

optContact__ :: (Epsilon a, Floating a, Ord a)
             => ConvexHull a
             -> ConvexHull a
             -> SATCache
             -> Maybe (Either (Neighborhood a) (Contact a))
             -> Maybe (Flipping (Either (Neighborhood a) (Contact a), SATCache))
optContact__ a b cache regenResult =
  getFirst $ wrap regenResult <> (First $ cachingContact a b)
  where wrap = First . fmap (Same . (\c -> (c, cache)))


cachingContact :: forall a . (Epsilon a, Floating a, Ord a)
               => ConvexHull a
               -> ConvexHull a
               -> Maybe (Flipping (Either (Neighborhood a) (Contact a), SATCache))
cachingContact a b =
  (fmap . fmap) f $ contact a b
  where f c = (c, buildSATCache c)

buildSATCache :: Either (Neighborhood a) (Contact a) -> SATCache
buildSATCache (Left separator) = SepCache $ _neighborhoodIndex separator
buildSATCache (Right (Contact{..})) =
  PenCache $ Penetration (_neighborhoodIndex _contactEdge)
  (pairMap _neighborhoodIndex _contactPenetratingEdge)
