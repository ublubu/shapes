module Directional where

import Control.Applicative
import SDL.Geometry

data GridDirection = GridRight | GridDown | GridLeft | GridUp

data Rectangular a = Rectangular a a a a

data GridOriented a = GridOriented GridDirection a

unitGeomPoint :: Rectangular GeomPoint
unitGeomPoint = Rectangular (1, 0) (0, 1) (-1, 0) (0, -1)

extract :: GridDirection -> Rectangular a -> a
extract GridRight (Rectangular x _ _ _) = x
extract GridDown (Rectangular _ x _ _) = x
extract GridLeft (Rectangular _ _ x _) = x
extract GridUp (Rectangular _ _ _ x) = x

extractOriented :: GridDirection -> Rectangular a -> GridOriented a
extractOriented GridRight (Rectangular x _ _ _) = GridOriented GridRight x
extractOriented GridDown (Rectangular _ x _ _) = GridOriented GridDown x
extractOriented GridLeft (Rectangular _ _ x _) = GridOriented GridLeft x
extractOriented GridUp (Rectangular _ _ _ x) = GridOriented GridUp x

injectOriented :: GridOriented a -> Rectangular (Maybe a)
injectOriented (GridOriented GridRight x) =
  Rectangular (Just x) Nothing Nothing Nothing
injectOriented (GridOriented GridDown x) =
  Rectangular Nothing (Just x) Nothing Nothing
injectOriented (GridOriented GridLeft x) =
  Rectangular Nothing Nothing (Just x) Nothing
injectOriented (GridOriented GridUp x) =
  Rectangular Nothing Nothing Nothing (Just x)

flatten :: Rectangular a -> [GridOriented a]
flatten (Rectangular a b c d) = [ GridOriented GridRight a
                                , GridOriented GridDown b
                                , GridOriented GridLeft c
                                , GridOriented GridUp d ]

collapse :: (a -> a -> Bool) -> Rectangular a -> Maybe (GridOriented a)
collapse shouldReplace t = foldl f' Nothing (flatten t)
  where f' aa b@(GridOriented _ x') = do
          a@(GridOriented _ x) <- aa
          return $ if shouldReplace x x' then b
                   else a

rotate :: Rectangular a -> Rectangular a
rotate (Rectangular a b c d) = Rectangular d a b c

instance Functor GridOriented where
  fmap f (GridOriented o x) = GridOriented o (f x)

instance Functor Rectangular where
  fmap f (Rectangular a b c d) = Rectangular (f a) (f b) (f c) (f d)

instance Applicative Rectangular where
  pure a = Rectangular a a a a
  (Rectangular fa fb fc fd) <*> (Rectangular a b c d) =
    Rectangular (fa a) (fb b) (fc c) (fd d)

toDirection :: GridOriented a -> GridDirection
toDirection (GridOriented dir _) = dir

degenerateRect :: Point a -> Rectangular a
degenerateRect (x, y) = Rectangular x y x y

intersect :: (Num a, Fractional a, Ord a) => Point (Point a) -> Rectangular a -> Maybe GridDirection
intersect p rect = toDirection <$> collapse (>) intersections
  where segment = degenerateRect p
        partial (x, y) t = (y - x) / (t - x)
        partials = partial <$> segment <*> rect
        isIntersection t = if t > 0 && t <= 1 then Just t
                           else Nothing
        intersections = fmap isIntersection partials

