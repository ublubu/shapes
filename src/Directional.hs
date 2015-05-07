module Directional where

import Control.Applicative
import Data.Maybe
import SDL.Geometry

data GridDirection = GridRight | GridDown | GridLeft | GridUp deriving (Show, Eq)
data Axis = XAxis | YAxis deriving (Show, Eq)

data Rectangular a = Rectangular a a a a deriving Show
data AxisAligned a = AxisAligned a a deriving Show

data GridOriented a = GridOriented GridDirection a deriving Show
data SingleAxis a = SingleAxis Axis a deriving Show

unitGeomPoint :: Rectangular GeomPoint
unitGeomPoint = Rectangular (1, 0) (0, 1) (-1, 0) (0, -1)

signedRect :: Num a => Rectangular a
signedRect = Rectangular 1 1 (-1) (-1)

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

injectOriented_ :: GridOriented a -> a -> Rectangular a
injectOriented_ (GridOriented GridRight x) y = Rectangular x y y y
injectOriented_ (GridOriented GridDown x) y = Rectangular y x y y
injectOriented_ (GridOriented GridLeft x) y = Rectangular y y x y
injectOriented_ (GridOriented GridUp x) y = Rectangular y y y x

injectAxis :: SingleAxis a -> a -> AxisAligned a
injectAxis (SingleAxis XAxis x) y = AxisAligned x y
injectAxis (SingleAxis YAxis y) x = AxisAligned x y

flatten :: Rectangular a -> [GridOriented a]
flatten (Rectangular a b c d) = [ GridOriented GridRight a
                                , GridOriented GridDown b
                                , GridOriented GridLeft c
                                , GridOriented GridUp d ]

collapse :: (a -> a -> Bool) -> Rectangular (Maybe a) -> Maybe (GridOriented a)
collapse shouldReplace t = foldl f' Nothing (flatten t)
  where f' maa (GridOriented dir mb) = case maa of
          Nothing -> do
            b <- mb
            return $ GridOriented dir b
          Just (GridOriented _ a) -> case mb of
            Nothing -> maa
            Just b -> if shouldReplace a b then return $ GridOriented dir b
                      else maa

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

instance Functor SingleAxis where
  fmap f (SingleAxis a x) = SingleAxis a (f x)

instance Functor AxisAligned where
  fmap f (AxisAligned a b) = AxisAligned (f a) (f b)

instance Applicative AxisAligned where
  pure a = AxisAligned a a
  (AxisAligned fa fb) <*> (AxisAligned a b) = AxisAligned (fa a) (fb b)

toDirection :: GridOriented a -> GridDirection
toDirection (GridOriented dir _) = dir

toAxis :: GridOriented a -> SingleAxis a
toAxis (GridOriented GridRight x) = SingleAxis XAxis x
toAxis (GridOriented GridDown x) = SingleAxis YAxis x
toAxis (GridOriented GridLeft x) = SingleAxis XAxis x
toAxis (GridOriented GridUp x) = SingleAxis YAxis x

toRect :: Num a => Point a -> Point a -> Rectangular a
toRect (sx, sy) (ox, oy) = Rectangular (ox + sx) (oy + sy) ox oy

degenerateRect_ :: AxisAligned a -> Rectangular a
degenerateRect_ (AxisAligned x y) = Rectangular x y x y

degenerateRect :: Point a -> Rectangular a
degenerateRect = degenerateRect_ . fromPoint

scaledSignedRect :: Num a => Point a -> Rectangular a
scaledSignedRect x = (*) <$> signedRect <*> degenerateRect x

fromBottomRight :: Num a => Point a -> Rectangular a
fromBottomRight (x, y) = Rectangular x y 0 0

splitSegment :: Point (Point a) -> AxisAligned (Point a)
splitSegment ((x, y), (x', y')) = AxisAligned (x, x') (y, y')

orientedOnRect :: GridOriented (a -> b) -> Rectangular a -> GridOriented b
orientedOnRect (GridOriented dir f) rect = GridOriented dir (f $ extract dir rect)

alignedOnRect :: AxisAligned (a -> b) -> Rectangular a -> Rectangular b
alignedOnRect f rect = degenerateRect_ f <*> rect

fromPoint :: Point a -> AxisAligned a
fromPoint (x, y) = AxisAligned x y

toPoint :: AxisAligned a -> Point a
toPoint (AxisAligned x y) = (x, y)

intersect :: (Num a, Fractional a, Ord a) => Point (Point a) -> Rectangular a -> Maybe (GridOriented (Point a))
intersect p rect = fmap intersectionPoint $ collapse (>) intersections
  where segment = splitSegment p
        partial (x, x') t = (t - x) / (x' - x)
        partials = partial <$> degenerateRect_ segment <*> rect
        isIntersection t = if t > 0 && t <= 1 then Just t
                           else Nothing
        intersections = fmap isIntersection partials
        intersectionPoint tt@(GridOriented dir t) =
          GridOriented dir point
          where bound = toAxis $ extractOriented dir rect
                useT (x, x') = x + (t * (x' - x))
                point = toPoint (injectAxis (fmap const bound) useT <*> segment)

