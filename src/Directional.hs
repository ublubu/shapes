module Directional where

import Control.Applicative
import Data.Foldable
import Data.Maybe
import Geometry

data GridDirection = GridRight | GridDown | GridLeft | GridUp deriving (Show, Eq)
data Axis = XAxis | YAxis deriving (Show, Eq)

data Rectangular a = Rectangular a a a a deriving Show
data AxisAligned a = AxisAligned a a deriving Show

data GridOriented a = GridOriented GridDirection a deriving Show
data SingleAxis a = SingleAxis Axis a deriving Show

reverseDirection :: GridDirection -> GridDirection
reverseDirection GridRight = GridLeft
reverseDirection GridDown = GridUp
reverseDirection GridLeft = GridRight
reverseDirection GridUp = GridDown

signedVectors :: Num a => Rectangular (Pair a)
signedVectors = Rectangular (Pair 1 0) (Pair 0 1) (Pair (-1) 0) (Pair 0 (-1))

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
collapse shouldReplace t = Prelude.foldl f' Nothing (flatten t)
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

reverseRect :: Rectangular a -> Rectangular a
reverseRect = rotate . rotate

swapAxes :: AxisAligned a -> AxisAligned a
swapAxes (AxisAligned a b) = AxisAligned b a

instance Functor GridOriented where
  fmap f (GridOriented o x) = GridOriented o (f x)

instance Functor Rectangular where
  fmap f (Rectangular a b c d) = Rectangular (f a) (f b) (f c) (f d)

instance Foldable Rectangular where
  foldMap f (Rectangular a b c d) = foldMap f [a, b, c, d]

instance Applicative Rectangular where
  pure a = Rectangular a a a a
  (Rectangular fa fb fc fd) <*> (Rectangular a b c d) =
    Rectangular (fa a) (fb b) (fc c) (fd d)

instance Functor SingleAxis where
  fmap f (SingleAxis a x) = SingleAxis a (f x)

instance Functor AxisAligned where
  fmap f (AxisAligned a b) = AxisAligned (f a) (f b)

instance Foldable AxisAligned where
  foldMap f (AxisAligned a b) = foldMap f [a, b]

instance Applicative AxisAligned where
  pure a = AxisAligned a a
  (AxisAligned fa fb) <*> (AxisAligned a b) = AxisAligned (fa a) (fb b)

toDirection :: GridOriented a -> GridDirection
toDirection (GridOriented dir _) = dir

deorient :: GridOriented a -> a
deorient  (GridOriented _ x) = x

toAxis :: GridOriented a -> SingleAxis a
toAxis (GridOriented GridRight x) = SingleAxis XAxis x
toAxis (GridOriented GridDown x) = SingleAxis YAxis x
toAxis (GridOriented GridLeft x) = SingleAxis XAxis x
toAxis (GridOriented GridUp x) = SingleAxis YAxis x

toRect :: Num a => Pair a -> Pair a -> Rectangular a
toRect (Pair sx sy) (Pair ox oy) = Rectangular (ox + sx) (oy + sy) ox oy

degenerateRect_ :: AxisAligned a -> Rectangular a
degenerateRect_ (AxisAligned x y) = Rectangular x y x y

degenerateRect :: Pair a -> Rectangular a
degenerateRect = degenerateRect_ . fromPair

scaledSignedRect :: Num a => Pair a -> Rectangular a
scaledSignedRect x = (*) <$> signedRect <*> degenerateRect x

fromBottomRight :: Num a => Pair a -> Rectangular a
fromBottomRight (Pair x y) = Rectangular x y 0 0

splitSegment :: Pair (Pair a) -> AxisAligned (Pair a)
splitSegment (Pair (Pair x y) (Pair x' y')) = AxisAligned (Pair x x') (Pair y y')

orientedOnRect :: GridOriented (a -> b) -> Rectangular a -> GridOriented b
orientedOnRect (GridOriented dir f) rect = GridOriented dir (f $ extract dir rect)

alignedOnRect :: AxisAligned (a -> b) -> Rectangular a -> Rectangular b
alignedOnRect f rect = degenerateRect_ f <*> rect

fromPair :: Pair a -> AxisAligned a
fromPair (Pair x y) = AxisAligned x y

toPair :: AxisAligned a -> Pair a
toPair (AxisAligned x y) = Pair x y

intersect :: (Num a, Fractional a, Ord a) => Pair (Pair a) -> Rectangular a -> Maybe (GridOriented (Pair a))
intersect p rect = fmap intersectionPair $ collapse (>) intersections
  where segment = splitSegment p
        partial (Pair x x') t = (t - x) / (x' - x)
        partials = partial <$> degenerateRect_ segment <*> rect
        isIntersection t = if t > 0 && t <= 1 then Just t
                           else Nothing
        intersections = fmap isIntersection partials
        intersectionPair tt@(GridOriented dir t) =
          GridOriented dir point
          where bound = toAxis $ extractOriented dir rect
                useT (Pair x x') = x + (t * (x' - x))
                point = toPair (injectAxis (fmap const bound) useT <*> segment)

sequenceRect :: Monad m => Rectangular (m a) -> m ()
sequenceRect (Rectangular a b c d) = a >> b >> c >> d >> return ()

generateRect :: (GridDirection -> a) -> Rectangular a
generateRect f = Rectangular (f GridRight) (f GridDown) (f GridLeft) (f GridUp)

rectToAxisAligned :: Rectangular a -> AxisAligned (Pair a)
rectToAxisAligned (Rectangular a b c d) = AxisAligned (Pair a c) (Pair b d)

toSignedRect :: a -> a -> Rectangular a
toSignedRect plus minus = Rectangular plus plus minus minus

toSignedPairs :: Rectangular a -> Rectangular (Pair a)
toSignedPairs rect = Pair <$> rect <*> (rotate . rotate) rect

