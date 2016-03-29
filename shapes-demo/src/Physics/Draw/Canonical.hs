{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Physics.Draw.Canonical where

import Data.Either.Combinators
import Linear.Affine (Point(..))
import Linear.V2
import Linear.Matrix
import Physics.Linear.Simple
import Utils.Utils

type V2' = V2 Double
type P2' = P2 Double
type ContactPoints = Either P2' (P2', P2')
data Contact =
  Contact { _contactPoints :: ContactPoints
          , _contactNormal :: V2'
          } deriving (Show)

data Overlap =
  Overlap { _overlapEdge :: (P2', P2')
          , _overlapVector :: V2'
          , _overlapPenetrator :: P2'
          } deriving (Show)

type Polygon = [P2']

newtype Aabb = Aabb (V2 (Double, Double))

class Transformable t where
  transform :: M33 Double -> t -> t

instance Transformable V2' where
  transform = afmul

instance Transformable P2' where
  transform = afmul

instance (Transformable a) => Transformable (a, a) where
  transform = pairMap . transform

instance (Transformable a, Transformable b) => Transformable (Either a b) where
  transform t = mapBoth (transform t) (transform t)

instance Transformable Contact where
  transform t (Contact x y) =
    Contact (transform t x) (transform t y)

instance Transformable Overlap where
  transform t (Overlap x y z) =
    Overlap (transform t x) (transform t y) (transform t z)

instance (Transformable a) => Transformable [a] where
  transform = fmap . transform

instance Transformable Aabb where
  transform t (Aabb box) = Aabb $ (,) <$> minBox <*> maxBox
    where (P minBox) = transform t . P $ fmap fst box
          (P maxBox) = transform t . P $ fmap snd box

class IsCanonical a where
  dummyIsCanonical :: a
  dummyIsCanonical = undefined

instance IsCanonical V2'
instance IsCanonical P2'
instance IsCanonical ContactPoints
instance IsCanonical Contact
instance IsCanonical Overlap
instance IsCanonical Polygon
instance IsCanonical Aabb

class (IsCanonical (Canonical a)) => ToCanonical a where
  type Canonical a
  toCanonical :: a -> Canonical a

transCanon :: (ToCanonical a, Transformable (Canonical a))
           => M33 Double
           -> a
           -> Canonical a
transCanon t = transform t . toCanonical
