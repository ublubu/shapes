{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE TypeFamilies   #-}

{- |
Another piece of the sample implementation of a physics engine that uses this library.
-}
module Physics.Engine (module Physics.Engine, ContactBehavior(..), makeWorldObj) where

import           GHC.Types                  (Double (D#))

import           Control.Monad.Primitive
import           Physics.Constraint         (PhysicalObj (..), toInvMass2)
import           Physics.Contact            (Shape (..))
import           Physics.Contact.Circle     (circleWithRadius)
import           Physics.Contact.ConvexHull (ConvexHull, listToHull,
                                             rectangleHull)
import           Physics.Contact.Types      (ContactBehavior (..))
import           Physics.Linear             (P2 (..), V2 (..))
import           Physics.World              (External, World, WorldObj,
                                             fromList, makeWorldObj)
import           Physics.World.External     (constantAccel)

pairToV2 :: (Double, Double) -> V2
pairToV2 (D# x, D# y) = V2 x y

-- | Create a @PhysicalObj@.
makePhysicalObj ::
     (Double, Double) -- ^ Velocity
  -> Double -- ^ Rotational velocity
  -> (Double, Double) -- ^ Position
  -> Double -- ^ Rotation
  -> (Double, Double) -- ^ Linear mass paired with rotational mass
  -> PhysicalObj
makePhysicalObj vel rotvel pos rotpos =
  PhysicalObj (pairToV2 vel) rotvel (pairToV2 pos) rotpos . toInvMass2

makeWorld :: (PrimMonad m) => [WorldObj label] -> m (World (PrimState m) label)
makeWorld = fromList

makeConstantAccel :: (Double, Double) -> External
makeConstantAccel = constantAccel . pairToV2

makeHull :: [(Double, Double)] -> Shape
makeHull = HullShape . listToHull . fmap (P2 . pairToV2)

makeRectangleHull :: Double -> Double -> Shape
makeRectangleHull (D# w) (D# h) = HullShape $ rectangleHull w h

makeCircle :: Double -> Shape
makeCircle = CircleShape . circleWithRadius
