{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Contact.Simple where

import Control.Lens
import Control.Monad
import Data.Either
import Data.Maybe
import Linear.Affine
import Linear.Epsilon
import Linear.V
import Linear.V2
import Linear.Vector
import Linear.Matrix
import Physics.Constraint.Simple
import Physics.Solver.Constraint
import Physics.Contact.Simple.ConvexHull
import Physics.Linear.Simple
import Physics.Contact.Simple.OptimizedSAT
import Physics.Contact.Simple.SAT
import Physics.Transform.Simple
import Utils.Utils

data ContactBehavior a = ContactBehavior { contactBaumgarte :: !a
                                         , contactPenetrationSlop :: !a } deriving Show

data Contact' a = Contact' { _contactEdge' :: !(Neighborhood a)
                           , _contactPenetrator' :: !(Neighborhood a)
                           , _contactDepth' :: !a
                           } deriving Show
makeLenses ''Contact'

contactDepth :: (Floating a)
             => Neighborhood a
             -> Neighborhood a
             -> a
contactDepth edge = contactDepth_ edge . _neighborhoodCenter

contactDepth_ :: (Floating a) => Neighborhood a -> P2 a -> a
contactDepth_ neighborhood p = f v - f p
  where f = afdot' n
        n = _neighborhoodUnitNormal neighborhood
        v = _neighborhoodCenter neighborhood

contactIndex :: Contact' a -> (Int, Int)
contactIndex Contact'{..} =
  (_neighborhoodIndex _contactEdge', _neighborhoodIndex _contactPenetrator')

class (Physical a p) => Contactable a p where
  contactMu :: p -> a
  contactHull :: p -> ConvexHull a

defaultContactBehavior :: (Num a) => ContactBehavior a
defaultContactBehavior = ContactBehavior { contactBaumgarte = 0
                                         , contactPenetrationSlop = 0 }

unwrapContactResult :: Maybe (Flipping (Either (Neighborhood a) (Contact a)))
                    -> Maybe (Flipping (Contact a))
unwrapContactResult contactInfo = (flipInjectF . fmap eitherToMaybe) =<< contactInfo

unwrapOptContactResult :: forall a . Maybe (Flipping (Either (Neighborhood a) (Contact a), SATCache))
                       -> (Maybe (Flipping (Contact a)), Maybe (Flipping SATCache))
unwrapOptContactResult optContactInfo =
  (unwrapContactResult contactInfo, cache)
  where contactInfo = (fmap . fmap) fst optContactInfo
        cache = (fmap . fmap) snd optContactInfo

flattenContactResult :: forall a . (Floating a)
                     => Maybe (Flipping (Contact a))
                     -> [Flipping (Contact' a)]
flattenContactResult =
  (flipInjectF . fmap flatten) <=< maybeToList
  where flatten :: Contact a -> [Contact' a]
        flatten Contact{..} = g <$> flattenContactPoints _contactPenetrator
          where g :: Neighborhood a -> Contact' a
                g pen = Contact' { _contactEdge' = _contactEdge
                                 , _contactPenetrator' = pen
                                 , _contactDepth' = contactDepth _contactEdge pen
                                 }

generateContacts' :: (Epsilon a, Floating a, Ord a, Contactable a p)
                 => (p, p)
                 -> Maybe (Flipping (Contact a))
generateContacts' contactPair = unwrapContactResult $ uncurry contact shapes
  where shapes = pairMap contactHull contactPair

generateContacts :: (Epsilon a, Floating a, Ord a, Contactable a p)
                 => (p, p)
                 -> [Flipping (Contact' a)]
generateContacts = flattenContactResult . generateContacts'

optGenerateContacts' :: (Epsilon a, Floating a, Ord a, Contactable a p)
                     => Maybe (Flipping SATCache)
                     -> (p, p)
                     -> (Maybe (Flipping (Contact a)), Maybe (Flipping SATCache))
optGenerateContacts' Nothing contactPair =
  unwrapOptContactResult $ uncurry cachingContact shapes
  where shapes = pairMap contactHull contactPair
optGenerateContacts' (Just cache) contactPair =
  unwrapOptContactResult $ uncurry optContact shapes cache
  where shapes = pairMap contactHull contactPair

optGenerateContacts :: (Epsilon a, Floating a, Ord a, Contactable a p)
                    => Maybe (Flipping SATCache)
                    -> (p, p)
                    -> ([Flipping (Contact' a)], Maybe (Flipping SATCache))
optGenerateContacts cache =
  (_1 %~ flattenContactResult) . optGenerateContacts' cache
