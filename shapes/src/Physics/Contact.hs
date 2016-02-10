{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Contact where

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
import Physics.SAT
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.ConvexHull
import Physics.Linear
import Physics.Transform
import Utils.Utils

data ContactBehavior a = ContactBehavior { contactBaumgarte :: !a
                                         , contactPenetrationSlop :: !a } deriving Show

data Contact' a = Contact' { _contactEdge' :: !(Neighborhood a)
                           , _contactPenetrator' :: !(Neighborhood a)
                           , _contactDepth' :: !a
                           } deriving Show
makeLenses ''Contact'

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
unwrapContactResult contactInfo = (g . f) =<< contactInfo
  where f :: Flipping (Either (Neighborhood a) (Contact a)) -> Flipping (Maybe (Contact a))
        f = fmap eitherToMaybe
        g :: Flipping (Maybe (Contact a)) -> Maybe (Flipping (Contact a))
        g = flipInjectF

flattenContactResult :: forall a . Maybe (Flipping (Contact a)) -> [Flipping (Contact' a)]
flattenContactResult =
  (flipInjectF . fmap flatten) <=< maybeToList
  where flatten :: Contact a -> [Contact' a]
        flatten Contact{..} = fmap g $ flattenContactPoints _contactPenetrator
          where g :: Neighborhood a -> Contact' a
                g pen = Contact' { _contactEdge' = _contactEdge
                                 , _contactPenetrator' = pen
                                 , _contactDepth' = _contactDepth
                                 }

generateContacts' :: (Epsilon a, Floating a, Ord a, Contactable a p)
                 => (p, p)
                 -> Maybe (Flipping (Contact a))
generateContacts' contactPair = unwrapContactResult $ uncurry contact shapes
  where shapes = pairMap contactHull contactPair
        contactPair' = contactPair ^. physPair

generateContacts :: (Epsilon a, Floating a, Ord a, Contactable a p)
                 => (p, p)
                 -> [Flipping (Contact' a)]
generateContacts = flattenContactResult . generateContacts'
