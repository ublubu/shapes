{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Contact.Opt where

import Control.Lens
import Control.Monad
import Data.Maybe
import Physics.Constraint.Opt
import Physics.Linear.Opt
import Physics.Contact.Opt.ConvexHull
import Physics.Contact.Opt.SAT
import Utils.Utils

data ContactBehavior =
  ContactBehavior { contactBaumgarte :: !Double
                  , contactPenetrationSlop :: !Double
                  } deriving Show

data Contact' =
  Contact' { _contactEdge' :: !Neighborhood
           , _contactPenetrator' :: !Neighborhood
           , _contactDepth' :: !Double
           } deriving Show
makeLenses ''Contact'

contactDepth :: Neighborhood
             -> Neighborhood
             -> Double
contactDepth edge = contactDepth_ edge . _neighborhoodCenter

contactDepth_ :: Neighborhood -> P2 -> Double
contactDepth_ neighborhood p = f v - f p
  where f = afdot' n
        n = _neighborhoodUnitNormal neighborhood
        v = _neighborhoodCenter neighborhood

contactIndex :: Contact' -> (Int, Int)
contactIndex Contact'{..} =
  (_neighborhoodIndex _contactEdge', _neighborhoodIndex _contactPenetrator')

class (Physical p) => Contactable p where
  contactMu :: p -> Double
  contactHull :: p -> ConvexHull

defaultContactBehavior :: ContactBehavior
defaultContactBehavior =
  ContactBehavior { contactBaumgarte = 0
                  , contactPenetrationSlop = 0
                  }

unwrapContactResult :: Maybe (Flipping (Either Neighborhood Contact))
                    -> Maybe (Flipping Contact)
unwrapContactResult contactInfo = (flipInjectF . fmap eitherToMaybe) =<< contactInfo

flattenContactResult :: Maybe (Flipping Contact)
                     -> [Flipping Contact']
flattenContactResult =
  (flipInjectF . fmap flatten) <=< maybeToList
  where flatten :: Contact -> [Contact']
        flatten Contact{..} = g <$> flattenContactPoints _contactPenetrator
          where g :: Neighborhood -> Contact'
                g pen = Contact' { _contactEdge' = _contactEdge
                                 , _contactPenetrator' = pen
                                 , _contactDepth' = contactDepth _contactEdge pen
                                 }

generateContacts' :: (Contactable p)
                 => (p, p)
                 -> Maybe (Flipping Contact)
generateContacts' contactPair = unwrapContactResult $ uncurry contact shapes
  where shapes = pairMap contactHull contactPair

generateContacts :: (Contactable p)
                 => (p, p)
                 -> [Flipping Contact']
generateContacts = flattenContactResult . generateContacts'
