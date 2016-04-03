{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Contact.Opt where

import Control.Lens
import Data.Vector.Unboxed.Deriving

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
  Contact' { _contactEdgeNormal' :: !V2
           , _contactPenetrator' :: !P2
           , _contactDepth' :: !Double
           } deriving Show

makeLenses ''Contact'
derivingUnbox "Contact'"
  [t| Contact' -> (V2, P2, Double) |]
  [| \Contact'{..} -> (_contactEdgeNormal', _contactPenetrator', _contactDepth') |]
  [| \(n, p, d) -> Contact' n p d |]

contactDepth :: Neighborhood
             -> Neighborhood
             -> Double
contactDepth edge = contactDepth_ edge . _neighborhoodCenter

contactDepth_ :: Neighborhood -> P2 -> Double
contactDepth_ neighborhood p = f v - f p
  where f = afdot' n
        n = _neighborhoodUnitNormal neighborhood
        v = _neighborhoodCenter neighborhood

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
                     -> Descending ((Int, Int), Flipping Contact')
flattenContactResult Nothing = Descending []
flattenContactResult (Just fContact) =
  fmap f . flipInjectF . fmap flatten $ fContact
  where flatten :: Contact -> Descending ((Int, Int), Contact')
        flatten Contact{..} = g <$> flattenContactPoints _contactPenetrator
          where g :: Neighborhood -> ((Int, Int), Contact')
                g pen =
                  ( (_neighborhoodIndex _contactEdge, _neighborhoodIndex pen)
                  , Contact' { _contactEdgeNormal' = _neighborhoodUnitNormal _contactEdge
                             , _contactPenetrator' = _neighborhoodCenter pen
                             , _contactDepth' = contactDepth _contactEdge pen
                             }
                  )
        f :: Flipping ((Int, Int), Contact') -> ((Int, Int), Flipping Contact')
        f x = (flipExtractPair fst x, snd <$> x)

generateContacts' :: (Contactable p)
                 => (p, p)
                 -> Maybe (Flipping Contact)
generateContacts' contactPair = unwrapContactResult $ uncurry contact shapes
  where shapes = pairMap contactHull contactPair

generateContacts :: (Contactable p)
                 => (p, p)
                 -> Descending ((Int, Int), Flipping Contact')
generateContacts = flattenContactResult . generateContacts'
