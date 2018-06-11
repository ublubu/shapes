{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Finding and describing the contact between two colliding objects.
Also, a type for configuring contact constraint solver behavior.
-}
module Physics.Contact.HullVsHull where

import           Control.Lens
import           Data.Vector.Unboxed.Deriving

import           Physics.Contact.ConvexHull
import           Physics.Contact.SAT
import           Physics.Contact.Types
import           Physics.Linear
import           Utils.Descending
import           Utils.Utils

contactDepth :: Neighborhood -- ^ Penetrated edge
             -> Neighborhood -- ^ Penetrating feature
             -> Double -- ^ Penetration depth
contactDepth edge = contactDepth_ edge . _neighborhoodCenter

contactDepth_ :: Neighborhood -- ^ Penetrated edge
              -> P2 -- ^ Penetrating feature
              -> Double -- ^ Penetration depth
contactDepth_ neighborhood p = f v - f p
  where f = afdot' n
        n = _neighborhoodUnitNormal neighborhood
        v = _neighborhoodCenter neighborhood

defaultContactBehavior :: ContactBehavior
defaultContactBehavior =
  ContactBehavior { contactBaumgarte = 0
                  , contactPenetrationSlop = 0
                  }

-- | Extract the 'Contact' if it exists.
unwrapContactResult :: Maybe (Flipping (Either Neighborhood Contact))
                    -- ^ May contain either a separating axis or a 'Contact'
                    -> Maybe (Flipping Contact)
unwrapContactResult contactInfo = (flipInjectF . fmap eitherToMaybe) =<< contactInfo

-- TODO: better names for Contact vs Contact'
-- | Flatten a 'Contact' into 'Contact''s.
flattenContactResult :: Maybe (Flipping Contact)
                     -> Descending ((Int, Int), Flipping Contact')
                     -- ^ in decreasing key order, where x is MSV and y is LSV in (x, y)
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

-- Find the 'Contact' between a pair of shapes if they overlap.
generateContacts' :: (ConvexHull, ConvexHull)
                  -> Maybe (Flipping Contact)
generateContacts' shapes = unwrapContactResult $ uncurry contact shapes

-- Find the 'Contact''s between a pair of shapes if they overlap.
generateContacts :: (ConvexHull, ConvexHull)
                 -> Descending ((Int, Int), Flipping Contact')
                 -- ^ in decreasing key order, where x is MSV and y is LSV in (x, y)
                 --   x is the first hull's feature, y is the second hull's feature
generateContacts = flattenContactResult . generateContacts'
