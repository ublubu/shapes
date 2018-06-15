{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Finding and describing the contact between two colliding objects.
Also, a type for configuring contact constraint solver behavior.
-}
module Physics.Contact.HullVsHull where

import           Control.Lens
import           Data.Vector.Unboxed.Deriving

import           Physics.Contact.ConvexHull
import           Physics.Contact.SAT          (_contactEdge, _contactPenetrator)
import qualified Physics.Contact.SAT          as S
import           Physics.Contact.Types        (Contact (..),
                                               ContactBehavior (..))
import           Physics.Linear
import           Utils.Descending
import           Utils.Utils

contactDepth :: Neighborhood -- ^ Penetrated edge
             -> Neighborhood -- ^ Penetrating feature
             -> Double -- ^ Penetration depth
contactDepth edge = contactDepth_ edge . _neighborhoodCenter
{-# INLINE contactDepth #-}

contactDepth_ :: Neighborhood -- ^ Penetrated edge
              -> P2 -- ^ Penetrating feature
              -> Double -- ^ Penetration depth
contactDepth_ neighborhood p = f v - f p
  where f = afdot' n
        n = _neighborhoodUnitNormal neighborhood
        v = _neighborhoodCenter neighborhood
{-# INLINE contactDepth_ #-}

defaultContactBehavior :: ContactBehavior
defaultContactBehavior =
  ContactBehavior { contactBaumgarte = 0
                  , contactPenetrationSlop = 0
                  }
{-# INLINE defaultContactBehavior #-}

-- | Extract the 'Contact' if it exists.
unwrapContactResult :: Maybe (Flipping (Either Neighborhood S.Contact))
                    -- ^ May contain either a separating axis or a 'Contact'
                    -> Maybe (Flipping S.Contact)
unwrapContactResult contactInfo = (flipInjectF . fmap eitherToMaybe) =<< contactInfo
{-# INLINE unwrapContactResult #-}

-- | Convert between 'Contact' types.
flattenContactResult :: Maybe (Flipping S.Contact)
                     -> Descending ((Int, Int), Flipping Contact)
                     -- ^ in decreasing key order, where x is MSV and y is LSV in (x, y)
flattenContactResult Nothing = Descending []
flattenContactResult (Just fContact) =
  fmap f . flipInjectF . fmap flatten $ fContact
  where
    flatten :: S.Contact -> Descending ((Int, Int), Contact)
    flatten S.Contact {..} = g <$> S.flattenContactPoints _contactPenetrator
      where
        g :: Neighborhood -> ((Int, Int), Contact)
        g pen =
          ( (_neighborhoodIndex _contactEdge, _neighborhoodIndex pen)
          , Contact
            { _contactNormal = _neighborhoodUnitNormal _contactEdge
            , _contactCenter = _neighborhoodCenter pen
            , _contactDepth = contactDepth _contactEdge pen
            })
    {-# INLINE flatten #-}
    f :: Flipping ((Int, Int), Contact) -> ((Int, Int), Flipping Contact)
    f x = (flipExtractPair fst x, snd <$> x)
    {-# INLINE f #-}
{-# INLINE flattenContactResult #-}

-- Find the 'Contact' between a pair of shapes if they overlap.
generateContacts' :: (ConvexHull, ConvexHull)
                  -> Maybe (Flipping S.Contact)
generateContacts' shapes = unwrapContactResult $ uncurry S.contact shapes
{-# INLINE generateContacts' #-}

-- Find the 'Contact's between a pair of shapes if they overlap.
generateContacts :: (ConvexHull, ConvexHull)
                 -> Descending ((Int, Int), Flipping Contact)
                 -- ^ in decreasing key order, where x is MSV and y is LSV in (x, y)
                 --   x is the first hull's feature, y is the second hull's feature
generateContacts = flattenContactResult . generateContacts'
{-# INLINE generateContacts #-}
