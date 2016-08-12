{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Contact where

import Control.Lens
import Data.Vector.Unboxed.Deriving

import Physics.Linear
import Physics.Contact.ConvexHull
import Physics.Contact.SAT
import Utils.Descending
import Utils.Utils

-- | Configuring contact constraint behavior
data ContactBehavior =
  ContactBehavior { contactBaumgarte :: !Double
                  -- ^ Bias factor: 0 <= B <= 1, used to feed positional error back into a constraint
                  , contactPenetrationSlop :: !Double
                  -- ^ Amount objects must overlap before they are considered \"touching\"
                  } deriving Show

-- | A contact between two objects - the source of a single set of contact constraints
data Contact' =
  Contact' { _contactEdgeNormal' :: !V2 -- ^ Normal of penetrated edge
           , _contactPenetrator' :: !P2 -- ^ Coordinates of penetrating feature
           , _contactDepth' :: !Double -- ^ Depth of penetration
           } deriving Show

makeLenses ''Contact'
derivingUnbox "Contact'"
  [t| Contact' -> (V2, P2, Double) |]
  [| \Contact'{..} -> (_contactEdgeNormal', _contactPenetrator', _contactDepth') |]
  [| \(n, p, d) -> Contact' n p d |]

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
unwrapContactResult :: Maybe (Flipping (Either Neighborhood Contact))
                    -- ^ May contain either a separating axis or a 'Contact'
                    -> Maybe (Flipping Contact)
unwrapContactResult contactInfo = (flipInjectF . fmap eitherToMaybe) =<< contactInfo
{-# INLINE unwrapContactResult #-}

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
        {-# INLINE flatten #-}
        f :: Flipping ((Int, Int), Contact') -> ((Int, Int), Flipping Contact')
        f x = (flipExtractPair fst x, snd <$> x)
        {-# INLINE f #-}
{-# INLINE flattenContactResult #-}

-- Find the 'Contact' between a pair of shapes if they overlap.
generateContacts' :: (ConvexHull, ConvexHull)
                  -> Maybe (Flipping Contact)
generateContacts' shapes = unwrapContactResult $ uncurry contact shapes
{-# INLINE generateContacts' #-}

-- Find the 'Contact''s between a pair of shapes if they overlap.
generateContacts :: (ConvexHull, ConvexHull)
                 -> Descending ((Int, Int), Flipping Contact')
                 -- ^ in decreasing key order, where x is MSV and y is LSV in (x, y)
generateContacts = flattenContactResult . generateContacts'
{-# INLINE generateContacts #-}
