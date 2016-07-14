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
{-# INLINE contactDepth #-}

contactDepth_ :: Neighborhood -> P2 -> Double
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

unwrapContactResult :: Maybe (Flipping (Either Neighborhood Contact))
                    -> Maybe (Flipping Contact)
unwrapContactResult contactInfo = (flipInjectF . fmap eitherToMaybe) =<< contactInfo
{-# INLINE unwrapContactResult #-}

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
        {-# INLINE flatten #-}
        f :: Flipping ((Int, Int), Contact') -> ((Int, Int), Flipping Contact')
        f x = (flipExtractPair fst x, snd <$> x)
        {-# INLINE f #-}
{-# INLINE flattenContactResult #-}

generateContacts' :: (ConvexHull, ConvexHull)
                  -> Maybe (Flipping Contact)
generateContacts' shapes = unwrapContactResult $ uncurry contact shapes
{-# INLINE generateContacts' #-}

generateContacts :: (ConvexHull, ConvexHull)
                 -> Descending ((Int, Int), Flipping Contact')
generateContacts = flattenContactResult . generateContacts'
{-# INLINE generateContacts #-}
